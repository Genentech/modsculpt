# sculpture metrics --------


#' Various metrics related to model sculpting
#'
#' @name var_imp
#'
#' @param object `sculpture`
#' @param newdata (Optional) Data to calculate the importance from.
#' If omitted, the data that were provided to build the sculpture are used.
#'
#' @return `data.table` with direct requested metrics.
#'
#' @examples
#' df <- mtcars
#' df$vs <- as.factor(df$vs)
#' model <- rpart::rpart(
#'   hp ~ mpg + carb + vs,
#'   data = df,
#'   control = rpart::rpart.control(minsplit = 10)
#' )
#' model_predict <- function(x) predict(model, newdata = x)
#' covariates <- c("mpg", "carb", "vs")
#' pm <- sample_marginals(df[covariates], n = 50, seed = 5)
#'
#' rs <- sculpt_rough(
#'   dat = pm,
#'   model_predict_fun = model_predict,
#'   n_ice = 10,
#'   seed = 1,
#'   verbose = 0
#' )
#'
#' # show direct variable importance
#' calc_dir_var_imp(rs)
#'
#' # show cumulative approximation R^2
#' calc_cumul_R2(rs)
NULL


calc_dir_var_imp_pdp <- function(dt) {
  stopifnot(
    all(c("rn", "feature", "pdp_c") %in% colnames(dt)),
    !"total" %in% tolower(unique(dt$feature)),
    nrow(dt) == nrow(unique(dt[, .(rn, feature)]))
  )

  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  . <- rn <- feature <- pdp_c <- ratio <- variance <- variance_total <-
    NULL # due to NSE notes in R CMD check

  # calculate total variance of PDPs
  var_total <- dt[, .(pdp_c = sum(pdp_c)), .(rn)][, var(pdp_c)]

  # calculate variance per feature
  dat_var <- dt[
    ,
    .(variance = var(pdp_c), variance_total = var_total),
    .(feature)
  ][
    ,
    ratio := variance / variance_total
  ][
    order(ratio, decreasing = TRUE)
  ]

  # define as factor to keep the order
  dat_var[, feature := factor(feature, levels = feature)]
  return(dat_var)
}

#' @describeIn var_imp Direct variable importance
#' @export
calc_dir_var_imp <- function(object, newdata = NULL) {
  checkmate::assert_class(object, "sculpture")
  if (is.null(newdata)) {
    return(attr(object, "var_imp"))
  }
  checkmate::assert_data_frame(newdata, any.missing = FALSE)
  calc_dir_var_imp_pdp(
    eval_sculpture(
      sculpture = object,
      data = newdata
    )$pdp
  )
}

calc_cumul_R2_pdp <- function(dt, feat_order, model_predictions, model_offset) {
  stopifnot(
    is.data.table(dt),
    all(c("rn", "feature", "pdp_c") %in% colnames(dt)),
    !"total" %in% tolower(unique(dt$feature)),
    nrow(dt) == nrow(unique(dt[, .(rn, feature)])),
    length(model_predictions) == length(unique(dt$rn)),
    is.character(feat_order),
    is.numeric(model_offset)
  )

  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  . <- rn <- feature <- pdp_c <- preds <- NULL # due to NSE notes in R CMD check

  # prepare ordered features
  cumul_features <- lapply(seq_along(feat_order), function(i) feat_order[1:i])

  # calculate R2
  R2_cumul <- vapply(
    cumul_features,
    function(cols) {
      predictions <- dt[
        feature %in% cols,
        .(preds = sum(pdp_c) + model_offset),
        .(rn)
      ][
        order(rn), preds
      ]
      metrics_R2(score_fun = "score_quadratic", y = model_predictions, y_hat = predictions)
    },
    numeric(1)
  )
  return(
    data.table(feature = factor(feat_order, levels = feat_order), R2 = R2_cumul)
  )
}


#' @describeIn var_imp Calculate cumulative approximation of R^2
#' @export
calc_cumul_R2 <- function(object, newdata = NULL) {
  checkmate::assert_class(object, "sculpture")
  if (is.null(newdata)) {
    return(attr(object, "cumul_R2"))
  }
  checkmate::assert_data_frame(newdata, any.missing = FALSE)

  eg <- eval_sculpture(
    sculpture = object,
    data = newdata
  )

  dat_var <- calc_dir_var_imp_pdp(dt = eg$pdp)

  calc_cumul_R2_pdp(
    dt = eg$pdp,
    feat_order = levels(dat_var$feature),
    model_predictions = eg$prediction$pred,
    model_offset = eg$offset
  )
}


# calculate range - for plots (facet sorting)
calc_range_pdp <- function(dt) {
  stopifnot(
    all(c("rn", "feature", "pdp_c") %in% colnames(dt)),
    nrow(dt) == nrow(unique(dt[, .(rn, feature)]))
  )

  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  . <- rn <- feature <- pdp_c <- NULL # due to NSE notes in R CMD check

  dt_range <- dt[
    , .(range = max(pdp_c) - min(pdp_c)),
    .(feature)
  ][
    order(-range)
  ]
  dt_range[, feature := factor(feature, levels = feature)][]

  return(dt_range)
}



# generic metrics --------


#' Various metrics for measuring model performance.
#'
#' @name metrics
#' @param score_fun A scoring function: `score_quadratic`, `score_log_loss`,
#' or a user-defined scoring rule. See below for more details.
#' @param y Vector of observations.
#' @param y_hat Vector of predictions.
#' @param y_hat_calib Vector of calibrated predictions. See below for more details.
#' @param na_rm Logical, defaults to `FALSE`. Should NAs be removed?
#'
#' @section Scoring function:
#' One can use predefined scores like `score_quadratic` or `score_log_loss`.
#' If those do not fit the needs, a user-defined scoring function can also be used.
#' This function needs to take exactly 3 arguments: `y` (truth values),
#' `y_hat` (estimated values), and `na_rm` (should NAs be removed?):
#' - both `y` and `y_hat` are numeric (not factors!)
#' - `na_rm` is a scalar logical
#'
#' It needs to return a number.
#' There is a utility function `check_score_fun` to check if the user-defined function is
#' programmed correctly.
#' It checks the input and the output, but not if the actual returned value makes sense.
#'
#'
#' @section Calibration:
#' To obtain calibrated predictions,
#' fit a calibration model and predict based on that model.
#' Users can use their own calibration model or make use of `metrics_fit_calib`,
#' which fits an `mgcv::gam()` model with smoother `mgcv::s(., k = -1)` (automatic knot selection).
#' If the input `y` is a factor, then a binomial family is used, otherwise a gaussian.
#' NAs are always dropped.
#'
#' Continuous response example:
#' ```
#' calibration_model <- metrics_fit_calib(
#'   y = truth,
#'   y_hat = prediction
#' )
#' calib_pred <- predict(calibration_model)
#' ```
#'
#' Binary response example:
#' ```
#' calibration_model <- metrics_fit_calib(
#'   y = factor(truth, levels = c("0", "1")),
#'   y_hat = prediction
#' )
#' calib_pred <- predict(calibration_model, type = "response")
#' ```
#' In the binary case, make sure that:
#' - `y` is a factor with correct level setting.
#' Usually "0" is the reference (first) level and "1" is the event (second level).
#' This may clash with `yardstick` setting where
#' the first level is by default the "event" level.
#' - `y_hat` are probabilities (not a log of odds).
#' - returned calibrated predictions `calib_pred` are also probabilities by setting
#' `type = "response"`.
#'
#'
#' @return `metrics_fit_calib` returns an [mgcv::gam()] model fit, otherwise a number.
#'
#' @examples
#' # Scores
#' score_quadratic(y = c(1.34, 2.8), y_hat = c(1.34, 2.8)) # must be 0
#' score_quadratic(y = 0.5, 0) # must be 0.5**2 = 0.25
#'
#' score_log_loss(y = c(0, 1), y_hat = c(0.01, 0.9)) # must be close to 0
#' score_log_loss(y = 0, y_hat = 0) # undefined
#'
#' check_score_fun(score_quadratic) # passes without errors
#'
#' # Metrics based on `lm` model
#' mod <- lm(hp ~ ., data = mtcars)
#' truth <- mtcars$hp
#' pred <- predict(mod)
#'
#' # calibration fit and calibrated predictions
#' calib_mod <- metrics_fit_calib(y = truth, y_hat = pred)
#' calib_pred <- predict(calib_mod)
#'
#' metrics_unc(score_fun = "score_quadratic", y = truth)
#' metrics_R2(score_fun = "score_quadratic", y = truth, y_hat = pred)
#' metrics_DI(score_fun = "score_quadratic", y = truth, y_hat_calib = calib_pred)
#' metrics_MI(score_fun = "score_quadratic", y = truth, y_hat = pred, y_hat_calib = calib_pred)
#' # Note that R^2 = DI - MI
#' metrics_r2(y = truth, y_hat = pred, y_hat_calib = calib_pred)
#'
#' # Metrics based on `glm` model (logistic regression)
#' # Note the correct setting of levels
#' mod <- glm(factor(vs, levels = c("0", "1")) ~ hp + mpg, data = mtcars, family = "binomial")
#' truth_fct <- factor(mtcars$vs, levels = c("0", "1"))
#' truth_num <- mtcars$vs
#' pred <- predict(mod, type = "response") # type = "response" returns probabilities
#'
#' # calibration fit and calibrated predictions
#' calib_mod <- metrics_fit_calib(y = truth_fct, y_hat = pred)
#' calib_pred <- predict(calib_mod, type = "response") # type = "response" returns probabilities
#'
#' metrics_unc(score_fun = "score_quadratic", y = truth_num)
#' metrics_R2(score_fun = "score_quadratic", y = truth_num, y_hat = pred)
#' metrics_DI(score_fun = "score_quadratic", y = truth_num, y_hat_calib = calib_pred)
#' metrics_MI(score_fun = "score_quadratic", y = truth_num, y_hat = pred, y_hat_calib = calib_pred)
#' # Note that R^2 = DI - MI
#' metrics_r2(y = truth_num, y_hat = pred, y_hat_calib = calib_pred)
#'
NULL

remove_missing <- function(...) {
  idx <- complete.cases(...)
  lapply(list(...), \(x) x[idx])
}

#' @describeIn metrics Binary log loss score
#' @export
score_log_loss <- function(y, y_hat, na_rm = FALSE) {
  checkmate::assert_numeric(y)
  checkmate::assert(
    checkmate::check_numeric(y_hat, len = length(y)),
    checkmate::check_numeric(y_hat, len = 1)
  )
  if (na_rm) {
    rm <- remove_missing(y = y, y_hat = y_hat)
    y <- rm[["y"]]
    y_hat <- rm[["y_hat"]]
  }
  -mean(y * log(y_hat) + (1 - y) * log(1 - y_hat))
}

#' @describeIn metrics Quadratic score
#' @export
score_quadratic <- function(y, y_hat, na_rm = FALSE) {
  checkmate::assert_numeric(y)
  checkmate::assert(
    checkmate::check_numeric(y_hat, len = length(y)),
    checkmate::check_numeric(y_hat, len = 1)
  )
  if (na_rm) {
    rm <- remove_missing(y = y, y_hat = y_hat)
    y <- rm[["y"]]
    y_hat <- rm[["y_hat"]]
  }
  mean((y - y_hat)**2)
}

#' @describeIn metrics Utility function for checking the properties of a user-defined `score_fun`.
#' @export
check_score_fun <- function(score_fun) {
  if (is.character(score_fun)) {
    checkmate::assert_function(eval(str2lang(score_fun)), args = c("y", "y_hat", "na_rm"))
  } else if (is.function(score_fun)) {
    checkmate::assert_function(score_fun, args = c("y", "y_hat", "na_rm"))
  } else {
    stop("`score_fun` must be a function.")
  }
  out <- do.call(score_fun, list(y = c(0.5, 0.6), y_hat = c(0.5, 0.55)))
  if (!checkmate::test_number(out, na.ok = TRUE)) {
    stop("The return value of `score_fun` must be a number")
  }
}


#' @describeIn metrics Uncertainty
#' @export
metrics_unc <- function(score_fun, y, na_rm = FALSE) {
  check_score_fun(score_fun)
  if (na_rm) {
    rm <- remove_missing(y = y)
    y <- rm[["y"]]
  }
  do.call(score_fun, list(y = y, y_hat = rep_len(mean(y), length(y))))
}

#' @describeIn metrics R^2 metric
#' @export
metrics_R2 <- function(score_fun, y, y_hat, na_rm = FALSE) {
  check_score_fun(score_fun)
  if (na_rm) {
    rm <- remove_missing(y = y, y_hat = y_hat)
    y <- rm[["y"]]
    y_hat <- rm[["y_hat"]]
  }
  1 -
    do.call(score_fun, list(y = y, y_hat = y_hat)) /
      do.call(score_fun, list(y = y, y_hat = rep_len(mean(y), length(y))))
}

#' @describeIn metrics Fit calibration curve using [mgcv::gam()].
#' Note that NAs are always dropped.
#' @export
metrics_fit_calib <- function(y, y_hat, rev_fct = FALSE) {
  requireNamespace("mgcv")
  s <- mgcv::s
  if (is.factor(y)) {
    fam <- binomial()
    if(rev_fct) y <- factor(y, levels=rev(levels(y)))
  } else {
    fam <- gaussian()
  }
  tryCatch(
    mgcv::gam(y ~ s(y_hat, k = -1), family = fam, na.action = "na.omit"),
    error = \(e) tryCatch(
      mgcv::gam(y ~ s(y_hat, k = 3), family = fam, na.action = "na.omit"),
      error = \(e) mgcv::gam(y ~ y_hat, family = fam, na.action = "na.omit")
    )
  )
}

#' @describeIn metrics Discrimination index
#' @export
metrics_DI <- function(score_fun, y, y_hat_calib, na_rm = FALSE) {
  check_score_fun(score_fun)
  if (na_rm) {
    rm <- remove_missing(y = y, y_hat_calib = y_hat_calib)
    y <- rm[["y"]]
    y_hat_calib <- rm[["y_hat_calib"]]
  }
  (
    do.call(score_fun, list(y = y, y_hat = rep_len(mean(y), length(y)))) -
      do.call(score_fun, list(y = y, y_hat = y_hat_calib))
  ) /
    do.call(score_fun, list(y = y, y_hat = rep_len(mean(y), length(y))))
}

#' @describeIn metrics Miscalibration index
#' @export
metrics_MI <- function(score_fun, y, y_hat, y_hat_calib, na_rm = FALSE) {
  check_score_fun(score_fun)
  if (na_rm) {
    rm <- remove_missing(y = y, y_hat = y_hat, y_hat_calib = y_hat_calib)
    y <- rm[["y"]]
    y_hat <- rm[["y_hat"]]
    y_hat_calib <- rm[["y_hat_calib"]]
  }
  (
    do.call(score_fun, list(y = y, y_hat = y_hat)) -
      do.call(score_fun, list(y = y, y_hat = y_hat_calib))
  ) /
    do.call(score_fun, list(y = y, y_hat = rep_len(mean(y), length(y))))
}


#' @describeIn metrics r^2 metric based on slope of `lm`
#' @export
metrics_r2 <- function(y, y_hat, y_hat_calib, na_rm = FALSE) {
  if (na_rm) {
    rm <- remove_missing(y = y, y_hat = y_hat, y_hat_calib = y_hat_calib)
    y <- rm[["y"]]
    y_hat <- rm[["y_hat"]]
    y_hat_calib <- rm[["y_hat_calib"]]
  } else if (anyNA(y) || anyNA(y_hat) || anyNA(y_hat_calib)) {
    return(NA)
  }
  lm_mod <- lm(y_hat_calib ~ y_hat)
  res <- (coef(lm_mod)[2] * sd(y_hat) / sd(y))**2
  if (is.na(res)) {
    res <- 0
  }
  return(unname(res))
}
