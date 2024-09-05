# product marginals --------

#' Sample product marginals dataset
#'
#' @param dat Data.frame to sample from, must include only covariates.
#' @param n Number of observations to sample.
#' @param seed `NULL` or seed for exact reproducibility.
#'
#' @details The product marginals dataset is a grid of values that is sampled independently
#' per each column (feature) from the original dataset.
#' The aim here is to disentangle the correlations between features and assess
#' how each feature affects the model predictions individually.
#' It will not contain new values per column, but it may contain new combinations of values not
#' seen in the original data.
#' One can also check how the model behaves if there are unseen observations
#' (new combination of features).
#’
#' Note that the use of the product marginal dataset for model sculpting only works
#' if the features are approximately additive for model predictions.
#' In the quite rare case when they are not, the sculpted models using the product marginal
#' dataset is expected to have significantly lower performance and
#' the conclusions may be misleading.
#'
#' One can also try using the original data instead of the product marginals for model
#' sculpting and see how the results differ.
#'
#' @return `data.frame` with same number of columns and `n` rows.
#' @export
#'
#' @examples
#' sample_marginals(mtcars, n = 5, seed = 543)
sample_marginals <- function(dat, n, seed = NULL) {
  checkmate::assert_data_frame(dat, any.missing = FALSE)
  checkmate::assert_integerish(n, lower = 1, any.missing = FALSE, len = 1)

  dat <- as.data.frame(dat)
  cols <- colnames(dat)
  stopifnot(ncol(dat) > 0, nrow(dat) > 0)

  # indexes: random samples of length n, individual per column
  set.seed(seed)
  idx_per_cols <- lapply(
    seq_along(cols),
    function(...) sample.int(nrow(dat), size = n, replace = TRUE)
  )

  # get values for the indexes above
  dat_sub <- lapply(
    seq_along(cols),
    function(i) dat[idx_per_cols[[i]], cols[i], drop = FALSE]
  )
  dat_sub <- do.call("cbind", c(dat_sub, list(row.names = NULL)))

  # if this function is used to generate product marginals or data for ice curves
  return(dat_sub)
}



# ICE data --------

# calculate ICE data by using product marginals and prediction function
calculate_ice_data <- function(sub, predict_fun, x, x_name, col_order) {
  stopifnot(
    is.data.frame(sub) | is.null(sub),
    is.function(predict_fun),
    is.atomic(x),
    is.character(x_name),
    is.character(col_order)
  )

  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  . <- ice <- line_id <- ..x <- ice_centered <- NULL # due to NSE notes in R CMD check

  # special case: sculpting performed on 1 variable
  if (is.null(sub)) {
    preds <- predict_fun(structure(data.frame(x), names = x_name))
    out <- data.table(
      x = x,
      ice = preds,
      ice_centered = preds - mean(preds),
      line_id = 1
    )

    # all other cases
  } else {
    stopifnot(!x_name %in% colnames(sub))
    out <- rbindlist(
      lapply(1:nrow(sub), function(i) cbind(x, sub[i, , drop = FALSE], row.names = NULL))
    )
    setnames(out, "x", x_name)
    out[, ice := predict_fun(as.data.frame(out)[, col_order])]
    out[, line_id := rep(1:nrow(sub), each = length(..x))]
    out[, ice_centered := ice - mean(ice), line_id]
    setnames(out, x_name, "x")
    out <- out[, c("x", "ice", "ice_centered", "line_id")]
  }
  return(out)
}

# generate ICE data from stored ICE predictions
# the result is similar shape as the returned object from calculate_ice_data
generate_ice_data <- function(predictions, x, logodds_to_prob = FALSE) {
  stopifnot(
    is.list(predictions),
    is.atomic(x)
  )

  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  ..x <- line_id <- NULL # due to NSE notes in R CMD check

  out <- rbindlist(
    lapply(
      predictions,
      function(p) {
        data.frame(
          x = x,
          y = `if`(logodds_to_prob, inv.logit(p), p),
          row.names = NULL
        )
      }
    )
  )
  out[, line_id := rep(seq_along(predictions), each = length(..x))]

  return(out)
}


# PDP data -----------

# calculate PDP data from ICE data
calculate_pdp_data <- function(id) {
  stopifnot(is.data.table(id))

  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  . <- x <- ice_centered <- NULL # due to NSE notes in R CMD check

  unique(id)[
    ,
    .(
      pdp_centered = mean(ice_centered),
      pdp_centered_se = sd(ice_centered, na.rm = F) / sqrt(.N)
    ),
    .(x)
  ]
}

# generate PDP data from stored ICE predictions
generate_pdp_data <- function(predictions, x, logodds_to_prob = FALSE) {
  id <- generate_ice_data(predictions = predictions, x = x, logodds_to_prob = logodds_to_prob)
  setnames(id, old = "y", new = "ice_centered")
  pd <- calculate_pdp_data(id)
  setnames(pd, old = "pdp_centered", new = "y")
  setnames(pd, old = "pdp_centered_se", new = "y_se")
  return(pd)
}


# rough sculpture ----------

check_data <- function(dat) {
  checkmate::assert_data_frame(dat, any.missing = FALSE)
  return(as.data.frame(dat)) # remove tbl_df etc
}

check_upf <- function(upf, dat) {
  checkmate::assert_function(upf, nargs = 1)
  upf_output <- upf(dat)
  check_upf_output(dat = dat, output = upf_output)
  return(upf_output)
}

check_upf_output <- function(dat, output) {
  checkmate::assert(
    checkmate::check_numeric(output, finite = TRUE, any.missing = FALSE, len = nrow(dat)),
    checkmate::check_factor(output, any.missing = FALSE, len = nrow(dat))
  )
  return(invisible(NULL))
}


#' Create a rough model
#'
#' @param dat Data to create the rough model from.
#' Must be a product marginal dataset (see `sample_marginals`)
#' with covariates only (i.e. without response).
#' @param model_predict_fun Function that returns predictions given a dataset.
#' @param n_ice Number of ICE curves to generate. Defaults to 10.
#' @param seed (`NULL`) or seed for exact reproducibility.
#' @param verbose (`integer`) 0 for silent run, > 0 for messages.
#' @param allow_par (`logical`) Allow parallel computation? Defaults to `FALSE`.
#' @param model_predict_fun_export For parallel computation only.
#' If there is a parallel backend registered (see `parallel_set()`),
#' then use this to export variables used in `model_predict_fun` (like model).
#' This is passed to `foreach::foreach(..., .export = model_predict_fun_export)`.
#' @param data_as_marginals (`logical`) Use the provided data `dat` as already sampled dataset?
#' Defaults to `FALSE`.
#'
#' @details For parallel computation, use [parallel_set()] and set `allow_par` to `TRUE`.
#' Note that parallel computation may fail if the model is too big and there is not enough memory.
#'
#' @return Object of classes `rough` and `sculpture`.
#' @export
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
#' class(rs)
#' head(predict(rs))
#'
#' # lm model without interaction -> additive -> same predictions
#' model <- lm(hp ~ mpg + carb + vs, data = df)
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
#' class(rs)
#' head(predict(rs))
#' head(predict(model, pm))
#'
sculpt_rough <- function(dat, model_predict_fun, n_ice = 10,
                         seed = NULL, verbose = 0,
                         allow_par = FALSE,
                         model_predict_fun_export = NULL,
                         data_as_marginals = FALSE) {
  dat <- check_data(dat)
  predictions <- check_upf(model_predict_fun, dat)
  checkmate::assert_integerish(n_ice, any.missing = FALSE, len = 1)
  checkmate::assert_integerish(verbose, lower = 0, any.missing = FALSE, len = 1)
  checkmate::assert_flag(allow_par)
  checkmate::assert_flag(data_as_marginals)

  covariates <- colnames(dat)

  `%operand%` <- define_foreach_operand(allow_par = allow_par)
  res <- foreach::foreach(col = covariates, .export = model_predict_fun_export) %operand% {
    # verbosity ...
    matched <- match(col, covariates)
    if (verbose > 0) {
      if (length(covariates) < 10) {
        message(paste("Sculpting variable:", matched, "/", length(covariates)))
      } else {
        if ((matched == 1) | (matched %% 10 == 0)) {
          message(paste("Sculpting variable:", matched, "/", length(covariates)))
        }
      }
    }

    # generate product marginals
    if (data_as_marginals) {
      dat_subs <- dat[covariates[covariates != col]]
      # Sample n_ice rows
      dat_subs <- dat_subs[sample(nrow(dat_subs), n_ice, replace = TRUE), ]

    } else {
      if (length(covariates) > 1) {
        dat_subs <- sample_marginals(
          dat = dat[setdiff(covariates, col)],
          n = n_ice,
          seed = seed
        )
        stopifnot(nrow(dat_subs) == n_ice)
      } else {
        dat_subs <- NULL
      }
    }

    # calculate ice
    ice <- calculate_ice_data(
      sub = dat_subs,
      predict_fun = model_predict_fun,
      x = dat[[col]],
      x_name = col,
      col_order = colnames(dat)
    )

    # calculate pdp
    pdp <- calculate_pdp_data(id = ice)

    # continuous flag
    is_continuous <- is.numeric(dat[[col]])

    # interpolation function - used for making predictions
    af <- y <- x <- NULL # due to NSE notes in R CMD check
    if (is_continuous && nrow(pdp) > 1) {
      e_predict_fun <- new.env(parent = globalenv())
      e_predict_fun$x <- pdp[["x"]]
      e_predict_fun$y <- pdp[["pdp_centered"]]
      e_predict_fun$af <- approxfun(x = e_predict_fun$x, y = e_predict_fun$y, rule = 2)
      predict_fun <- function(v) af(v)
      environment(predict_fun) <- e_predict_fun
    } else {
      e_predict_fun <- new.env(parent = globalenv())
      e_predict_fun$x <- pdp[["x"]]
      e_predict_fun$y <- pdp[["pdp_centered"]]
      predict_fun <- function(v) {
        ind <- match(v, x)
        ifelse(is.na(ind), 0, y[ind])
      }
      environment(predict_fun) <- e_predict_fun
    }

    return(list(
      subsets = dat_subs,
      predict = predict_fun,
      ice_centered = split(ice$ice_centered, ice$line_id),
      ice = split(ice$ice, ice$line_id),
      is_discrete = !is_continuous,
      x = dat[[col]],
      x_name = col
    ))
  }

  names(res) <- covariates
  attr(res, "offset") <- mean(predictions)
  class(res) <- c("rough", "sculpture", class(res))

  # evaluate the sculpture
  es <- eval_sculpture(
    sculpture = res,
    data = as.data.frame(as.data.table(lapply(res, "[[", "x")))
  )

  # calculate variable importance
  dat_var <- calc_dir_var_imp_pdp(es$pdp)
  feat_order <- levels(dat_var$feature)

  # calculate cumulative R2
  dat_R2_cumul <- calc_cumul_R2_pdp(
    dt = es$pdp,
    feat_order = feat_order,
    model_predictions = es$prediction$pred,
    model_offset = es$offset
  )

  # calculate range
  dat_range <- calc_range_pdp(es$pdp)

  attr(res, "var_imp") <- dat_var
  attr(res, "cumul_R2") <- dat_R2_cumul
  attr(res, "range") <- dat_range

  return(res)
}



# detailed sculpture --------

#' Create a detailed model with user defined smoother
#'
#' @param rs Rough model, i.e. object of classes `rough` and `sculpture`.
#' @param smoother_fit Smoother fitting function.
#' @param smoother_predict Smoother prediction function.
#' @param missings (`NULL`) or single value or a named vector.
#' Specifies the value(-s) that stand for the missing values.
#' If `NULL`, then no missing value handling is carried out.
#' If single value, then it is assumed that this value is used for flagging missing values across
#' all continuous variables.
#' If named vector, then the names are used to refer to continuous variables and the values for
#' flagging missing values in that variable.
#' @param verbose (`integer`) 0 for silent run, > 0 for messages.
#' @param allow_par (`logical`) Allow parallel computation? Defaults to `FALSE`.
#'
#' @details For parallel computation, use [parallel_set()] and set `allow_par` to `TRUE`.
#' Note that parallel computation may fail if the model is too big and there is not enough memory.
#'
#' @section Custom smoothers:
#' If none of the predefined smoothers ([sculpt_detailed_gam()], [sculpt_detailed_lm()])
#' suits your needs, you can define your own smoothers.
#' You need to define 2 functions: `smoother_fit` and `smoother_predict`:
#'
#' `smoother_fit` takes 5 arguments ("x", "y", "is_discrete", "column_name", "na_ind") and
#' returns a model fit. "x" are the feature values, "y" are the PDP values,
#' "is_discrete" flags a discrete feature, "column_name" holds the feature name,
#' and "na_ind" passes the NA value from `missings` (or NULL by default).
#'
#' `smoother_predict` takes also 5 arguments ("smoother", "new_x", "is_discrete", "column_name",
#' "na_ind") and returns predictions as a vector. "smoother" is the model fit returned from
#' `smoother_fit`, "new_x" are the feature values that we want to predict, "is_discrete",
#' "column_name", and "na_ind" have the same purpose as in `smoother_fit`.
#' See also Examples.
#'
#' @return Object of classes `detailed` and `sculpture`.
#' @export
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
#' # define custom smoother
#' # - gam with 3 knots for variable "mpg"
#' # - gam with 5 knots for variable "carb"
#' # - lm for any discrete variable
#' library(mgcv)
#' my_smoother <- function(x, y, is_discrete, column_name, na_ind = NULL) {
#'   if (column_name == "mpg") {
#'     gam(y ~ s(x, k = 3))
#'   } else if (column_name == "carb") {
#'     gam(y ~ s(x, k = 5))
#'   } else if (is_discrete) {
#'     lm(y ~ x)
#'   } else {
#'     stop("Undefined smoother")
#'   }
#' }
#'
#' # define appropriate predict function
#' # - predict.gam returns an array, we need to convert it to vector
#' # - if-else branch for illustration purposes
#' my_smoother_predict <- function(smoother, new_x, is_discrete, column_name, na_ind = NULL) {
#'   if (inherits(smoother, "gam")) {
#'     # as.numeric: convert array to vector
#'     as.numeric(predict(smoother, newdata = data.frame(x = new_x)))
#'   } else {
#'     predict(smoother, newdata = data.frame(x = new_x))
#'   }
#' }
#'
#' ds <- sculpt_detailed_generic(
#'   rs = rs,
#'   smoother_fit = my_smoother,
#'   smoother_predict = my_smoother_predict
#' )
#' class(ds)
#' \dontrun{
#' # see components
#' g_component(ds)$continuous
#' }
#'
#'
#' # another example with constrained gam (cgam) package
#' \dontrun{
#' library(cgam)
#'
#' cgam_smoother <- function(x, y, is_discrete, column_name, na_ind = NULL) {
#'   if (column_name == "carb") {
#'     cgam(y ~ s.incr(x, numknots = 3))
#'   } else if (column_name == "mpg") {
#'     cgam(y ~ s.decr(x, numknots = 3))
#'   } else {
#'     cgam(y ~ x)
#'   }
#' }
#'
#' cgam_predict <- function(smoother, new_x, is_discrete, column_name, na_ind = NULL) {
#'   predict(smoother, newData = data.frame(x = new_x))$fit
#' }
#'
#' ds2 <- sculpt_detailed_generic(
#'   rs = rs,
#'   smoother_fit = cgam_smoother,
#'   smoother_predict = cgam_predict
#' )
#'
#' # see components
#' g_component(ds2)$continuous
#' }
sculpt_detailed_generic <- function(rs, smoother_fit, smoother_predict,
                                    missings = NULL, verbose = 0, allow_par = FALSE) {
  checkmate::assert_class(rs, "sculpture")
  checkmate::assert_class(rs, "rough")
  checkmate::assert_function(
    smoother_fit,
    args = c("x", "y", "is_discrete", "column_name", "na_ind")
  )
  checkmate::assert_function(
    smoother_predict,
    args = c("smoother", "new_x", "is_discrete", "column_name", "na_ind")
  )
  checkmate::assert(
    checkmate::check_null(missings),
    checkmate::check_atomic(missings, any.missing = FALSE, len = 1),
    checkmate::check_atomic(missings, any.missing = FALSE, max.len = length(rs), names = "named")
  )
  check_continuous <- vapply(rs, "[[", logical(1), "is_discrete")
  check_continuous <- names(Filter(isFALSE, check_continuous))
  if (length(missings) == 1) {
    missings <- rep(list(missings), length(check_continuous))
    names(missings) <- check_continuous
  } else if (length(missings) != 0) {
    missings <- as.list(missings)
    checkmate::assert_subset(names(missings), check_continuous, .var.name = "missings")
  }
  checkmate::assert_integerish(verbose, lower = 0, any.missing = FALSE, len = 1)
  checkmate::assert_flag(allow_par)

  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  x <- NULL # due to NSE notes in R CMD check

  `%operand%` <- define_foreach_operand(allow_par = allow_par)
  res <- foreach::foreach(col = names(rs)) %operand% {
    # verbosity ...
    matched <- match(col, names(rs))
    if (verbose > 0) {
      if (length(rs) < 10) {
        message(paste("Sculpting variable:", matched, "/", length(rs)))
      } else {
        if ((matched == 1) | (matched %% 10 == 0)) {
          message(paste("Sculpting variable:", matched, "/", length(rs)))
        }
      }
    }

    # build the smoother from PDPs (based on original data, i.e. with duplicates)
    pdp_dupl <- data.table(x = rs[[col]]$x, pdp_centered = rs[[col]]$predict(rs[[col]]$x))
    pdp_dupl <- pdp_dupl[order(x)]

    # memory optimization: use a clean environment for the predict function
    e_predict_fun <- new.env()

    # estimate smoothers
    e_predict_fun$smoother <- smoother_fit(
      x = pdp_dupl$x,
      y = pdp_dupl$pdp_centered,
      is_discrete = rs[[col]]$is_discrete,
      column_name = col,
      na_ind = `if`(!is.null(missings[[col]]), pdp_dupl$x == missings[[col]])
    )

    # add the rest of variables into the function environment
    e_predict_fun$smoother_predict <- smoother_predict
    e_predict_fun$is_discrete <- rs[[col]]$is_discrete
    e_predict_fun$col <- col
    e_predict_fun$missings_flag <- missings[[col]]

    # smoother prediction function
    smoother <- is_discrete <- missings_flag <- NULL # due to NSE notes in R CMD check
    predict_fun <- function(x) {
      smoother_predict(
        smoother = smoother,
        new_x = x,
        is_discrete = is_discrete,
        column_name = col,
        na_ind = `if`(!is.null(missings_flag), x == missings_flag)
      )
    }
    # use the defined environment as the environment of the function
    environment(predict_fun) <- e_predict_fun

    # check the output of the smoother prediction
    pf_check <- predict_fun(pdp_dupl$x)
    if (!is.vector(pf_check) || is.character(pf_check)) {
      stop("The output of the `smoother_predict` needs to be a numeric/factor vector.")
    }

    return(list(
      predict = predict_fun,
      is_discrete = rs[[col]]$is_discrete,
      x = rs[[col]]$x,
      x_name = rs[[col]]$x_name,
      missings_flag = missings[[col]]
    ))
  }

  names(res) <- names(rs)
  attr(res, "offset") <- attr(rs, "offset")
  class(res) <- c("detailed", "sculpture", class(res))

  # evaluate the sculpture
  es <- eval_sculpture(
    sculpture = res,
    data = as.data.frame(as.data.table(lapply(res, "[[", "x")))
  )

  # calculate variable importance
  dat_var <- calc_dir_var_imp_pdp(es$pdp)
  feat_order <- levels(dat_var$feature)

  # calculate cumulative R2
  dat_R2_cumul <- calc_cumul_R2_pdp(
    dt = es$pdp,
    feat_order = feat_order,
    model_predictions = es$prediction$pred,
    model_offset = es$offset
  )

  # calculate range
  dat_range <- calc_range_pdp(es$pdp)

  attr(res, "var_imp") <- dat_var
  attr(res, "cumul_R2") <- dat_R2_cumul
  attr(res, "range") <- dat_range

  return(res)
}


smoother_gam <- function(x, y, is_discrete, column_name, na_ind = NULL) {
  s <- mgcv::s
  if (!is_discrete) {
    tryCatch(
      mgcv::gam(as.formula(paste0("y ~ s(x, k = -1)", `if`(!is.null(na_ind), " + na_ind")))),
      error = function(e) {
        tryCatch(
          mgcv::gam(as.formula(paste0("y ~ s(x, k = 3)", `if`(!is.null(na_ind), " + na_ind")))),
          error = function(e) {
            if (length(x) == 1) {
              lm(as.formula(paste0("y ~ x", `if`(!is.null(na_ind), " + na_ind"))))
            } else {
              mgcv::gam(as.formula(paste0("y ~ x", `if`(!is.null(na_ind), " + na_ind"))))
            }
          }
        )
      }
    )
  } else {
    tryCatch(
      mgcv::gam(y ~ x),
      error = function(e) {
        if (length(unique(x)) == 1) {
          lm(y ~ 0)
        } else {
          stop(paste(
            "Cannot fit a smoother for", column_name,
            "The error message is:", e$message
          ))
        }
      }
    )
  }
}

smoother_gam_predict <- function(smoother, new_x, is_discrete, column_name, na_ind = NULL) {
  newdata <- data.frame(x = new_x)
  newdata$na_ind <- na_ind
  tryCatch(
    as.numeric(predict(smoother, newdata = newdata)),
    warning = function(w) {
      if (grepl("^factor levels .* not in original fit$", w$message)) {
        idx_known <- new_x %in% smoother$model$x
        y <- vector("numeric", length = length(new_x))
        y[idx_known] <- as.numeric(predict(smoother, newdata = newdata[idx_known, , drop = FALSE]))
        y[!idx_known] <- 0
        return(y)
      } else {
        stop("Unknown value for prediction")
      }
    }
  )
}


#' Create a detailed model with gam smoother
#'
#' @inheritParams sculpt_detailed_generic
#'
#' @details For parallel computation, use [parallel_set()] and set `allow_par` to `TRUE`.
#' Note that parallel computation may fail if the model is too big and there is not enough memory.
#'
#' @return Object of classes `detailed` and `sculpture`.
#' @export
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
#' ds <- sculpt_detailed_gam(rs)
#' class(ds)
#'
sculpt_detailed_gam <- function(rs, missings = NULL, verbose = 0, allow_par = FALSE) {
  requireNamespace("mgcv")
  sculpt_detailed_generic(
    rs = rs, verbose = verbose,
    allow_par = allow_par,
    smoother_fit = smoother_gam,
    smoother_predict = smoother_gam_predict,
    missings = missings
  )
}




smoother_lm <- function(x, y, is_discrete, column_name, na_ind = NULL) {
  tryCatch(
    lm(as.formula(paste0("y ~ x", `if`(!is.null(na_ind), " + na_ind")))),
    error = function(e) {
      if (length(unique(x)) == 1) {
        lm(y ~ 0)
      } else {
        stop(paste(
          "Cannot fit a smoother for", column_name,
          "The error message is:", e$message
        ))
      }
    }
  )
}

smoother_lm_predict <- function(smoother, new_x, is_discrete, column_name, na_ind = NULL) {
  newdata <- data.frame(x = new_x)
  newdata$na_ind <- na_ind
  tryCatch(
    unname(predict(smoother, newdata = newdata)),
    error = function(e) {
      if (is_discrete && grepl("factor x has new level", e$message)) {
        idx_known <- new_x %in% smoother$model$x
        y <- vector("numeric", length = length(new_x))
        y[idx_known] <- as.numeric(predict(smoother, newdata = newdata[idx_known, , drop = FALSE]))
        y[!idx_known] <- 0
        return(y)
      } else {
        stop("Unknown value for prediction")
      }
    }
  )
}


#' Create a detailed model with lm smoother
#'
#' @inheritParams sculpt_detailed_generic
#'
#' @details For parallel computation, use [parallel_set()] and set `allow_par` to `TRUE`.
#' Note that parallel computation may fail if the model is too big and there is not enough memory.
#'
#' @return Object of classes `detailed` and `sculpture`.
#' @export
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
#' ds <- sculpt_detailed_lm(rs)
#' class(ds)
#'
sculpt_detailed_lm <- function(rs, missings = NULL, verbose = 0, allow_par = FALSE) {
  sculpt_detailed_generic(
    rs = rs, verbose = verbose,
    allow_par = allow_par,
    smoother_fit = smoother_lm,
    smoother_predict = smoother_lm_predict,
    missings = missings
  )
}


# polished sculpture --------


#' Create a polished model
#'
#' @param object Object of class `sculpture`, either `rough` or `detailed`.
#' @param k Number of most important variables to keep.
#' @param vars Vector of variables to keep.
#'
#' @return Object of classes `rough` / `detailed` and `sculpture`.
#' @export
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
#' ds <- sculpt_detailed_gam(rs)
#'
#' # this keeps only "mpg"
#' ps <- sculpt_polished(ds, k = 1)
#'
sculpt_polished <- function(object, k = NULL, vars = NULL) {
  checkmate::assert_class(object, "sculpture")
  checkmate::assert(
    checkmate::check_null(k),
    checkmate::check_null(vars)
  )

  if (is.null(k)) {
    checkmate::assert_character(vars, min.len = 1, any.missing = FALSE)
    checkmate::assert_subset(vars, names(object))
  } else if (is.null(vars)) {
    checkmate::assert_number(k, lower = 1)
    vars <- levels(attr(object, "cumul_R2")$feature)[1:k]
  }

  res <- object[vars]
  attr(res, "offset") <- attr(object, "offset")
  class(res) <- class(object)

  # evaluate the sculpture
  es <- eval_sculpture(
    sculpture = res,
    data = as.data.frame(as.data.table(lapply(res, "[[", "x")))
  )

  # calculate variable importance
  dat_var <- calc_dir_var_imp_pdp(es$pdp)
  feat_order <- levels(dat_var$feature)

  # calculate cumulative R2
  dat_R2_cumul <- calc_cumul_R2_pdp(
    dt = es$pdp,
    feat_order = feat_order,
    model_predictions = es$prediction$pred,
    model_offset = es$offset
  )

  # calculate range
  dat_range <- calc_range_pdp(es$pdp)

  attr(res, "var_imp") <- dat_var
  attr(res, "cumul_R2") <- dat_R2_cumul
  attr(res, "range") <- dat_range

  return(res)
}



# utils -----


eval_sculpture <- function(sculpture, data) {
  stopifnot(
    inherits(sculpture, "sculpture"),
    ncol(data) >= length(sculpture),
    all(names(sculpture) %in% colnames(data))
  )

  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  . <- rn <- pdp_c <- NULL # due to NSE notes in R CMD check

  # get offset for predictions
  offset <- attr(sculpture, "offset")

  # get predict functions
  interp_funs <- lapply(sculpture, "[[", "predict")

  # PDPs at data
  pdp <- lapply(names(sculpture), function(col) interp_funs[[col]](data[[col]]))
  names(pdp) <- names(sculpture)

  # reshape PDPs to create predictions
  pdp <- cbind(data.table(rn = 1:nrow(data)), do.call("cbind", pdp))
  pdp <- melt(pdp, id.vars = "rn", variable.name = "feature", value.name = "pdp_c")
  pred <- pdp[, .(pred = sum(pdp_c) + offset), .(rn)][order(rn)]

  return(list(
    pdp = pdp,
    offset = offset,
    prediction = pred
  ))
}

#' @export
predict.sculpture <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    newdata <- as.data.frame(as.data.table(lapply(object, "[[", "x")))
  } else {
    checkmate::assert_subset(names(object), colnames(newdata))
    newdata <- as.data.frame(newdata)
  }
  tmp <- eval_sculpture(
    sculpture = object,
    data = newdata[names(object)]
  )
  return(structure(tmp$prediction$pred, names = tmp$prediction$rn))
}

#' @export
print.sculpture <- function(x, ...) {
  n_vars <- length(x)
  cat(
    paste(
      stringr::str_to_sentence(class(x)[1]), "sculpture with",
      n_vars, paste0("variable", `if`(n_vars > 1, "s"))
    )
  )
}

# for transforming log-odds to probability
inv.logit <- function(x) 1 / (1 + exp(-x))
