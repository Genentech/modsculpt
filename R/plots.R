# colors -------

# function for generating colours
ms_color <- function(n, hue_coloring = FALSE) {
  if (n < 7 && !hue_coloring) {
    c("#0a0a0a", "#14a3a8", "#e3211d", "#b15829", "#6a3d9a", "#34a02b")[1:n]
  } else {
    hcl(h = seq(15, 375, length = n + 1), l = 35, c = 85)[1:n]
  }
}


# facets specification -------

#' Instructions for facet vizualisations
#'
#' @param labels (`NULL`) or named character vector with variable labels.
#' @param ncol (`NULL`) or number of columns in the facet.
#' @param sort One of "alphabetical", "importance", or "range" - sorting of the facets.
#' @param top_k (`NULL`) or number of most important features to show.
#' @param subset (`NULL`) or a vector of variables to show.
#' @param scales One of "free", "free_x", or "free_y" - axis scales of the graphs.
#'
#' @return List of class `facet_specification`.
#' @export
#'
#' @examples
#' \dontrun{
#' g_ice(
#'   sculpture,
#'   facet_spec = facet_specification(
#'     ncol = 3, # display 3 columns
#'     sort = "importance" # sort by importance
#'   )
#' )
#' }
#'
facet_specification <- function(labels = NULL,
                                ncol = NULL,
                                sort = "alphabetical",
                                top_k = NULL,
                                subset = NULL,
                                scales = "free_x") {
  checkmate::assert(
    checkmate::check_character(labels, any.missing = FALSE, names = "named"),
    checkmate::check_null(labels)
  )

  checkmate::assert(
    checkmate::check_integerish(ncol, any.missing = FALSE, len = 1, lower = 1),
    checkmate::check_null(ncol)
  )

  checkmate::assert_character(sort, any.missing = FALSE, len = 1)
  checkmate::assert_subset(sort, c("alphabetical", "importance", "range"))

  checkmate::assert(
    checkmate::check_number(top_k, lower = 1),
    checkmate::check_null(top_k)
  )

  checkmate::assert(
    checkmate::check_character(subset),
    checkmate::check_null(subset)
  )

  checkmate::assert_character(scales, len = 1, any.missing = FALSE)
  checkmate::assert_subset(scales, c("free", "free_y", "free_x"))

  if (!is.null(top_k) & !is.null(subset)) {
    stop("Please use either `top_k` or `subset`, but not both together.")
  }

  out <- list(
    labels = labels, ncol = ncol, sort = sort, top_k = top_k,
    subset = subset, scales = scales
  )
  class(out) <- "facet_specification"

  return(out)
}


resolve_facet_specification <- function(obj, fs) {
  # checks
  checkmate::assert_class(obj, "sculpture")
  checkmate::assert_class(fs, "facet_specification")

  # resolve labels
  if (is.null(fs$labels)) {
    fs$labels <- structure(names(obj), names = names(obj))
  }
  checkmate::assert_character(
    fs$labels,
    names = "named", len = length(obj), any.missing = FALSE,
    .var.name = "facet_specification$labels"
  )

  # resolve facet sorting
  feat_ordered <- resolve_facet_sort(obj = obj, facet_sort = fs$sort)

  # resolve facet subset and top_k
  obj <- resolve_facet_subset_topk(
    obj = obj, facet_subset = fs$subset, facet_top_k = fs$top_k,
    feat_ordered = feat_ordered
  )

  # resolve facet ncol
  idx_c <- !vapply(obj, "[[", logical(1), "is_discrete")
  facet_ncol_res <- resolve_facet_ncol(idx_c = idx_c, facet_ncol = fs$ncol)

  return(
    list(
      object = obj,
      labels = fs$labels,
      ncol_c = facet_ncol_res$ncol_c,
      ncol_d = facet_ncol_res$ncol_d,
      scales = fs$scales
    )
  )
}


resolve_facet_sort <- function(obj, facet_sort) {
  if (facet_sort == "alphabetical") {
    feat_ordered <- sort(names(obj))
  } else if (facet_sort == "importance") {
    vimp <- attr(obj, "var_imp")
    feat_ordered <- levels(vimp$feature)
  } else if (facet_sort == "range") {
    rng <- attr(obj, "range")
    feat_ordered <- levels(rng$feature)
  } else {
    stop("Unknown sorting")
  }
  return(feat_ordered)
}

resolve_facet_subset_topk <- function(obj, facet_subset, facet_top_k, feat_ordered) {
  stopifnot(
    is.null(facet_subset) || is.null(facet_top_k),
    all(feat_ordered %in% names(obj))
  )
  if (!is.null(facet_top_k)) {
    vars <- feat_ordered[1:min(facet_top_k, length(feat_ordered))]
  } else if (!is.null(facet_subset)) {
    vars <- feat_ordered[feat_ordered %in% facet_subset]
  } else {
    vars <- feat_ordered
  }
  new_attrs <- attributes(obj)
  obj <- obj[vars]
  new_attrs$names <- names(obj)
  attributes(obj) <- new_attrs
  return(obj)
}

resolve_facet_ncol <- function(idx_c, facet_ncol) {
  n_feat_c <- sum(idx_c)
  n_feat_d <- sum(!idx_c)
  if (is.null(facet_ncol)) {
    facet_ncol_c <- min(c(n_feat_c, 4))
    facet_ncol_d <- min(c(n_feat_d, 4))
  } else {
    facet_ncol_c <- min(c(n_feat_c, facet_ncol))
    facet_ncol_d <- min(c(n_feat_d, facet_ncol))
  }
  return(list(ncol_c = facet_ncol_c, ncol_d = facet_ncol_d))
}



resolve_y_limits <- function(dat_c, dat_d, facet_scales) {
  if (facet_scales %in% c("free", "free_y")) {
    c(NA_real_, NA_real_)
  } else {
    c(
      floor(min(c(dat_c[["y"]], dat_d[["y"]])) * 10) / 10,
      ceiling(max(c(dat_c[["y"]], dat_d[["y"]])) * 10) / 10
    )
  }
}



# missings specification -------


#' Instructions for missings vizualisations
#'
#' @param vline (`logical`) Should the vertical line be shown? Defaults to `FALSE`.
#' @param hline (`logical`) Should the horizontal line be shown? Defaults to `FALSE`.
#' @param values (`NULL`) or single value or a named vector.
#' Specifies the value(-s) that stand for the missing values.
#' If `NULL`, then no missing value handling is carried out.
#' If single value, then it is assumed that this value is used for flagging missing values across
#' all continuous variables.
#' If named vector, then the names are used to refer to continuous variables and the values for
#' flagging missing values in that variable.
#' @param drop_from_plot (`logical`) Should the missing values be dropped from plot?
#' Defaults to `FALSE`.
#'
#' @return List of class `missings_specification`.
#' @export
#'
#' @examples
#' \dontrun{
#' g_ice(
#'   sculpture,
#'   missings_spec = missings_specification(
#'     vline = TRUE, # show vertical line
#'     values = -1 # NAs in all continuous variables displayed as -1
#'   )
#' )
#' }
#'
missings_specification <- function(vline = FALSE, hline = FALSE, values = NULL,
                                   drop_from_plot = FALSE) {
  checkmate::assert_flag(vline)
  checkmate::assert_flag(hline)
  checkmate::assert_flag(drop_from_plot)
  checkmate::assert(
    checkmate::check_null(values),
    checkmate::check_atomic(values, any.missing = FALSE, len = 1),
    checkmate::check_atomic(values, any.missing = FALSE, min.len = 2, names = "named")
  )
  if (any(c(vline, hline)) && is.null(values)) {
    stop("Specified to show lines, but no missing values provided.")
  }
  if (drop_from_plot && is.null(values)) {
    stop("Specified to drop missings from plot area, but no missing values provided.")
  }
  if (drop_from_plot && vline) {
    stop("Please use either `drop_from_plot` or `vline`, but not both.")
  }
  out <- list(vline = vline, hline = hline, values = values, drop_from_plot = drop_from_plot)
  class(out) <- "missings_specification"
  return(out)
}

resolve_missings_specification <- function(dat_c, ms, missings) {
  if (is.null(missings)) {
    return(list(dat_c = dat_c, missings = missings))
  }

  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  line_id <- feature <- ..cols <- NULL # due to NSE notes in R CMD check

  # add PDP_centered column
  if ("line_id" %in% colnames(dat_c)) {
    dat_c_pdp <- dat_c[line_id == "pdp"]
  } else {
    dat_c_pdp <- dat_c
  }
  missings_new <- missings[dat_c_pdp, nomatch = NULL, on = c("feature", "x")]
  cols <- c("feature", "x", "y", `if`("Model" %in% colnames(missings_new), "Model"))
  missings_new <- missings_new[, ..cols]
  missings_new[, feature := factor(feature, levels = levels(dat_c$feature))]

  # remove missing observations if requested
  if (ms$drop_from_plot) {
    dat_c <- dat_c[!missings, on = c("feature", "x")]
  }
  return(list(dat_c = dat_c, missings = missings_new))
}



# plots -------


#' Plot variable importances and cumulative approximation of R^2
#'
#' @param object (`sculpture`)
#' @param feat_labels (`NULL`) or named character vector providing the variable labels.
#' @param textsize Size of text.
#' @param top_k (`NULL`) or number to show only the most `k` important variables.
#' @param pdp_plot_sample (`logical`) Sample PDP for faster ploting? Defaults to `TRUE`.
#' @param show_pdp_plot (`logical`) Show plot with PDP ranges? Defaults to `TRUE`.
#' @param var_imp_type (`character`) One of `c("normalized", "absolute", "ice", "ice_orig_mod")`.
#' Defaults to "normalized". "ice" is only valid for a rough sculpture.
#' @param logodds_to_prob (`logical`) Only valid for binary response and sculptures built on
#' the log-odds scale. Defaults to `FALSE` (i.e. no effect).
#' If `TRUE`, then the y-values are transformed through inverse logit function 1 / (1 + exp(-x)).
#' @param plot_ratios (`numeric`) Used in the layout matrix of `gridExtra::arrangeGrob()`.
#' If `show_pdp_plot`, then the default is `c(3,2,2)`, making the first plot 3 units wide and
#' the other two plots 2 units wide.
#' If `!show_pdp_plot`, then the default is `c(3,2)`, making the first plot 3 units wide and
#' the second plot 2 units wide.
#' Note that the length needs to be 3 if `show_pdp_plot` or 2 if `!show_pdp_plot`.
#'
#' @return `grob`. Use `grid::grid.draw` to plot the output
#' (`grid::grid.newpage` resets the plotting area).
#'
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
#' # optionally define labels
#' labels <- structure(
#'   toupper(covariates), # labels
#'   names = covariates # current (old) names
#' )
#' vi <- g_var_imp(rs, feat_labels = labels)
#' grid::grid.draw(vi)
#'
g_var_imp <- function(object, feat_labels = NULL, textsize = 16, top_k = NULL,
                      pdp_plot_sample = TRUE,
                      show_pdp_plot = TRUE,
                      var_imp_type = "normalized",
                      logodds_to_prob = FALSE,
                      plot_ratios = `if`(show_pdp_plot, c(3, 2, 2), c(3, 2))) {
  checkmate::assert_class(object, "sculpture")
  checkmate::assert_integerish(textsize, len = 1, any.missing = FALSE)
  checkmate::assert_flag(pdp_plot_sample)
  checkmate::assert_flag(show_pdp_plot)
  checkmate::assert_choice(var_imp_type, choices = c("normalized", "absolute", "ice", "ice_orig_mod"))
  checkmate::assert_flag(logodds_to_prob)

  if (show_pdp_plot) {
    checkmate::assert_integerish(plot_ratios, lower = 1, len = 3, any.missing = FALSE)
  } else {
    checkmate::assert_integerish(plot_ratios, lower = 1, len = 2, any.missing = FALSE)
  }

  if (is.null(feat_labels)) {
    feat_labels <- structure(names(object), names = names(object))
  }
  checkmate::assert_character(feat_labels, names = "named", len = length(object))

  checkmate::assert(
    checkmate::check_null(top_k),
    checkmate::check_integerish(top_k, any.missing = FALSE, len = 1, lower = 1)
  )

  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  feature <- . <- pdp_c <- ice_centered <- line_id <- var_y <-
    NULL # due to NSE notes in R CMD check

  if (logodds_to_prob) {
    # evaluate the sculpture
    es <- eval_sculpture(
      sculpture = object,
      data = as.data.frame(as.data.table(lapply(object, "[[", "x")))
    )
    dt <- es$pdp

    # convert log-odds scale to probability scale and center back to 0
    dt[, pdp_c := inv.logit(pdp_c) - 0.5]

    # get importance
    dat_var <- calc_dir_var_imp_pdp(dt)
    feat_order <- levels(dat_var$feature)

    # get cumul. R2
    dat_R2_cumul <- calc_cumul_R2_pdp(
      dt = dt,
      feat_order = feat_order,
      model_predictions = inv.logit(es$prediction$pred),
      model_offset = inv.logit(es$offset)
    )
  } else {
    # get importance
    dat_var <- attr(object, "var_imp")
    feat_order <- levels(dat_var$feature)

    # get cumul. R2
    dat_R2_cumul <- attr(object, "cumul_R2")

    # get PDPs and predictions
    if (show_pdp_plot) {
      eg <- eval_sculpture(
        sculpture = object,
        data = as.data.frame(as.data.table(lapply(object, "[[", "x")))
      )
      dt <- eg$pdp
    }
  }

  # subset top_k if requested
  if (!is.null(top_k)) {
    top_k <- min(top_k, nrow(dat_var))
    feat_order <- feat_order[1:top_k]
    dat_var <- dat_var[feature %in% feat_order]
    dat_R2_cumul <- dat_R2_cumul[feature %in% feat_order]
    if (show_pdp_plot) {
      object <- object[feat_order]
      class(object) <- "sculpture"
    }
  }

  # g1 - PDP values
  if (show_pdp_plot) {
    # check centering
    check_dt <- dt[, .(mean_pdp_c = mean(pdp_c)), .(feature)]
    if (abs(mean(check_dt$mean_pdp_c)) > 1e-1) {
      stop(paste(
        "PDPs not centered, mean relative difference of",
        abs(mean(check_dt$mean_pdp_c))
      ))
    }
    # draw PDP plot
    dt$feature <- factor(dt$feature, levels = feat_order)
    g1 <- g_pdp(dt = dt, pdp_plot_sample = pdp_plot_sample, feat_labels = feat_labels)
  } else {
    g1 <- NULL
  }

  # g2 - variable importance
  if (var_imp_type == "normalized") {
    g2 <- g_imp_norm(dat_var = dat_var, show_pdp_plot = show_pdp_plot, textsize = textsize)
  } else if (var_imp_type == "absolute") {
    g2 <- g_imp_abs(dat_var = dat_var, show_pdp_plot = show_pdp_plot, textsize = textsize)
  } else if (var_imp_type == "ice_orig_mod") {
    if (!inherits(object, "rough")) {
      stop('`var_imp_type == "ice"` is only valid for a rough sculpture.')
    }
    # get ice curves
    dat_var_ice <- rbindlist(
      lapply(
        object,
        function(v) {
          generate_ice_data(
            predictions = v[["ice"]],
            x = v$x,
            logodds_to_prob = logodds_to_prob
          )[, .(y, line_id)]
        }
      ),
      idcol = "feature"
    )
    # convert to factor
    dat_var_ice$feature <- factor(dat_var_ice$feature, levels = feat_order)
    # calculate variance
    dat_var_ice <- dat_var_ice[, .(var_y = var(y)), by = .(feature, line_id)]
    # calculate mean of variances
    vars_mean <- dat_var_ice[, .(mean_var_y = mean(var_y)), by = .(feature)]
    # plot ice variances
    g2 <- g_imp_ice(vars = dat_var_ice, vars_mean = vars_mean)
  } else if (var_imp_type == "ice") {
    model_predict_fun <- function(x) {
      if(logodds_to_prob) {
        p <- predict(object, newdata = x)
        inv.logit(p)
      } else {
        predict(object, newdata = x)
      }
    }

    dat_var_ice <- rbindlist(
      lapply(
        object,
        function(v) {
          calculate_ice_data(
              sub = v$subsets,
              predict_fun = model_predict_fun,
              x = v$x,
              x_name = v$x_name,
              col_order = names(object)
          )[, .(ice, line_id)]
        }
      ),
      idcol = "feature"
    )

    # convert to factor
    dat_var_ice$feature <- factor(dat_var_ice$feature, levels = feat_order)
    # calculate variance
    dat_var_ice <- dat_var_ice[, .(var_y = var(ice)), by = .(feature, line_id)]
    # calculate mean of variances
    vars_mean <- dat_var_ice[, .(mean_var_y = mean(var_y)), by = .(feature)]
    # plot ice variances
    g2 <- g_imp_ice(vars = dat_var_ice, vars_mean = vars_mean)
  }

  if (show_pdp_plot) {
    g2 <- g2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
  } else {
    g2 <- g2 + scale_y_discrete(labels = function(x) feat_labels[x])
  }

  # g3 - cumulative R2
  g3 <- g_cumulR2(dat_R2_cumul = dat_R2_cumul, textsize = textsize)

  # combined graph
  if (show_pdp_plot) {
    g_var_imp <- gridExtra::arrangeGrob(
      g1 + theme(
        plot.margin = unit(c(0.8, 0.5, 0.3, 0.3), "cm"),
        text = element_text(size = textsize)
      ),
      g2 + theme(
        plot.margin = unit(c(0.8, 0.5, 0.3, 0.3), "cm"),
        text = element_text(size = textsize)
      ),
      g3 + theme(
        plot.margin = unit(c(0.8, 0.5, 0.05, 0.3), "cm"),
        text = element_text(size = textsize)
      ),
      layout_matrix = matrix(rep(1:3, plot_ratios), nrow = 1)
    )
  } else {
    g_var_imp <- gridExtra::arrangeGrob(
      g2 + theme(
        plot.margin = unit(c(0.8, 0.5, 0.3, 0.3), "cm"),
        text = element_text(size = textsize)
      ),
      g3 + theme(
        plot.margin = unit(c(0.8, 0.5, 0.05, 0.3), "cm"),
        text = element_text(size = textsize)
      ),
      layout_matrix = matrix(rep(1:2, plot_ratios), nrow = 1)
    )
  }

  return(g_var_imp)
}


#' Plot additivity scatterplot(-s) with R^2 value(-s)
#'
#' @param sp Sculpted predictions. Either as a vector or as a list of those.
#' @param lp Learner predictions. Either as a vector or as a list of those. Same size as `sp`.
#' @param descriptions (Optional) Descriptions of the models to be shown on the plot.
#' Same size as `sp` if `sp` is provided as a list.
#' @param cex `cex` graphical parameter.
#' @param plot_only (`logical`) Return plot only or plot with the R^2 value?
#' Defaults to the first (i.e. `TRUE`).
#'
#' @return If `plot_only`, then a plot. If `!plot_only`, then a plot and a data.frame.
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
#' g_additivity(
#'   sp = predict(rs, pm),
#'   lp = model_predict(pm),
#'   descriptions = "Product Marginal"
#' )
#'
g_additivity <- function(sp, lp, descriptions = NULL, cex = 4, plot_only = TRUE) {
  checkmate::assert(
    checkmate::check_atomic(sp),
    checkmate::check_list(sp, types = "atomic")
  )
  if (!is.list(sp)) {
    sp <- list(sp)
  }

  checkmate::assert(
    checkmate::check_atomic(lp, any.missing = FALSE, len = length(sp[[1]])),
    checkmate::check_list(lp, types = "atomic", len = length(sp))
  )
  if (is.list(lp)) {
    lapply(
      seq_along(lp),
      function(i) checkmate::assert_atomic(lp[[i]], any.missing = FALSE, len = length(sp[[i]]))
    )
  } else {
    lp <- rep(list(lp), length(sp))
  }

  checkmate::assert(
    checkmate::check_null(descriptions),
    checkmate::check_character(descriptions, any.missing = FALSE, len = length(sp))
  )
  if (is.null(descriptions)) {
    if (is.null(names(sp))) {
      descriptions <- paste("Sculpture", seq_along(sp))
    } else {
      descriptions <- names(sp)
    }
  }

  checkmate::assert_numeric(cex, lower = 0, any.missing = FALSE, len = 1)
  checkmate::assert_logical(plot_only, any.missing = FALSE, len = 1)

  # get plot data
  pd <- lapply(seq_along(sp), function(i) {
    data.frame(
      sculpted = sp[[i]],
      learner = lp[[i]],
      Model = descriptions[i]
    )
  })
  pd <- do.call("rbind", pd)
  pd$Model <- factor(pd$Model, levels = descriptions)

  # calculate R2 (vs strong learner)
  R2_mod_vs_approx <- vapply(
    seq_along(sp),
    function(i) metrics_R2(score_fun = "score_quadratic", y = lp[[i]], y_hat = sp[[i]]),
    FUN.VALUE = numeric(1)
  )

  # create R2 annotations
  annotations <- data.frame(
    R2 = paste0("R^2==", round(R2_mod_vs_approx, 4)),
    Model = factor(descriptions, levels = descriptions),
    sculpted = min(pd$sculpted) + (max(pd$sculpted) - min(pd$sculpted)) / 10,
    learner = 0.9 * max(pd$learner)
  )

  g <- ggplot(pd) +
    geom_point(aes(x = .data$sculpted, y = .data$learner), alpha = 0.4, shape = 16) +
    geom_abline(slope = 1, intercept = 0) +
    facet_wrap("Model") +
    geom_label(
      data = annotations,
      mapping = aes(x = .data$sculpted, y = .data$learner, label = .data$R2),
      hjust = 0, parse = TRUE, size = cex
    ) +
    theme_bw() +
    labs(
      x = "Sculpted Model Predictions",
      y = "Learner Predictions"
    )

  if (plot_only) {
    return(g)
  } else {
    return(list(plot = g, R2 = data.frame(description = descriptions, R2 = R2_mod_vs_approx)))
  }
}



#' Plot centered ICE profiles with centered PDP curves
#'
#' @param object Object of classes `rough` and `sculpture`.
#' @param centered `logical`, centered ice plots? Defaults to `TRUE`.
#' @param show_PDP `logical`, show PDP line? Defaults to `TRUE`.
#' @param coloured `logical`, coloured curves? Defaults to `FALSE`.
#' @param rug_sides "" for none, "b", for bottom, "trbl" for all 4 sides (see `geom_rug`)
#' @param missings_spec Object of class `missings_specificatoin`.
#' @param facet_spec Object of class `facet_specificatoin`.
#' @param logodds_to_prob (`logical`) Only valid for binary response and sculptures built on
#' the log-odds scale. Defaults to `FALSE` (i.e. no effect).
#' If `TRUE`, then the y-values are transformed through inverse logit function 1 / (1 + exp(-x)).
#'
#' @return List of `ggplot`s (one for continuous features, one for discrete).
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
#' g_ice(rs)$continuous
#'
g_ice <- function(object, centered = TRUE, show_PDP = TRUE, coloured = FALSE,
                  rug_sides = "b",
                  missings_spec = missings_specification(),
                  facet_spec = facet_specification(),
                  logodds_to_prob = FALSE) {
  checkmate::assert_class(object, "rough")
  checkmate::assert_flag(centered)
  checkmate::assert_flag(show_PDP)
  checkmate::assert_flag(coloured)
  checkmate::assert_character(rug_sides, any.missing = FALSE, len = 1)
  checkmate::assert_class(facet_spec, "facet_specification")
  checkmate::assert_class(missings_spec, "missings_specification")
  checkmate::assert_flag(logodds_to_prob)

  # transform missings into a list of values per each continuous variable
  check_continuous <- vapply(object, "[[", logical(1), "is_discrete")
  check_continuous <- names(Filter(isFALSE, check_continuous))
  if (length(missings_spec$values) == 1) {
    missings <- data.table(feature = check_continuous, x = missings_spec$values)
  } else if (length(missings_spec$values) > 1) {
    missings <- data.table(feature = names(missings_spec$values), x = missings_spec$values)
    checkmate::assert_names(
      missings$feature,
      subset.of = check_continuous,
      .var.name = "missings_spec$values"
    )
  } else {
    missings <- NULL
  }

  if (coloured & show_PDP) {
    stop("Coloured lines are only available without PDP, so please set `show_PDP = FALSE`.")
  }

  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  . <- x <- x_ribbon <- line_id <- y_se <- feature <- NULL # due to NSE notes in R CMD check

  # resolve facet specification
  rfs <- resolve_facet_specification(obj = object, fs = facet_spec)
  object <- rfs$object

  # get continuous vars
  idx_continuous_vars <- !vapply(object, "[[", logical(1), "is_discrete")
  has_continuous <- any(idx_continuous_vars)
  has_discrete <- any(!idx_continuous_vars)

  pred_var <- if (centered) "ice_centered" else "ice"

  # continuous
  if (has_continuous) {
    # ICE
    ice_continuous <- rbindlist(
      lapply(
        object[idx_continuous_vars],
        function(v) {
          generate_ice_data(
            predictions = v[[pred_var]],
            x = v$x,
            logodds_to_prob = logodds_to_prob
          )
        }
      ),
      idcol = "feature"
    )
    ice_continuous <- unique(ice_continuous)
    ice_continuous[, `:=`(type = "ICE Profiles", line_id = as.character(line_id))]

    # PDP
    if (show_PDP) {
      pdp_continuous <- rbindlist(
        lapply(
          object[idx_continuous_vars],
          function(v) {
            generate_pdp_data(
              predictions = v[[pred_var]],
              x = v$x,
              logodds_to_prob = logodds_to_prob
            )
          }
        ),
        idcol = "feature"
      )
      pdp_continuous[, y_se := ifelse(is.na(y_se), 0, y_se)]
      pdp_continuous[, `:=`(line_id = "pdp", type = "Rough model (with SE)")]

      dat_c <- rbind(ice_continuous, pdp_continuous, fill = TRUE)
    } else {
      dat_c <- ice_continuous
    }
  } else {
    dat_c <- data.table(y = numeric(0), feature = character(0))
  }

  # discrete
  if (has_discrete) {
    # ICE
    ice_discrete <- rbindlist(
      lapply(
        object[!idx_continuous_vars],
        function(v) {
          generate_ice_data(
            predictions = v[[pred_var]],
            x = v$x,
            logodds_to_prob = logodds_to_prob
          )
        }
      ),
      idcol = "feature"
    )
    ice_discrete <- unique(ice_discrete)
    ice_discrete[, `:=`(type = "ICE Profiles", line_id = as.character(line_id))]

    # PDP
    if (show_PDP) {
      pdp_discrete <- rbindlist(
        lapply(
          object[!idx_continuous_vars],
          function(v) {
            generate_pdp_data(
              predictions = v[[pred_var]],
              x = v$x,
              logodds_to_prob = logodds_to_prob
            )
          }
        ),
        idcol = "feature"
      )
      pdp_discrete[, `:=`(line_id = "pdp", type = "Rough model (with SE)")]

      dat_d <- rbind(ice_discrete, pdp_discrete, fill = TRUE)
    } else {
      dat_d <- ice_discrete
    }
  } else {
    dat_d <- data.table(y = numeric(0), feature = character(0))
  }

  # resolve y limits
  y_limits <- resolve_y_limits(dat_c = dat_c, dat_d = dat_d, facet_scales = facet_spec$scales)

  # resolve facet sort - need to convert to factor
  dat_c[, feature := factor(feature, levels = names(object)[idx_continuous_vars])]
  dat_d[, feature := factor(feature, levels = names(object)[!idx_continuous_vars])]

  # resolve missings specification
  rms <- resolve_missings_specification(dat_c = dat_c, ms = missings_spec, missings = missings)
  dat_c <- rms$dat_c
  missings <- rms$missings

  # graph for continuous
  if (nrow(dat_c) > 0) {
    gc <- ggplot()

    if (show_PDP) {
      gc <- gc +
        geom_ribbon(
          mapping = aes(
            x = .data$x,
            ymin = .data$y - .data$y_se,
            ymax = .data$y + .data$y_se
          ),
          data = dat_c[line_id == "pdp"],
          na.rm = T, alpha = 0.4, colour = "lightblue", fill = "lightblue"
        )
    }

    if (coloured) {
      gc <- gc +
        geom_line(
          mapping = aes(x = .data$x, y = .data$y, colour = .data$line_id),
          linewidth = 1,
          alpha = 0.3,
          data = dat_c,
          na.rm = F
        )
    } else {
      gc <- gc +
        geom_line(
          mapping = aes(
            x = .data$x, y = .data$y,
            group = .data$line_id, colour = .data$type
          ),
          data = dat_c,
          na.rm = F
        )
    }

    gc <- gc +
      geom_rug(
        mapping = aes(x = .data$x, y = .data$y),
        data = dat_c[line_id == 1],
        na.rm = F,
        sides = rug_sides
      ) +
      facet_wrap(
        "feature",
        scales = rfs$scales, ncol = rfs$ncol_c,
        labeller = as_labeller(rfs$labels)
      )

    if (!coloured) {
      gc <- gc +
        scale_color_manual(
          values = c(
            "ICE Profiles" = ifelse(show_PDP, "gray60", "black"),
            "Rough model (with SE)" = "blue"
          )[c(T, show_PDP)],
          name = ""
        )
    }

    # add missings lines
    if (!is.null(missings)) {
      if (missings_spec$vline) {
        gc <- gc +
          geom_vline(
            mapping = aes(xintercept = .data$x),
            data = missings,
            linetype = "dotted"
          )
      }
      if (missings_spec$hline) {
        gc <- gc +
          geom_hline(
            mapping = aes(yintercept = .data$y, linetype = "Score for Missing Feature"),
            data = missings
          ) +
          scale_linetype_manual(values = c("Score for Missing Feature" = "dotted"), name = NULL)
      }
    }

    gc <- gc +
      labs(x = "Features", y = "Feature Score", caption = "", colour = NULL) +
      ylim(y_limits) +
      theme_bw()

    if (show_PDP) {
      gc <- gc + guides(colour = guide_legend(override.aes = list(size = 1)))
    } else {
      gc <- gc + guides(colour = guide_none())
    }

    if (coloured) {
      dat_c2 <- dat_c
      dat_c$x_perc <- ecdf(dat_c$x)(dat_c$x)
      gc2 <- ggplot() +
        geom_line(
          mapping = aes(x = .data$x_perc, y = .data$y, colour = .data$line_id),
          linewidth = 1,
          alpha = 0.3,
          data = dat_c,
          na.rm = F
        ) +
        geom_rug(
          mapping = aes(x = .data$x_perc, y = .data$y),
          data = dat_c[line_id == 1],
          na.rm = F,
          sides = rug_sides
        ) +
        facet_wrap(
          "feature",
          scales = rfs$scales, ncol = rfs$ncol_c,
          labeller = as_labeller(rfs$labels)
        ) +
        labs(x = "Features", y = "Feature Score", caption = "", colour = NULL) +
        ylim(y_limits) +
        theme_bw() +
        guides(colour = guide_none())
    }
  } else {
    gc <- NULL
  }

  # graph for discrete
  if (nrow(dat_d) > 0) {
    gd <- ggplot()

    if (show_PDP) {
      dat_d[line_id == "pdp", x_ribbon := as.numeric(droplevels(as.factor(x))), by = .(feature)]
      gd <- gd +
        geom_point(
          mapping = aes(x = .data$x, y = .data$y, colour = .data$type),
          data = dat_d[line_id == "pdp"],
          na.rm = T
        ) +
        geom_ribbon(
          mapping = aes(
            x = .data$x_ribbon,
            ymin = .data$y - .data$y_se,
            ymax = .data$y + .data$y_se
          ),
          data = dat_d[line_id == "pdp"],
          na.rm = T, alpha = 0.4, colour = "lightblue", fill = "lightblue"
        )
    }

    if (coloured) {
      gd <- gd +
        geom_line(
          mapping = aes(x = .data$x, y = .data$y, colour = .data$line_id, group = .data$line_id),
          linewidth = 1,
          alpha = 0.3,
          data = dat_d,
          na.rm = F
        )
    } else {
      gd <- gd +
        geom_line(
          mapping = aes(
            x = .data$x, y = .data$y,
            colour = .data$type, group = .data$line_id
          ),
          data = dat_d,
          na.rm = F
        )
    }

    gd <- gd +
      facet_wrap(
        "feature", scales = rfs$scales, ncol = rfs$ncol_d,
        labeller = as_labeller(rfs$labels)
      )

    if (!coloured) {
      gd <- gd +
        scale_color_manual(
          values = c(
            "ICE Profiles" = ifelse(show_PDP, "gray60", "black"),
            "Rough model (with SE)" = "blue"
          )[c(T, show_PDP)],
          name = ""
        )
    }

    gd <- gd +
      labs(x = "Features", y = "Feature Score", caption = "", colour = NULL, linetype = NULL) +
      ylim(y_limits) +
      theme_bw()

    if (show_PDP) {
      gd <- gd + guides(colour = guide_legend(override.aes = list(size = 1)))
    } else {
      gd <- gd + guides(colour = guide_none())
    }
  } else {
    gd <- NULL
  }

  if (coloured) {
    return(list(continuous = gc, discrete = gd, perc = gc2))
  } else {
    return(list(continuous = gc, discrete = gd))
  }
}


#' Plot component functions
#'
#' @param object Object of class `sculpture`.
#' @inheritParams g_ice
#'
#' @return List of `ggplot`s (one for continuous features, one for discrete).
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
#' g_component(ds)$continuous
#'
g_component <- function(object, rug_sides = "b",
                        missings_spec = missings_specification(),
                        facet_spec = facet_specification(),
                        logodds_to_prob = FALSE) {
  checkmate::assert_class(object, "sculpture")
  checkmate::assert_character(rug_sides, any.missing = FALSE, len = 1)
  checkmate::assert_class(missings_spec, "missings_specification")
  checkmate::assert_class(facet_spec, "facet_specification")
  checkmate::assert_flag(logodds_to_prob)

  # transform missings into a list of values per each continuous variable
  check_continuous <- vapply(object, "[[", logical(1), "is_discrete")
  check_continuous <- names(Filter(isFALSE, check_continuous))
  if (length(missings_spec$values) == 1) {
    missings <- data.table(feature = check_continuous, x = missings_spec$values)
  } else if (length(missings_spec$values) > 1) {
    missings <- data.table(feature = names(missings_spec$values), x = missings_spec$values)
    checkmate::assert_names(
      missings$feature,
      subset.of = check_continuous,
      .var.name = "missings_spec$values"
    )
  } else {
    missings <- NULL
  }

  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  feature <- NULL # due to NSE notes in R CMD check

  # resolve facet specification
  rfs <- resolve_facet_specification(obj = object, fs = facet_spec)
  object <- rfs$object

  # get continuous vars
  idx_continuous_vars <- !vapply(object, "[[", logical(1), "is_discrete")
  has_continuous <- any(idx_continuous_vars)
  has_discrete <- any(!idx_continuous_vars)

  if (has_continuous) {
    dat_c <- rbindlist(
      lapply(
        object[idx_continuous_vars],
        function(v) {
          data.table(
            x = v$x,
            y = `if`(logodds_to_prob, inv.logit(v$predict(v$x)), v$predict(v$x))
          )
        }
      ),
      idcol = "feature"
    )
  } else {
    dat_c <- data.table(feature = character(0))
  }

  if (has_discrete) {
    dat_d <- rbindlist(
      lapply(
        object[!idx_continuous_vars],
        function(v) {
          data.table(
            x = v$x,
            y = `if`(logodds_to_prob, inv.logit(v$predict(v$x)), v$predict(v$x))
          )
        }
      ),
      idcol = "feature"
    )
  } else {
    dat_d <- data.table(feature = character(0))
  }

  # resolve y limits
  y_limits <- resolve_y_limits(dat_c = dat_c, dat_d = dat_d, facet_scales = facet_spec$scales)

  # resolve facet sort - need to convert to factor
  dat_c[, feature := factor(feature, levels = names(object)[idx_continuous_vars])]
  dat_d[, feature := factor(feature, levels = names(object)[!idx_continuous_vars])]

  # resolve missings specification
  rms <- resolve_missings_specification(dat_c = dat_c, ms = missings_spec, missings = missings)
  dat_c <- rms$dat_c
  missings <- rms$missings

  if (missings_spec$hline) {
    legend_model_name <- paste(stringr::str_to_title(class(object)[1]), "Model Component")
    line_mapping <- aes(
      x = .data$x, y = .data$y,
      group = .data$feature, linetype = .data$legend_model_name
    )
  } else {
    line_mapping <- aes(x = .data$x, y = .data$y, group = .data$feature)
  }

  if (nrow(dat_c) > 0) {
    gc <- ggplot(dat_c) +
      geom_line(mapping = line_mapping) +
      geom_rug(
        mapping = aes(x = .data$x, y = .data$y),
        na.rm = F,
        sides = rug_sides
      ) +
      facet_wrap(
        "feature", scales = rfs$scales, ncol = rfs$ncol_c,
        labeller = as_labeller(rfs$labels)
      )

    # add missings lines
    if (!is.null(missings)) {
      if (missings_spec$vline) {
        gc <- gc +
          geom_vline(
            mapping = aes(xintercept = .data$x),
            data = missings,
            linetype = "dotted"
          )
      }
      if (missings_spec$hline) {
        gc <- gc +
          geom_hline(
            mapping = aes(yintercept = .data$y, linetype = "Score for Missing Feature"),
            data = missings
          ) +
          scale_linetype_manual(
            values = structure(
              c("dotted", "solid"),
              names = c("Score for Missing Feature", legend_model_name)
            ),
            name = NULL
          )
      }
    }

    gc <- gc +
      labs(x = "Features", y = "Feature Score") +
      ylim(y_limits) +
      theme_bw()
  } else {
    gc <- NULL
  }

  if (nrow(dat_d) > 0) {
    gd <- ggplot(dat_d) +
      geom_line(aes(x = .data$x, y = .data$y, group = .data$feature)) +
      facet_wrap(
        "feature", scales = rfs$scales, ncol = rfs$ncol_d,
        labeller = as_labeller(rfs$labels)
      ) +
      labs(x = "Features", y = "Feature Score") +
      ylim(y_limits) +
      theme_bw()
  } else {
    gd <- NULL
  }

  return(list(continuous = gc, discrete = gd))
}

#' Plot comparison of component functions
#'
#' @param sculptures List of objects of classes `sculpture`.
#' @param descriptions Character vector with model names. Same length as `sculptures`.
#' @inheritParams g_ice
#' @param hue_coloring Logical, use hue-based coloring?
#' Defaults to FALSE, meaning that predefined colors will be used instead.
#'
#' @details The first element of `sculptures` works as a reference sculpture.
#' All other sculptures must have a subset of variables with respect to the first one
#' (i.e. the same variables or less, but not new ones).
#' This allows to visualize polished together with non-polished sculptures,
#' if the non-polished one is specified as the first one.
#'
#' @return List of `ggplot`s (one for continuous features, one for discrete).
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
#' # also define simple labels
#' labels <- structure(
#'   toupper(covariates), # labels
#'   names = covariates # current (old) names
#' )
#'
#' # Component functions of "Detailed" and "Polished" are the same for "mpg" variable,
#' # therefore red curve overlays the blue one for "mpg"
#' comp <- g_comparison(
#'   sculptures = list(rs, ds, ps),
#'   descriptions = c("Rough", "Detailed", "Polished"),
#'   facet_spec = facet_specification(ncol = 2, labels = labels)
#' )
#' comp$continuous
#' comp$discrete
#'
g_comparison <- function(sculptures, descriptions, rug_sides = "b",
                         missings_spec = missings_specification(),
                         facet_spec = facet_specification(),
                         hue_coloring = FALSE,
                         logodds_to_prob = FALSE) {
  checkmate::assert_list(sculptures, types = "sculpture")
  checkmate::assert_character(descriptions, len = length(sculptures))
  checkmate::assert_character(rug_sides, any.missing = FALSE, len = 1)
  checkmate::assert_class(facet_spec, "facet_specification")
  checkmate::assert_flag(hue_coloring)
  checkmate::assert_flag(logodds_to_prob)

  names_sc_1 <- names(sculptures[[1]])
  check_names <- vapply(sculptures, function(sc) all(names(sc) %in% names_sc_1), logical(1))
  if (!all(check_names)) {
    stop("All sculptures must be subsets of the first sculpture (in terms of variables).")
  }

  # transform missings into a list of values per each continuous variable
  check_continuous <- vapply(sculptures[[1]], "[[", logical(1), "is_discrete")
  check_continuous <- names(Filter(isFALSE, check_continuous))
  if (length(missings_spec$values) == 1) {
    missings <- data.table(feature = check_continuous, x = missings_spec$values)
  } else if (length(missings_spec$values) > 1) {
    missings <- data.table(feature = names(missings_spec$values), x = missings_spec$values)
    checkmate::assert_names(
      missings$feature,
      subset.of = check_continuous,
      .var.name = "missings_spec$values"
    )
  } else {
    missings <- NULL
  }

  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  feature <- Model <- NULL # due to NSE notes in R CMD check

  # resolve facet specification
  rfs <- resolve_facet_specification(obj = sculptures[[1]], fs = facet_spec)
  sculptures[[1]] <- rfs$object

  # get continuous vars
  idx_continuous_vars <- !vapply(sculptures[[1]], "[[", logical(1), "is_discrete")
  has_continuous <- any(idx_continuous_vars)
  has_discrete <- any(!idx_continuous_vars)

  if (has_discrete) {
    dat_d <- rbindlist(
      lapply(
        seq_along(sculptures),
        function(i) {
          rbindlist(
            lapply(
              sculptures[[i]][vapply(sculptures[[i]], "[[", logical(1), "is_discrete")],
              function(v) {
                data.table(
                  x = v$x,
                  y = `if`(logodds_to_prob, inv.logit(v$predict(v$x)), v$predict(v$x)),
                  Model = descriptions[i]
                )
              }
            ),
            idcol = "feature"
          )
        }
      )
    )
    dat_d$Model <- factor(dat_d$Model, levels = descriptions)
  } else {
    dat_d <- data.table(feature = character(0))
  }

  if (has_continuous) {
    dat_c <- rbindlist(
      lapply(
        seq_along(sculptures),
        function(i) {
          rbindlist(
            lapply(
              sculptures[[i]][!vapply(sculptures[[i]], "[[", logical(1), "is_discrete")],
              function(v) {
                data.table(
                  x = v$x,
                  y = `if`(logodds_to_prob, inv.logit(v$predict(v$x)), v$predict(v$x)),
                  Model = descriptions[i]
                )
              }
            ),
            idcol = "feature"
          )
        }
      )
    )
    dat_c$Model <- factor(dat_c$Model, levels = descriptions)
  } else {
    dat_c <- data.table(feature = character(0))
  }

  # resolve y limits
  y_limits <- resolve_y_limits(dat_c = dat_c, dat_d = dat_d, facet_scales = facet_spec$scales)

  # resolve facet sort - need to convert to factor
  dat_c[, feature := factor(feature, levels = names(sculptures[[1]])[idx_continuous_vars])]
  dat_d[, feature := factor(feature, levels = names(sculptures[[1]])[!idx_continuous_vars])]

  # resolve missings specification
  rms <- resolve_missings_specification(dat_c = dat_c, ms = missings_spec, missings = missings)
  dat_c <- rms$dat_c
  missings <- rms$missings

  colours <- structure(
    ms_color(length(sculptures), hue_coloring = hue_coloring),
    names = descriptions
  )

  if (nrow(dat_c) > 0) {
    gc <- ggplot(dat_c) +
      geom_line(
        aes(
          x = .data$x,
          y = .data$y,
          colour = .data$Model,
          group = interaction(.data$feature, .data$Model)
        )
      ) +
      geom_rug(
        mapping = aes(x = .data$x, y = .data$y),
        data = dat_c[Model == descriptions[1]],
        na.rm = F,
        sides = rug_sides
      ) +
      facet_wrap(
        "feature", scales = rfs$scales, ncol = rfs$ncol_c,
        labeller = as_labeller(rfs$labels)
      ) +
      scale_color_manual(values = colours)

    # add missings lines
    if (!is.null(missings)) {
      if (missings_spec$vline) {
        gc <- gc +
          geom_vline(
            mapping = aes(xintercept = .data$x),
            data = missings,
            linetype = "dotted"
          )
      }
      if (missings_spec$hline) {
        gc <- gc +
          geom_hline(
            mapping = aes(
              yintercept = .data$y, linetype = "Score for Missing Feature",
              color = Model
            ),
            data = missings
          ) +
          scale_linetype_manual(values = c("Score for Missing Feature" = "dotted"), name = NULL)
      }
    }

    gc <- gc +
      labs(x = "Features", y = "Feature Score") +
      ylim(y_limits) +
      theme_bw()
  } else {
    gc <- NULL
  }

  if (nrow(dat_d) > 0) {
    gd <- ggplot(dat_d) +
      geom_line(
        aes(
          x = .data$x,
          y = .data$y,
          colour = .data$Model,
          group = interaction(.data$feature, .data$Model)
        )
      ) +
      facet_wrap(
        "feature", scales = rfs$scales, ncol = rfs$ncol_d,
        labeller = as_labeller(rfs$labels)
      ) +
      scale_color_manual(values = colours) +
      labs(x = "Features", y = "Feature Score") +
      ylim(y_limits) +
      theme_bw()
  } else {
    gd <- NULL
  }

  return(list(continuous = gc, discrete = gd))
}
