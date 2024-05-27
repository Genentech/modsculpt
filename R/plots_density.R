#' Create ICE curves at quantiles
#' @keywords internal
#'
#' @param object Object of class sculpture (rough, detailed)
#' @param new_data Data to make quantiles on
#' @param var_name String specifying which variable to generate ICE
#' @param qtiles Quantiles to generate ICE curves
#' @param task Prediction task type (regression or classification)
#'
#' @return Predictions
#'
#' @details
#' It should be amenable to any 1st-order model without interaction terms,
#' however not implemented yet, such as handling `predict()` function output
#' for binary endpoint
#'
calc_ice_quantile <- function(object, new_data, var_name, qtiles = seq(0, 1, by = 0.1),
                              task = "regression") {
  checkmate::assert_class(object, "sculpture")
  match.arg(task, c("regression", "classification"))

  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  median <- quantile <- rgb <- NULL # due to NSE notes in R CMD check

  # Predict for all samples after replacing the variable with 1st value,
  # then take quantiles
  cov_1st_val <- new_data[[var_name]][1]

  new_data_with_1st_val <- new_data
  new_data_with_1st_val[[var_name]] <- cov_1st_val

  pred_at_1st_val <- predict(object, new_data_with_1st_val)

  pred_qtile_at_1st_qtiles <- quantile(pred_at_1st_val, qtiles)

  pred_at_1st_qtiles <- data.frame(pred_at_1st = pred_qtile_at_1st_qtiles, qtile = qtiles)

  # Separately predict for all values of the cov of interest
  preds_for_adjust_1 <- merge(
    new_data[1, setdiff(colnames(new_data), var_name)],
    unique(new_data[var_name])
  )
  preds_for_adjust_1$pred <- predict(object, newdata = preds_for_adjust_1)


  # Get the pred at the first element, because
  # it is what was selected for cov_1st_val
  pred_for_adjust_at_1st <- preds_for_adjust_1$pred[1]

  preds_for_adjust <- preds_for_adjust_1[var_name]
  preds_for_adjust$pred_adjust <- preds_for_adjust_1$pred - pred_for_adjust_at_1st

  # Combine the above 2 to make quantile lines
  pred_ice_qtile <- merge(
    pred_at_1st_qtiles,
    preds_for_adjust
  )
  pred_ice_qtile$pred <- pred_ice_qtile$pred_at_1st + pred_ice_qtile$pred_adjust

  # Convert to probabilities if classification
  if (task == "classification") {
    pred_ice_qtile$pred <- inv.logit(pred_ice_qtile$pred)
  }

  return(pred_ice_qtile)
}


#' Create density curves
#' @keywords internal
#'
#' @param new_data_with_pred Data with prediction to make density calculations on
#' @param var_name String specifying which variable to calculate density
#' @param vec_y_expand Optional values to expand y-axis
#' @return Density data for plotting
#'
#' @details
#' It should be amenable to any 1st-order model without interaction terms,
#' however not implemented yet, such as handling `predict()` function output
#' for binary endpoint
#'
calc_density <- function(new_data_with_pred, var_name,
                         vec_y_expand = NULL) {
  x_axis_range_data <- range(new_data_with_pred[[var_name]])
  x_axis_range_density <- expand_range(x_axis_range_data, 0.5, 0.5)


  y_axis_range_data <- range(c(new_data_with_pred$pred, vec_y_expand))
  y_axis_range_density <- expand_range(y_axis_range_data, 0.1, 0.1)


  # Estimate 2d density
  # Calculate bandwidth manually if MASS::bandwidth.nrd fails
  # (happens when most of data has same covariate, e.g. ==0.
  # MASS::bandwidth.nrd uses quantiles to calculate bandwidth)
  bandwidth_x <- MASS::bandwidth.nrd(new_data_with_pred[[var_name]])
  bandwidth_y <- MASS::bandwidth.nrd(new_data_with_pred$pred)
  # If bandwidth is 0, set to 25% of range
  bandwidth_x <- ifelse(bandwidth_x == 0, diff(x_axis_range_data) * 0.25, bandwidth_x)
  bandwidth_y <- ifelse(bandwidth_y == 0, diff(y_axis_range_data) * 0.25, bandwidth_y)

  density_est <- MASS::kde2d(
    x = new_data_with_pred[[var_name]],
    y = new_data_with_pred$pred,
    n = 100,
    lims = c(x_axis_range_density, y_axis_range_density),
    h = c(bandwidth_x, bandwidth_y)
  )

  # Convert to data frame
  density_data <- expand.grid(x = density_est$x, y = density_est$y)
  density_data$z <- as.vector(density_est$z)
  density_data <- density_data[order(density_data$x, density_data$y), ]

  # Dummy data to make legend go down to 0.0, replace 1st row z value with 0
  density_data[1, 3] <- 0

  return(density_data)
}


#' Expand the range of values for density plot
#' @keywords internal
#'
#' @param x numeric vector
#' @param expand_left_side Fraction to expand on left hand side
#' @param expand_right_side Fraction to expand on right hand side
#'
#' @return Vector of 2 values
#'
#'
expand_range <- function(x, expand_left_side = 0.1, expand_right_side = 0.2,
                         type = c("relative", "absolute")) {
  type <- match.arg(type)

  if (type == "relative") {
    expand_left_side <- expand_left_side * diff(range(x))
    expand_right_side <- expand_right_side * diff(range(x))
  }

  return(c(min(x) - expand_left_side, max(x) + expand_right_side))
}

#' Density plots overlaid with ICE curves
#'
#' Create density plot for the data, overlaid with ICE curves at quantiles
#' of the variable(s) of interest.
#'
#'
#' @name g_density_ice
NULL


#' @rdname g_density_ice
#' @export
#'
#' @param object Object of class sculpture (rough, detailed)
#' @param new_data Data to make quantiles on
#' @param var_name String specifying which variable to generate ICE
#' @param var_label String (optional) specifying variable label (x label of the plot)
#' @param qtiles Quantiles to generate ICE curves
#' @param task Prediction task type (regression or classification)
#'
#' @return [g_density_ice_plot()]: ggplot object
#'
#' @details
#' [g_density_ice_plot()] creates a density plot for a single variable.
#'
#' [g_density_ice_plot_list()] creates a list of density plots for multiple variables.
#'
#' These functions should be amenable to any 1st-order model without interaction terms,
#' however not implemented yet, such as handling `predict()` function output
#' for binary endpoint
#'
#'
#' @examples
#' \dontrun{
#' df <- mtcars
#' df$cyl <- as.factor(df$cyl)
#' model <- lm(hp ~ ., data = df)
#' model_predict <- function(x) predict(model, newdata = x)
#' covariates <- setdiff(colnames(df), "hp")
#' pm <- sample_marginals(df[covariates], n = 50, seed = 5)
#'
#' rs <- sculpt_rough(
#'   dat = pm,
#'   model_predict_fun = model_predict,
#'   n_ice = 5,
#'   seed = 1,
#'   verbose = 0
#' )
#'
#' g_density_ice_plot(rs, new_data = pm, var_name = "mpg")
#' g_list <- g_density_ice_plot_list(
#'   rs, new_data = pm, var_names = c("mpg", "cyl", "disp", "drat")
#' )
#' grid::grid.draw(gridExtra::arrangeGrob(grobs = g_list))
#' }
#'
g_density_ice_plot <- function(object, new_data, var_name, var_label = NULL,
                               qtiles = seq(0, 1, by = 0.1),
                               task = c("regression", "classification")) {
  checkmate::assert_class(object, "sculpture")
  new_data <- check_data(new_data)
  checkmate::assert_string(var_name)
  checkmate::assert_string(var_label, null.ok = TRUE)
  checkmate::assert_numeric(qtiles, lower = 0, upper = 1)
  checkmate::assert_character(task)

  task <- match.arg(task)

  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  x <- y <- z <- pred <- qtile <- NULL # due to NSE notes in R CMD check

  if (is.null(var_label)) {
    var_label <- var_name
  }

  is_var_discrete <- !is.numeric(new_data[[var_name]])

  new_data_with_pred <- new_data
  new_data_with_pred$pred <- predict(object, newdata = new_data)

  if (task == "classification") {
    new_data_with_pred$pred <- inv.logit(new_data_with_pred$pred)
  }

  pred_ice_qtile <- calc_ice_quantile(
    object, new_data,
    var_name = var_name, qtiles = qtiles, task = task
  )

  if (is_var_discrete) {
    new_data_with_pred[[var_name]] <- as.numeric(as.factor(new_data_with_pred[[var_name]]))
    pred_ice_qtile[[var_name]] <- as.numeric(as.factor(pred_ice_qtile[[var_name]]))
  }

  density_data <- calc_density(
    new_data_with_pred,
    var_name = var_name,
    vec_y_expand = pred_ice_qtile$pred
  )

  x_axis_range_data <- range(new_data_with_pred[[var_name]])
  if (is_var_discrete) {
    x_axis_range_plot <- expand_range(x_axis_range_data, 0.3, 0.3, type = "absolute")
  } else {
    x_axis_range_plot <- expand_range(x_axis_range_data, 0, 0.15)
  }
  y_axis_range_plot <- range(c(new_data_with_pred$pred, pred_ice_qtile$pred))

  ggrepel_data <- pred_ice_qtile[
    pred_ice_qtile$qtile %in% c(0, 0.5, 1) &
      pred_ice_qtile[[var_name]] == max(pred_ice_qtile[[var_name]]),
  ]

  density_plot <- ggplot(density_data, aes(x = x, y = y)) +
    geom_raster(aes(fill = z), interpolate = TRUE) +
    labs(x = var_label, y = "Predicted Value", fill = "Density") +
    scale_fill_viridis_c() +
    coord_cartesian(xlim = x_axis_range_plot, ylim = y_axis_range_plot) +
    theme(
      panel.ontop = TRUE,
      panel.background = element_rect(color = NA, fill = NA),
      panel.grid.major = element_line(color = grDevices::rgb(1, 1, 1, 0.1)),
      panel.grid.minor = element_line(color = grDevices::rgb(1, 1, 1, 0.1))
    ) +
    geom_line(
      data = pred_ice_qtile,
      aes(x = .data[[var_name]], y = pred, group = qtile), linewidth = 0.3,
      color = "grey70"
    ) +
    geom_line(
      data = pred_ice_qtile[pred_ice_qtile$qtile %in% c(0, 0.5, 1), ],
      aes(x = .data[[var_name]], y = pred, group = qtile), linewidth = 0.7,
      color = "grey70"
    ) +
    ggrepel::geom_text_repel(
      data = ggrepel_data,
      aes(x = .data[[var_name]], y = pred, label = paste0(round(qtile * 100), "%")),
      color = "grey70",
      box.padding = unit(0.25, "lines"),
      point.padding = unit(0.25, "lines"),
      segment.linetype = "dotted",
      min.segment.length = unit(0, "lines"),
      nudge_x = diff(range(x_axis_range_plot)) / 6,
      direction = "y", hjust = "right"
    )


  if (is_var_discrete) {
    levels <- levels(as.factor(new_data[[var_name]]))
    density_plot <- density_plot +
      scale_x_continuous(
        breaks = seq_len(length(levels)),
        labels = levels,
        minor_breaks = NULL
      )
  }

  return(density_plot)
}


#' @rdname g_density_ice
#' @export
#'
#' @param var_names Vector of strings specifying which variables to generate ICE
#' @param var_labels Named vector of strings specifying variable labels.
#'
#' @return [g_density_ice_plot_list()]: list of ggplot objects
#'
g_density_ice_plot_list <- function(object, new_data, var_names, var_labels = NULL,
                                    qtiles = seq(0, 1, by = 0.1),
                                    task = c("regression", "classification")) {
  checkmate::assert_class(object, "sculpture")
  new_data <- check_data(new_data)
  checkmate::assert_character(var_names)
  checkmate::assert_character(var_labels, null.ok = TRUE)
  checkmate::assert_numeric(qtiles, lower = 0, upper = 1)
  checkmate::assert_character(task)

  task <- match.arg(task)

  out <- vector("list", length(var_names))
  names(out) <- var_names

  for (var_name in var_names) {
    out[[var_name]] <-
      g_density_ice_plot(object, new_data, var_name, var_labels[var_name], qtiles, task)
  }

  return(out)
}
