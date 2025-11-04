test_that("density plot encodes expected surfaces and annotations", {
  sc <- fixture_sculpture()
  new_data <- fixture_newdata()

  density_plot <- g_density_ice_plot(
    object = sc,
    new_data = new_data,
    var_name = "mpg",
    qtiles = c(0, 0.5, 1)
  )

  build <- ggplot2::ggplot_build(density_plot)

  raster_range <- round(range(build$data[[1]]$y), 4)
  ice_points <- lapply(seq_len(3), function(i) {
    vals <- round(as.numeric(build$data[[2]][i, c("x", "y")]), 4)
    stats::setNames(vals, c("x", "y"))
  })
  label_positions <- round(build$data[[4]]$y, 4)

  density_summary <- list(
    raster_range = raster_range,
    ice_head = ice_points,
    annotation_y = label_positions
  )

  expect_snapshot_value(density_summary, style = "json2")
})

test_that("variable importance plots expose stable numeric payloads", {
  sc <- fixture_sculpture()
  dat_var <- attr(sc, "var_imp")
  dat_r2 <- attr(sc, "cumul_R2")

  norm_plot <- modsculpt:::g_imp_norm(dat_var, show_pdp_plot = FALSE, textsize = 14)
  norm_build <- ggplot2::ggplot_build(norm_plot)

  norm_summary <- list(
    features = as.character(norm_build$data[[1]]$y),
    ratios = round(norm_build$data[[1]]$x, 4)
  )

  r2_plot <- modsculpt:::g_cumulR2(dat_r2, textsize = 14)
  r2_build <- ggplot2::ggplot_build(r2_plot)
  r2_summary <- list(
    features = as.character(r2_build$data[[1]]$y),
    cumul_r2 = round(r2_build$data[[1]]$x, 4)
  )

  expect_snapshot_value(
    list(normalized = norm_summary, cumulative = r2_summary),
    style = "json2"
  )
})

test_that("additivity scatter plot preserves learner-alignment statistics", {
  wf <- build_regression_workflow(seed = 99)
  train_x <- wf$train[wf$features]

  add_plot <- g_additivity(
    sp = list(
      Rough = predict(wf$rough, train_x),
      Polished = predict(wf$polished, train_x[names(wf$polished)])
    ),
    lp = list(
      predict(wf$model, newdata = train_x),
      predict(wf$model, newdata = train_x)
    ),
    descriptions = c("Rough", "Polished"),
    plot_only = FALSE
  )

  add_build <- ggplot2::ggplot_build(add_plot$plot)
  add_data <- add_build$data[[1]]
  panel_map <- setNames(
    as.character(add_build$layout$layout$PANEL),
    add_build$layout$layout$PANEL
  )
  correlations <- vapply(
    split(add_data, add_data$PANEL),
    function(df) round(stats::cor(df$x, df$y), 6),
    numeric(1)
  )

  expect_snapshot_value(
    list(
      panel_cor = unname(correlations[panel_map]),
      r2_table = round(add_plot$R2$R2, 6)
    ),
    style = "json2"
  )
})

test_that("ICE plot data stays centred and ordered", {
  sc <- fixture_sculpture()
  gi <- g_ice(sc)

  cont_build <- ggplot2::ggplot_build(gi$continuous)
  ribbon <- cont_build$data[[1]]
  ice_lines <- cont_build$data[[2]]
  pdp_line <- cont_build$data[[3]]
  panel_map <- setNames(
    as.character(cont_build$layout$layout$feature),
    cont_build$layout$layout$PANEL
  )

  first_panel <- levels(ice_lines$PANEL)[1]
  first_line <- round(
    as.numeric(ice_lines[ice_lines$PANEL == first_panel, c("x", "y")][1, ]),
    4
  )
  names(first_line) <- c("x", "y")

  pdp_means <- vapply(
    split(pdp_line$y, pdp_line$PANEL),
    mean,
    numeric(1)
  )
  pdp_named <- setNames(
    round(unname(pdp_means), 4),
    panel_map[names(pdp_means)]
  )

  ice_summary <- list(
    ribbon_range = round(range(ribbon$y), 4),
    first_line = first_line,
    pdp_mean = pdp_named
  )

  expect_snapshot_value(ice_summary, style = "json2")
})

test_that("component plot encodes deterministic smoother output", {
  sc <- fixture_sculpture()
  detailed <- sculpt_detailed_lm(sc)
  comp <- g_component(detailed)

  cont_build <- ggplot2::ggplot_build(comp$continuous)
  cont_data <- cont_build$data[[1]]
  panel_map <- setNames(
    as.character(cont_build$layout$layout$feature),
    cont_build$layout$layout$PANEL
  )
  means <- vapply(
    split(cont_data$y, cont_data$PANEL),
    mean,
    numeric(1)
  )

  component_summary <- list(
    feature = unname(panel_map[names(means)]),
    mean_score = round(unname(means), 4)
  )

  expect_snapshot_value(component_summary, style = "json2")
})

test_that("classification calibration and predictiveness metrics remain stable", {
  skip_if_not_installed("mgcv")

  wf <- build_classification_workflow(seed = 303)
  holdout_x <- wf$holdout[wf$features]
  truth <- as.numeric(as.character(wf$holdout$am))

  logits <- predict(wf$rough, holdout_x)
  prob <- modsculpt:::inv.logit(logits)
  eps <- 1e-6
  prob_adj <- pmin(pmax(prob, eps), 1 - eps)

  calib_model <- metrics_fit_calib(y = truth, y_hat = prob_adj)
  calib_pred <- predict(calib_model, type = "response")
  calib_adj <- pmin(pmax(calib_pred, eps), 1 - eps)

  calibration_metrics <- list(
    calib_head = round(calib_adj[1:6], 6),
    di = round(metrics_DI("score_log_loss", truth, calib_adj), 6),
    mi = round(metrics_MI("score_log_loss", truth, prob_adj, calib_adj), 6),
    r2 = round(metrics_R2("score_log_loss", truth, prob_adj), 6)
  )

  expect_snapshot_value(calibration_metrics, style = "json2")
})
