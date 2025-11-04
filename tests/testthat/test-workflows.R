test_that("regression workflow produces consistent sculptures", {
  wf <- build_regression_workflow()
  features <- wf$features

  expect_s3_class(wf$rough, "rough")
  expect_s3_class(wf$detailed, "detailed")
  expect_true(all(names(wf$polished) %in% features))

  holdout_x <- wf$holdout[features]
  preds_rough <- predict(wf$rough, holdout_x)
  preds_detailed <- predict(wf$detailed, holdout_x)
  preds_polished <- predict(wf$polished, holdout_x[names(wf$polished)])

  expect_length(preds_rough, nrow(holdout_x))
  expect_length(preds_polished, nrow(holdout_x))
  expect_false(anyNA(preds_polished))

  actual <- wf$holdout$hp
  expect_true(is.finite(metrics_R2("score_quadratic", actual, preds_rough)))
  expect_true(is.finite(metrics_R2("score_quadratic", actual, preds_detailed)))

  add_plot <- g_additivity(
    sp = list(Rough = predict(wf$rough, wf$train[features]),
              Polished = predict(wf$polished, wf$train[names(wf$polished)])),
    lp = list(predict(wf$model, newdata = wf$train[features]),
              predict(wf$model, newdata = wf$train[features])),
    descriptions = c("Rough", "Polished"),
    plot_only = FALSE
  )
  expect_s3_class(add_plot$plot, "ggplot")
  expect_named(add_plot$R2, c("description", "R2"))

  comp <- g_comparison(
    sculptures = list(wf$rough, wf$detailed, wf$polished),
    descriptions = c("Rough", "Detailed", "Polished")
  )
  expect_s3_class(comp$continuous, "ggplot")
})

test_that("classification workflow converts log-odds to probability", {
  wf <- build_classification_workflow()
  features <- wf$features
  holdout_x <- wf$holdout[features]

  preds_rough <- predict(wf$rough, holdout_x)
  prob_rough <- modsculpt:::inv.logit(preds_rough)
  expect_true(all(prob_rough >= 0 & prob_rough <= 1))

  prob_polished <- modsculpt:::inv.logit(predict(wf$polished, holdout_x[names(wf$polished)]))
  expect_true(all(prob_polished >= 0 & prob_polished <= 1))

  var_imp_plot <- g_var_imp(wf$rough, show_pdp_plot = FALSE, logodds_to_prob = TRUE)
  expect_s3_class(var_imp_plot, "gtable")
})

test_that("comparison requires subordinate sculptures", {
  base_sc <- fixture_sculpture()
  discrete_flow <- build_discrete_rough()

  expect_error(
    g_comparison(
      sculptures = list(base_sc, discrete_flow$rough),
      descriptions = c("Base", "Discrete")
    ),
    "subsets"
  )
})

test_that("g_ice handles drop-from-plot and discrete-only input", {
  wf <- build_regression_workflow()
  miss_spec <- missings_specification(values = wf$train$mpg[1], drop_from_plot = TRUE)
  gi <- g_ice(wf$rough, missings_spec = miss_spec)
  expect_s3_class(gi$continuous, "ggplot")

  discrete <- build_discrete_rough()
  gi_disc <- g_ice(discrete$rough)
  expect_null(gi_disc$continuous)
  expect_s3_class(gi_disc$discrete, "ggplot")
})

test_that("g_component returns expected panels", {
  wf <- build_regression_workflow()
  comp <- g_component(wf$polished)
  expect_null(comp$discrete)
  expect_s3_class(comp$continuous, "ggplot")

  discrete <- build_discrete_rough()
  comp_disc <- g_component(discrete$rough)
  expect_null(comp_disc$continuous)
  expect_s3_class(comp_disc$discrete, "ggplot")
})
