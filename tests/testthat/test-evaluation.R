test_that("eval_sculpture returns PDP and predictions for regression", {
  wf <- build_regression_workflow()
  res <- modsculpt:::eval_sculpture(wf$rough, wf$train[wf$features])

  expect_true(inherits(res$pdp, "data.table"))
  expect_named(res$prediction, c("rn", "pred"))
  expect_equal(nrow(res$prediction), nrow(wf$train))
  expect_true(all(is.finite(res$prediction$pred)))
})

test_that("eval_sculpture handles classification log-odds", {
  wf <- build_classification_workflow()
  res <- modsculpt:::eval_sculpture(wf$rough, wf$train[wf$features])

  expect_true(inherits(res$pdp, "data.table"))
  expect_equal(nrow(res$prediction), nrow(wf$train))
  expect_true(any(res$prediction$pred < 0))
})
