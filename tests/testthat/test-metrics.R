test_that("metrics_r2 works", {
  actual <- metrics_r2(y = c(1, 2, 3), y_hat = c(0.5, 1.5, 2.5), y_hat_calib = c(0.5, 1.5, 3.5))
  expect_equal(actual, 2.25, tolerance = sqrt(.Machine$double.eps))

  actual <- metrics_r2(
    y = c(1, 2, 3), y_hat = c(0.5, 1.5, NA), y_hat_calib = c(0.5, 1.5, NA),
    na_rm = TRUE
  )
  expect_identical(actual, 1)

  actual <- metrics_r2(
    y = c(1, 2, 3), y_hat = c(0.5, 1.5, NA), y_hat_calib = c(0.5, 1.5, NA),
    na_rm = FALSE
  )
  expect_identical(actual, NA)
})

test_that("metrics_R2 works", {
  actual <- metrics_R2(score_fun = "score_quadratic", y = c(1, 2, 3), y_hat = c(0.5, 2, 3.5))
  expect_identical(actual, 0.75)

  actual <- metrics_R2(score_fun = "score_quadratic", y = c(1, 2, 3), y_hat = c(0.5, 2, NA))
  expect_identical(actual, NA_real_)

  actual <- metrics_R2(
    score_fun = "score_quadratic", y = c(1, 2, 3), y_hat = c(0.5, 2, NA),
    na_rm = TRUE
  )
  expect_identical(actual, 0.5)
})

test_that("metrics_DI works", {
  actual <- metrics_DI(score_fun = "score_quadratic", y = c(1, 2, 3), y_hat_calib = c(0.5, 2, 3.5))
  expect_identical(actual, 0.75)

  actual <- metrics_DI(score_fun = "score_quadratic", y = c(1, 2, 3), y_hat_calib = c(0.5, 2, NA))
  expect_identical(actual, NA_real_)

  actual <- metrics_DI(
    score_fun = "score_quadratic", y = c(1, 2, 3), y_hat_calib = c(0.5, 2, NA),
    na_rm = TRUE
  )
  expect_identical(actual, 0.5)
})

test_that("metrics_MI works", {
  actual <- metrics_MI(
    score_fun = "score_quadratic", y = c(1, 2, 3), y_hat = c(0.5, 2, 3.6),
    y_hat_calib = c(0.6, 2, 3.4)
  )
  expect_identical(round(actual, 3), 0.145)

  actual <- metrics_MI(
    score_fun = "score_quadratic", y = c(1, 2, 3), y_hat = c(0.5, 2, 3.5),
    y_hat_calib = c(0.6, 2, 3.4)
  )
  expect_identical(round(actual, 3), 0.09)

  actual <- metrics_MI(
    score_fun = "score_quadratic", y = c(1, 2, 3), y_hat = c(0.5, 2, NA),
    y_hat_calib = c(0.6, 2, NA)
  )
  expect_identical(actual, NA_real_)

  actual <- metrics_MI(
    score_fun = "score_quadratic", y = c(1, 2, 3), y_hat = c(0.5, 2, NA),
    y_hat_calib = c(0.6, 2, NA), na_rm = TRUE
  )
  expect_identical(round(actual, 3), 0.18)
})

test_that("metrics_fit_calib works", {
  set.seed(132)
  mod1 <- metrics_fit_calib(y = rnorm(20), y_hat = rnorm(20))
  expect_true(inherits(mod1, "gam"))
  expect_equal(mod1$smooth[[1]]$bs.dim, 10)

  mod2 <- metrics_fit_calib(y = c(1, 2, 3), y_hat = c(0.5, 2, 3.5))
  expect_true(inherits(mod2, "gam"))
  expect_equal(mod2$smooth[[1]]$bs.dim, 3)

  mod3 <- metrics_fit_calib(y = c(1, 2), y_hat = c(0.5, 2))
  expect_true(inherits(mod3, "gam"))
  expect_identical(mod3$smooth, list())
})

test_that("calc_dir_var_imp_pdp and calc_cumul_R2_pdp compute expected values", {
  dt <- data.table::data.table(
    rn = c(1, 2, 1, 2),
    feature = factor(c("a", "a", "b", "b"), levels = c("a", "b")),
    pdp_c = c(1, 3, 2, 5)
  )

  var_tbl <- modsculpt:::calc_dir_var_imp_pdp(dt)
  var_total <- stats::var(dt[, .(total = sum(pdp_c)), by = rn]$total)
  expect_equal(as.character(var_tbl$feature), c("b", "a"))
  expect_equal(var_tbl$variance, c(4.5, 2))
  expect_equal(unique(var_tbl$variance_total), var_total)
  expect_equal(var_tbl$ratio, var_tbl$variance / var_total)

  feat_order <- c("a", "b")
  model_predictions <- c(6, 10)
  res <- modsculpt:::calc_cumul_R2_pdp(
    dt = dt,
    feat_order = feat_order,
    model_predictions = model_predictions,
    model_offset = 0
  )

  expect_equal(
    res$R2,
    c(
      metrics_R2("score_quadratic", model_predictions, c(1, 3)),
      metrics_R2("score_quadratic", model_predictions, c(3, 8))
    )
  )
})

test_that("score functions handle NA removal and scalar predictions", {
  y <- c(0, 1, NA)
  y_hat <- c(0.2, 0.8, NA)
  expect_true(is.na(score_log_loss(y, y_hat)))
  expect_equal(
    score_log_loss(y, y_hat, na_rm = TRUE),
    -mean(y[1:2] * log(y_hat[1:2]) + (1 - y[1:2]) * log(1 - y_hat[1:2]))
  )

  expect_equal(
    score_log_loss(c(0, 1), 0.5),
    -mean(c(0 * log(0.5) + 1 * log(0.5), 1 * log(0.5) + 0 * log(0.5)))
  )

  expect_true(is.na(score_quadratic(y, y_hat)))
  expect_equal(score_quadratic(y, y_hat, na_rm = TRUE), mean((c(0, 1) - c(0.2, 0.8))^2))
  expect_equal(score_quadratic(c(0, 1), 0.5), mean((c(0, 1) - 0.5)^2))
})

test_that("check_score_fun validates user-defined scoring functions", {
  bad_fun <- function(y, y_hat) y
  expect_error(check_score_fun(bad_fun), "arguments")

  good_fun <- function(y, y_hat, na_rm) mean(abs(y - y_hat))
  expect_silent(check_score_fun(good_fun))
  expect_silent(check_score_fun("score_quadratic"))
})

test_that("metrics_unc and fit calibration cover NA and factor paths", {
  y <- c(1, 2, NA, 4)
  expect_true(is.na(metrics_unc("score_quadratic", y)))
  expect_equal(metrics_unc("score_quadratic", y, na_rm = TRUE),
               score_quadratic(y[!is.na(y)], rep(mean(y[!is.na(y)]), 3)))

  y_factor <- factor(c(0, 1, 1, 0), levels = c(0, 1))
  y_hat <- c(0.1, 0.8, 0.6, 0.4)
  mod <- suppressWarnings(metrics_fit_calib(y_factor, y_hat))
  expect_true(inherits(mod, "gam"))
})

test_that("metrics_r2 returns zero when response variance is zero", {
  expect_equal(
    metrics_r2(
      y = c(2, 2, 2),
      y_hat = c(1, 1, 1),
      y_hat_calib = c(1, 1, 1)
    ),
    0
  )
})

test_that("high-level importance helpers work with sculptures", {
  wf <- build_regression_workflow()
  vi_attr <- calc_dir_var_imp(wf$rough)
  expect_true(inherits(vi_attr, "data.table"))

  vi_new <- calc_dir_var_imp(wf$rough, newdata = wf$train[wf$features])
  expect_true(inherits(vi_new, "data.table"))
  expect_true(all(vi_new$variance >= 0))

  cumul_attr <- calc_cumul_R2(wf$rough)
  expect_true(inherits(cumul_attr, "data.table"))

  cumul_new <- calc_cumul_R2(wf$rough, newdata = wf$train[wf$features])
  expect_true(inherits(cumul_new, "data.table"))
  expect_true(all(cumul_new$R2 <= 1))
})
