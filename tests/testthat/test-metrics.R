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
