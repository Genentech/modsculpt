skip_on_cran()

test_that("g_pdp samples large PDP tables for quicker plotting", {
  dt <- data.table::data.table(
    feature = factor(rep(c("mpg", "wt"), each = 25050), levels = c("mpg", "wt")),
    pdp_c = rnorm(50100)
  )
  labels <- c(mpg = "MPG", wt = "Weight")

  gp <- modsculpt:::g_pdp(dt, pdp_plot_sample = TRUE, feat_labels = labels)
  expect_s3_class(gp, "ggplot")
})

test_that("g_imp_abs positions labels relative to leading variance", {
  dat_var <- data.table::data.table(
    feature = factor(c("a", "b"), levels = c("a", "b")),
    variance = c(1, 5)
  )

  ga <- modsculpt:::g_imp_abs(dat_var, show_pdp_plot = FALSE, textsize = 12)
  expect_s3_class(ga, "ggplot")
  expect_true(all(dat_var$variance_vs_top <= 1))
})
