
df <- mtcars
df$cyl <- as.factor(df$cyl)
model <- lm(hp ~ ., data = df)
model_predict <- function(x) predict(model, newdata = x)
covariates <- setdiff(colnames(df), "hp")
pm <- sample_marginals(df[covariates], n = 50, seed = 5)

rs <- sculpt_rough(
  dat = pm,
  model_predict_fun = model_predict,
  n_ice = 5,
  seed = 1,
  verbose = 0
)

test_that("check density and quantile calculations", {
  object <- rs
  new_data <- pm
  var_name = "drat"

  new_data_with_pred <- new_data
  new_data_with_pred$pred <- predict(rs, newdata = new_data)

  pred_ice_qtile <- calc_ice_quantile(
    object, new_data,
    var_name = var_name
  )

  density_data <- calc_density(
    new_data_with_pred,
    var_name = var_name,
    vec_y_expand = pred_ice_qtile$pred
  )


  expect_snapshot(pred_ice_qtile)
  expect_snapshot(head(density_data, 400L))
})

test_that("calc_ice_quantile converts to probabilities for classification task", {
  sc <- fixture_sculpture()
  new_data <- fixture_newdata()

  preds <- calc_ice_quantile(
    object = sc,
    new_data = new_data,
    var_name = "mpg",
    qtiles = c(0, 0.5, 1),
    task = "classification"
  )

  expect_true(all(preds$pred >= 0 & preds$pred <= 1))
  expect_equal(unique(preds$qtile), c(0, 0.5, 1))
})

test_that("calc_density handles degenerate bandwidths", {
  new_data_with_pred <- data.frame(
    x = c(rep(5, 5), 5.0001),
    pred = c(rep(1, 5), 1.0001)
  )

  density_data <- calc_density(new_data_with_pred, var_name = "x")

  expect_s3_class(density_data, "data.frame")
  expect_true(any(density_data$z == 0))
  expect_false(any(is.na(density_data$z)))
})

test_that("expand_range supports absolute adjustments", {
  expanded <- expand_range(
    x = c(10, 15),
    expand_left_side = 2,
    expand_right_side = 3,
    type = "absolute"
  )
  expect_equal(expanded, c(8, 18))
})

test_that("g_density_ice_plot variants build plots", {
  sc <- fixture_sculpture()
  new_data <- fixture_newdata()

  plot_single <- g_density_ice_plot(
    object = sc,
    new_data = new_data,
    var_name = "mpg",
    qtiles = c(0, 0.5, 1)
  )
  expect_s3_class(plot_single, "ggplot")

  plot_list <- g_density_ice_plot_list(
    object = sc,
    new_data = new_data,
    var_names = c("mpg", "wt"),
    qtiles = c(0, 0.5, 1)
  )
  expect_type(plot_list, "list")
  expect_length(plot_list, 2)
  expect_true(all(vapply(plot_list, inherits, logical(1), what = "ggplot")))
})
