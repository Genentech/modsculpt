
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
