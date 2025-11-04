
test_that("sample_marginals works as expected", {
  actual <- sample_marginals(mtcars, n = 5, seed = 1)
  expect_equal(actual$cyl, c(8, 8, 6, 8, 4))
  expect_equal(actual$vs, c(0, 1, 1, 1, 1))
  expect_equal(actual$am, c(0, 0, 0, 1, 1))

  df <- sample_marginals(
    dat = mtcars,
    n = 1e4,
    seed = 1
  )

  actual <- vapply(df, mean, numeric(1))
  expect_equal(
    round(actual, 3),
    c(20.125, 6.221, 231.279, 146.336, 3.598, 3.207, 17.867, 0.437, 0.406, 3.699, 2.829),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  actual <- vapply(df, var, numeric(1))
  expect_equal(
    actual,
    c(35.815, 3.061, 14943.95, 4665.924, 0.273, 0.922, 3.068, 0.246, 0.241, 0.522, 2.596),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  expect_equal(dim(df), c(1e4, ncol(mtcars)))

  expect_equal(colnames(df), colnames(mtcars))
})


test_that("calculate_ice_data and generate_ice_data works as expected", {
  df <- mtcars
  df$cyl <- as.factor(df$cyl)
  model <- lm(hp ~ ., data = df)
  model_predict <- function(x) predict(model, newdata = x)
  covariates <- setdiff(colnames(df), "hp")

  # for mpg - continuous
  subsets <- sample_marginals(
    dat = df[setdiff(covariates, "mpg")],
    n = 5,
    seed = 1
  )
  ice_data <- calculate_ice_data(
    sub = subsets,
    predict_fun = model_predict,
    x = df$mpg,
    x_name = "mpg",
    col_order = covariates
  )

  expect_snapshot(as.data.frame(ice_data))

  predictions <- split(ice_data$ice_centered, ice_data$line_id)
  ice_data_gen <- generate_ice_data(predictions = predictions, x = df$mpg)
  setnames(ice_data_gen, old = "y", new = "ice_centered")
  expect_equal(ice_data[, c("x", "ice_centered", "line_id")], ice_data_gen)

  # for cyl - discrete
  subsets <- sample_marginals(
    dat = df[setdiff(covariates, "cyl")],
    n = 5,
    seed = 1
  )
  ice_data <- calculate_ice_data(
    sub = subsets,
    predict_fun = model_predict,
    x = df$cyl,
    x_name = "cyl",
    col_order = covariates
  )

  expect_snapshot(as.data.frame(ice_data))

  predictions <- split(ice_data$ice_centered, ice_data$line_id)
  ice_data_gen <- generate_ice_data(predictions = predictions, x = df$cyl)
  setnames(ice_data_gen, old = "y", new = "ice_centered")
  expect_equal(ice_data[, c("x", "ice_centered", "line_id")], ice_data_gen)
})


test_that("calculate_pdp_data and generate_pdp_data works as expected", {
  df <- mtcars
  df$cyl <- as.factor(df$cyl)
  model <- lm(hp ~ ., data = df)
  model_predict <- function(x) predict(model, newdata = x)
  covariates <- setdiff(colnames(df), "hp")

  # for mpg - continuous
  subsets <- sample_marginals(
    dat = df[setdiff(covariates, "mpg")],
    n = 5,
    seed = 1
  )
  ice_data <- calculate_ice_data(
    sub = subsets,
    predict_fun = model_predict,
    x = df$mpg,
    x_name = "mpg",
    col_order = covariates
  )
  pdp_data <- calculate_pdp_data(ice_data)

  expect_snapshot(as.data.frame(pdp_data[, .(x, pdp_centered)]))

  pdp_data_gen <- generate_pdp_data(
    predictions = split(ice_data$ice_centered, ice_data$line_id),
    x = df$mpg,
  )
  setnames(pdp_data_gen, old = c("y", "y_se"), new = c("pdp_centered", "pdp_centered_se"))
  expect_equal(pdp_data, pdp_data_gen)

  # for cyl - discrete
  subsets <- sample_marginals(
    dat = df[setdiff(covariates, "cyl")],
    n = 5,
    seed = 1
  )
  ice_data <- calculate_ice_data(
    sub = subsets,
    predict_fun = model_predict,
    x = df$cyl,
    x_name = "cyl",
    col_order = covariates
  )
  pdp_data <- calculate_pdp_data(ice_data)

  expect_snapshot(as.data.frame(pdp_data[, .(x, pdp_centered)]))

  pdp_data_gen <- generate_pdp_data(
    predictions = split(ice_data$ice_centered, ice_data$line_id),
    x = df$cyl,
  )
  setnames(pdp_data_gen, old = c("y", "y_se"), new = c("pdp_centered", "pdp_centered_se"))
  expect_equal(pdp_data, pdp_data_gen)
})


test_that("generate_ice_data and generate_pdp_data with logodds_to_prob works as expected", {
  df <- mtcars
  df$vs <- as.factor(df$vs)
  model <- glm(vs ~ hp + mpg, data = df, family = "binomial")
  model_predict <- function(x) predict(model, newdata = x, type = "link")
  covariates <- c("hp", "mpg")

  # for mpg - continuous - calculated ice and pdp
  subsets <- sample_marginals(
    dat = df[setdiff(covariates, "mpg")],
    n = 5,
    seed = 1
  )
  ice_data <- calculate_ice_data(
    sub = subsets,
    predict_fun = model_predict,
    x = df$mpg,
    x_name = "mpg",
    col_order = covariates
  )
  pdp_data <- calculate_pdp_data(ice_data)

  # generated ice
  predictions <- split(ice_data$ice_centered, ice_data$line_id)
  ice_data_gen <- generate_ice_data(predictions = predictions, x = df$mpg, logodds_to_prob = TRUE)
  setnames(ice_data_gen, old = "y", new = "ice_centered")
  ice_data$ice_centered <- inv.logit(ice_data$ice_centered)
  expect_equal(ice_data[, c("x", "ice_centered", "line_id")], ice_data_gen)

  # generated pdp
  pdp_data_gen <- generate_pdp_data(predictions = predictions, x = df$mpg, logodds_to_prob = TRUE)
  setnames(pdp_data_gen, old = c("y", "y_se"), new = c("pdp_centered", "pdp_centered_se"))
  pdp_data$pdp_centered <- inv.logit(pdp_data$pdp_centered)
  expect_equal(pdp_data, pdp_data_gen)
})



test_that("sculpt_rough works as expected", {
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

  expect_s3_class(rs, "rough")
  expect_s3_class(rs, "sculpture")
  expect_equal(length(rs), 10)
  expect_equal(attr(rs, "offset"), 139.24, tolerance = 1e-2)

  # continuous variable
  expect_snapshot(rs$mpg$subsets)
  expect_snapshot(rs$mpg$ice_centered)
  expect_snapshot(rs$mpg$predict(c(15, NA, -5, 1e5)))
  expect_false(rs$mpg$is_discrete)
  expect_equal(rs$mpg$x, pm$mpg)
  expect_equal(rs$mpg$x_name, "mpg")

  # discrete variable
  expect_snapshot(rs$cyl$subsets)
  expect_snapshot(rs$cyl$ice_centered)
  expect_snapshot(rs$cyl$predict(c(4, NA, -5)))
  expect_true(rs$cyl$is_discrete)
  expect_equal(rs$cyl$x, pm$cyl)
  expect_equal(rs$cyl$x_name, "cyl")

  expect_snapshot(predict(rs))
})


test_that("sculpt_rough works with 1/2/3 variable(-s)", {
  # with 1 variable
  df <- mtcars
  model <- lm(hp ~ mpg, data = df)
  model_predict <- function(x) predict(model, newdata = x)
  covariates <- "mpg"
  pm <- sample_marginals(df[covariates], n = 50, seed = 7)

  rs <- sculpt_rough(
    dat = pm,
    model_predict_fun = model_predict,
    n_ice = 5,
    seed = 1,
    verbose = 0
  )

  expect_s3_class(rs, "rough")
  expect_s3_class(rs, "sculpture")
  expect_equal(length(rs), 1)
  expect_equal(attr(rs, "offset"), 143.51, tolerance = 1e-2)

  # continuous variable
  expect_equal(rs$mpg$subsets, NULL)
  expect_snapshot(rs$mpg$ice_centered)
  expect_snapshot(rs$mpg$predict(c(15, NA, -5, 1e5)))
  expect_false(rs$mpg$is_discrete)
  expect_equal(rs$mpg$x, pm$mpg)
  expect_equal(rs$mpg$x_name, "mpg")

  # with 2 variables
  model <- lm(hp ~ mpg + cyl, data = df)
  model_predict <- function(x) predict(model, newdata = x)
  covariates <- c("mpg", "cyl")
  pm <- sample_marginals(df[covariates], n = 50, seed = 7)

  rs <- sculpt_rough(
    dat = pm,
    model_predict_fun = model_predict,
    n_ice = 5,
    seed = 1,
    verbose = 0
  )
  expect_s3_class(rs, "rough")

  # with 3 variables
  model <- lm(hp ~ mpg + cyl + vs, data = df)
  model_predict <- function(x) predict(model, newdata = x)
  covariates <- c("mpg", "cyl", "vs")
  pm <- sample_marginals(df[covariates], n = 50, seed = 7)

  rs <- sculpt_rough(
    dat = pm,
    model_predict_fun = model_predict,
    n_ice = 5,
    seed = 1,
    verbose = 0
  )
  expect_s3_class(rs, "rough")
})


test_that("sculpt_detailed_lm works as expected", {
  df <- mtcars
  df$cyl <- as.factor(df$cyl)
  model <- lm(hp ~ ., data = df)
  model_predict <- function(x) predict(model, newdata = x)
  covariates <- setdiff(colnames(df), "hp")
  pm <- sample_marginals(df[covariates], n = 50, seed = 9)

  rs <- sculpt_rough(
    dat = pm,
    model_predict_fun = model_predict,
    n_ice = 5,
    seed = 1,
    verbose = 0
  )

  ds <- sculpt_detailed_lm(rs)

  expect_s3_class(ds, "detailed")
  expect_s3_class(ds, "sculpture")
  expect_equal(length(ds), 10)
  expect_equal(attr(ds, "offset"), 160.39, tolerance = 1e-2)

  # continuous
  expect_snapshot(ds$mpg$predict(c(15, NA, -5, 1e5)))
  expect_false(ds$mpg$is_discrete)
  expect_equal(ds$mpg$x, pm$mpg)
  expect_equal(ds$mpg$x_name, "mpg")

  # discrete
  expect_snapshot(ds$cyl$predict(factor(c("4", "6", "5"))))
  expect_error(suppressWarnings(ds$cyl$predict(1)), regexp = "Unknown value for prediction")
  expect_identical(ds$cyl$predict("1"), 0)
  expect_true(ds$cyl$is_discrete)
  expect_equal(ds$cyl$x, pm$cyl)
  expect_equal(ds$cyl$x_name, "cyl")
})


test_that("sculpt_detailed_gam works as expected", {
  df <- mtcars
  df$cyl <- as.factor(df$cyl)
  model <- lm(hp ~ ., data = df)
  model_predict <- function(x) predict(model, newdata = x)
  covariates <- setdiff(colnames(df), "hp")
  pm <- sample_marginals(df[covariates], n = 50, seed = 11)

  rs <- sculpt_rough(
    dat = pm,
    model_predict_fun = model_predict,
    n_ice = 5,
    seed = 1,
    verbose = 0
  )

  ds <- sculpt_detailed_gam(rs)

  expect_s3_class(ds, "detailed")
  expect_s3_class(ds, "sculpture")
  expect_equal(length(ds), 10)
  expect_equal(attr(ds, "offset"), 157.67, tolerance = 1e-2)

  # continuous
  expect_snapshot(ds$mpg$predict(c(15, NA, -5, 1e5)))
  expect_false(ds$mpg$is_discrete)
  expect_equal(ds$mpg$x, pm$mpg)
  expect_equal(ds$mpg$x_name, "mpg")

  # discrete
  expect_snapshot(ds$cyl$predict(factor(c("4", "6", "5"))))
  expect_identical(ds$cyl$predict(1), 0)
  expect_identical(ds$cyl$predict("1"), 0)
  expect_true(ds$cyl$is_discrete)
  expect_equal(ds$cyl$x, pm$cyl)
  expect_equal(ds$cyl$x_name, "cyl")
})


test_that("sculptures work with one variable", {
  df <- mtcars[c("hp", "mpg")]
  covariates <- "mpg"
  model <- lm(hp ~ mpg, data = df)
  model_predict <- function(x) predict(model, newdata = x)
  pm <- sample_marginals(df[covariates], n = 50, seed = 12323)

  rs <- sculpt_rough(
    dat = pm, model_predict_fun = model_predict,
    n_ice = 10, seed = 1, verbose = 0
  )
  ds <- sculpt_detailed_lm(rs)
  expect_equal(unname(round(head(predict(rs, df), 2), 2)), c(138.66, 138.66))
  expect_equal(unname(round(head(predict(ds, df), 2), 2)), c(138.66, 138.66))
})


test_that("sculptures work with missings flag - all same", {
  covariates <- c("mpg", "cyl", "disp")
  df <- mtcars[c("hp", covariates)]
  df$mpg[1] <- -1 # missing imputation / flag
  df$disp[2] <- -1 # missing imputation / flag
  df$cyl <- as.factor(df$cyl)
  model <- lm(hp ~ mpg + cyl + disp, data = df)
  model_predict <- function(x) predict(model, newdata = x)
  pm <- sample_marginals(df[covariates], n = 50, seed = 16)

  rs <- sculpt_rough(
    dat = pm, model_predict_fun = model_predict,
    n_ice = 10, seed = 1, verbose = 0
  )
  ds <- sculpt_detailed_lm(rs)
  expect_snapshot(predict(rs, df))
  expect_snapshot(predict(ds, df))

  ds <- sculpt_detailed_lm(rs, missings = -1)
  expect_snapshot(predict(ds, df))
})


test_that("sculptures work with missings flag - different per column", {
  requireNamespace("rpart", quietly = TRUE)

  covariates <- c("mpg", "cyl", "disp")
  df <- mtcars[c("hp", covariates)]
  df$mpg[1] <- -1 # missing imputation / flag
  df$disp[2] <- -2 # missing imputation / flag
  df$cyl <- as.factor(df$cyl)

  model <- rpart::rpart(hp ~ mpg + cyl + disp,
    data = df,
    control = rpart::rpart.control(minsplit = 10)
  )
  model_predict <- function(x) predict(model, newdata = x)
  pm <- sample_marginals(df[covariates], n = 50, seed = 26)

  rs <- sculpt_rough(
    dat = pm, model_predict_fun = model_predict,
    n_ice = 10, seed = 1, verbose = 0
  )

  expect_snapshot(rs$mpg$predict(-1))
  expect_snapshot(predict(rs, df))

  ds <- sculpt_detailed_lm(rs)

  expect_snapshot(predict(ds, df))
  expect_snapshot(ds$mpg$predict(-1))
  expect_snapshot(ds$mpg$predict(-1.1))

  ds2 <- sculpt_detailed_lm(rs, missings = c("mpg" = -1, "disp" = -2))

  expect_snapshot(predict(ds2, df))
  expect_snapshot(ds2$mpg$predict(-1))
  expect_snapshot(ds2$mpg$predict(-1.1))

  # check that only continuous features are allowed
  expect_error(
    sculpt_detailed_lm(rs, missings = c("mpg" = -1, "cyl" = -2)),
    regexp = "Assertion on 'missings' failed: Must be a subset of \\{'mpg','disp'\\}"
  )
})


test_that("Rough class is defined properly", {
  test_rough_var <- function(v) {
    expect_equal(length(v), 7)
    expect_identical(
      names(v),
      c("subsets", "predict", "ice_centered", "ice", "is_discrete", "x", "x_name")
    )
    expect_true(is.list(v$subsets))
    expect_true(is.list(v$ice_centered))
    expect_true(is.function(v$predict))
    expect_equal(length(formals(v$predict)), 1)
    expect_true(is.logical(v$is_discrete))
    expect_equal(length(v$is_discrete), 1)
    expect_true(is.atomic(v$is_discrete))
    expect_true(is.atomic(v$x))
    expect_true(is.character(v$x_name))
    expect_equal(length(v$x_name), 1)
  }

  test_rough <- function(x, covariates) {
    expect_s3_class(x, "rough")
    expect_s3_class(x, "sculpture")
    expect_equal(length(x), length(covariates))
    expect_identical(names(x), covariates)

    for (v in x) test_rough_var(v)
  }

  covariates <- c("mpg", "cyl", "disp")
  df <- mtcars[c("hp", covariates)]
  df$mpg[1] <- -1 # missing imputation / flag
  df$disp[2] <- -2 # missing imputation / flag
  df$cyl <- as.factor(df$cyl)

  model <- rpart::rpart(hp ~ mpg + cyl + disp,
    data = df,
    control = rpart::rpart.control(minsplit = 10)
  )
  model_predict <- function(x) predict(model, newdata = x)
  pm <- sample_marginals(df[covariates], n = 50, seed = 26)

  rs <- sculpt_rough(
    dat = pm, model_predict_fun = model_predict,
    n_ice = 10, seed = 1, verbose = 0
  )

  test_rough(rs, covariates)
})


test_that("Detailed class is defined properly", {
  test_detailed_var <- function(v) {
    expect_equal(length(v), 5)
    expect_identical(names(v), c("predict", "is_discrete", "x", "x_name", "missings_flag"))
    expect_true(is.function(v$predict))
    expect_equal(length(formals(v$predict)), 1)
    expect_true(is.logical(v$is_discrete))
    expect_equal(length(v$is_discrete), 1)
    expect_true(is.atomic(v$is_discrete))
    expect_true(is.character(v$x_name))
    expect_equal(length(v$x_name), 1)
    expect_equal(length(v$missings_flag), 0)
  }

  test_detailed <- function(x, covariates) {
    expect_s3_class(x, "detailed")
    expect_s3_class(x, "sculpture")
    expect_equal(length(x), length(covariates))
    expect_identical(names(x), covariates)

    for (v in x) test_detailed_var(v)
  }

  covariates <- c("mpg", "cyl", "disp")
  df <- mtcars[c("hp", covariates)]
  df$mpg[1] <- -1 # missing imputation / flag
  df$disp[2] <- -2 # missing imputation / flag
  df$cyl <- as.factor(df$cyl)

  model <- rpart::rpart(hp ~ mpg + cyl + disp,
    data = df,
    control = rpart::rpart.control(minsplit = 10)
  )
  model_predict <- function(x) predict(model, newdata = x)
  pm <- sample_marginals(df[covariates], n = 50, seed = 26)

  rs <- sculpt_rough(
    dat = pm, model_predict_fun = model_predict,
    n_ice = 10, seed = 1, verbose = 0
  )
  ds <- sculpt_detailed_lm(rs)

  test_detailed(ds, covariates)
})


test_that("variable importance plots with ice", {
  df <- mtcars
  df$vs <- as.factor(df$vs)
  model <- glm(vs ~ hp + mpg, data = df, family = "binomial")
  model_predict <- function(x) predict(model, newdata = x, type = "link")
  covariates <- c("hp", "mpg")

  pm <- sample_marginals(df[covariates], n = 50, seed = 26)

  rs <- sculpt_rough(
    dat = pm, model_predict_fun = model_predict,
    n_ice = 10, seed = 1, verbose = 0
  )

  # vi <- g_var_imp(rs, feat_labels = labels)
  # grid::grid.draw(vi)
  vi_pm_prob <- g_var_imp(
    rs,
    show_pdp_plot = FALSE,
    textsize = 16,
    var_imp_type = "ice",
    logodds_to_prob = TRUE
  )
  expect_s3_class(vi_pm_prob, "gtable")
})

test_that("sculpt_rough handles verbose and data_as_marginals workflow", {
  df <- head(mtcars, 6)
  covariates <- c("mpg", "wt", "disp")
  dat <- df[covariates]
  model <- lm(hp ~ mpg + wt + disp, data = head(mtcars, 12))
  predict_fun <- function(x) predict(model, newdata = x)

  msgs <- capture.output(
    rs <- sculpt_rough(
      dat = dat,
      model_predict_fun = predict_fun,
      n_ice = 3,
      seed = 2,
      verbose = 1,
      data_as_marginals = TRUE
    ),
    type = "message"
  )
  expect_true(any(grepl("Sculpting variable: 1 / 3", msgs, fixed = TRUE)))
  expect_s3_class(rs, "rough")
  expect_s3_class(rs, "sculpture")
  expect_equal(nrow(rs$mpg$subsets), 3)
  expect_equal(colnames(rs$mpg$subsets), c("wt", "disp"))
})

test_that("sculpt_rough verbose handles larger covariate sets", {
  set.seed(17)
  dat <- as.data.frame(matrix(rnorm(120), ncol = 12))
  colnames(dat) <- paste0("x", seq_len(ncol(dat)))
  predict_fun <- function(x) rowSums(as.matrix(x))

  msgs <- capture.output(
    sculpt_rough(
      dat = dat,
      model_predict_fun = predict_fun,
      n_ice = 3,
      seed = 5,
      verbose = 1
    ),
    type = "message"
  )
  expect_true(any(grepl("Sculpting variable: 10 / 12", msgs, fixed = TRUE)))
})

test_that("sculpt_detailed_lm expands missings flag and reports progress", {
  df <- mtcars
  df$cyl <- as.factor(df$cyl)
  covariates <- c("mpg", "wt", "cyl")
  pm <- sample_marginals(df[covariates], n = 40, seed = 21)
  model <- lm(hp ~ mpg + wt + cyl, data = df)
  predict_fun <- function(x) predict(model, newdata = x)

  rs <- sculpt_rough(
    dat = pm,
    model_predict_fun = predict_fun,
    n_ice = 4,
    seed = 11,
    verbose = 0
  )

  msgs <- capture.output(
    ds <- sculpt_detailed_lm(rs, missings = -1, verbose = 1),
    type = "message"
  )
  expect_true(any(grepl("Sculpting variable: 1 / 3", msgs, fixed = TRUE)))
  expect_identical(ds$mpg$missings_flag, -1)
  expect_null(ds$cyl$missings_flag)

  ds_named <- sculpt_detailed_lm(rs, missings = c(mpg = -1, wt = -2))
  expect_identical(ds_named$mpg$missings_flag, -1)
  expect_identical(ds_named$wt$missings_flag, -2)
})

test_that("smoother_gam handles discrete edge cases", {
  skip_if_not_installed("mgcv")

  set.seed(42)
  x_const <- factor(rep("a", 5))
  y <- rnorm(5)
  sm_const <- smoother_gam(
    x = x_const,
    y = y,
    is_discrete = TRUE,
    column_name = "feat"
  )
  expect_s3_class(sm_const, "lm")

  x_levels <- factor(rep(letters[1:2], each = 4))
  y_levels <- rnorm(8)
  sm_levels <- smoother_gam(
    x = x_levels,
    y = y_levels,
    is_discrete = TRUE,
    column_name = "feat"
  )
  preds <- smoother_gam_predict(
    smoother = sm_levels,
    new_x = factor(c("a", "b", "c"), levels = letters[1:3]),
    is_discrete = TRUE,
    column_name = "feat"
  )
  expect_equal(preds[3], 0)

  expect_error(
    smoother_gam(
      x = factor(c("a", "b")),
      y = c(NA_real_, NA_real_),
      is_discrete = TRUE,
      column_name = "feat"
    ),
    "Cannot fit a smoother"
  )
})

test_that("smoother_lm fallbacks cover degenerate inputs", {
  x_const <- rep(1, 5)
  y <- seq_along(x_const)
  sm_const <- smoother_lm(
    x = x_const,
    y = y,
    is_discrete = FALSE,
    column_name = "feat"
  )
  expect_s3_class(sm_const, "lm")

  expect_error(
    smoother_lm(
      x = c(1, 2, NA_real_),
      y = c(NA_real_, NA_real_, NA_real_),
      is_discrete = FALSE,
      column_name = "feat"
    ),
    "Cannot fit a smoother"
  )
})

test_that("smoother_lm_predict handles new discrete levels", {
  set.seed(7)
  x <- factor(rep(letters[1:2], each = 3))
  y <- rnorm(length(x))
  sm <- smoother_lm(
    x = x,
    y = y,
    is_discrete = TRUE,
    column_name = "feat"
  )

  preds <- smoother_lm_predict(
    smoother = sm,
    new_x = factor(c("a", "c"), levels = letters[1:3]),
    is_discrete = TRUE,
    column_name = "feat"
  )
  expect_equal(preds[2], 0)

  expect_error(
    smoother_lm_predict(
      smoother = sm,
      new_x = factor(c("a", "c"), levels = letters[1:3]),
      is_discrete = FALSE,
      column_name = "feat"
    ),
    "Unknown value for prediction"
  )
})

test_that("sculpt_polished supports selection by k and vars", {
  rs <- fixture_sculpture()
  ds <- sculpt_detailed_lm(rs)

  ps_k <- sculpt_polished(ds, k = 1)
  expect_equal(length(ps_k), 1)
  expect_equal(names(ps_k), levels(attr(ds, "cumul_R2")$feature)[1])

  ps_vars <- sculpt_polished(ds, vars = "wt")
  expect_equal(names(ps_vars), "wt")
  expect_equal(attr(ps_vars, "offset"), attr(ds, "offset"))
})

test_that("predict.sculpture supports explicit newdata and printing works", {
  sc <- fixture_sculpture()
  newdata <- fixture_newdata()

  preds <- predict(sc, newdata = newdata)
  expect_length(preds, nrow(newdata))
  expect_named(preds)

  tmp <- tempfile()
  capture.output(print(sc), file = tmp)
  printed <- readLines(tmp, warn = FALSE)
  unlink(tmp)
  expect_true(any(grepl("Rough sculpture with", printed, fixed = TRUE)))
})
