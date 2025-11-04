## Test fixture helpers
##
## Lightweight helpers that construct reusable sculptures mirroring the
## regression and classification workflows showcased in `modsculpt_example`.

fixture_sculpture <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      dat <- head(mtcars, 10)
      dat$cyl <- factor(dat$cyl)
      model <- lm(hp ~ mpg + wt + cyl, data = dat)
      covariates <- c("mpg", "wt", "cyl")
      pm <- sample_marginals(dat[covariates], n = 12, seed = 42)
      cached <<- sculpt_rough(
        dat = pm,
        model_predict_fun = function(x) predict(model, newdata = x),
        n_ice = 4,
        seed = 99,
        verbose = 0
      )
    }
    cached
  }
})

fixture_sculpture_alt <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      dat <- head(mtcars, 10)
      dat$cyl <- factor(dat$cyl)
      model <- lm(hp ~ mpg + wt + cyl, data = dat)
      covariates <- c("mpg", "wt", "cyl")
      pm <- sample_marginals(dat[covariates], n = 12, seed = 123)
      cached <<- sculpt_rough(
        dat = pm,
        model_predict_fun = function(x) predict(model, newdata = x),
        n_ice = 5,
        seed = 77,
        verbose = 0
      )
    }
    cached
  }
})

fixture_newdata <- function() {
  dat <- head(mtcars, 10)
  dat$cyl <- factor(dat$cyl)
  dat[c("mpg", "wt", "cyl")]
}

build_regression_workflow <- function(seed = 101) {
  set.seed(seed)
  idx <- sample(seq_len(nrow(mtcars)), size = 20)
  train <- mtcars[idx, ]
  holdout <- mtcars[-idx, ]
  features <- c("mpg", "wt")
  model <- lm(hp ~ mpg + wt, data = train)
  predict_fun <- function(x) predict(model, newdata = x)
  pm <- sample_marginals(train[features], n = 60, seed = 11)
  rough <- sculpt_rough(pm, predict_fun, n_ice = 5, seed = 17, verbose = 0)
  detailed <- sculpt_detailed_lm(rough)
  polished <- sculpt_polished(detailed, k = 1)
  list(
    features = features,
    train = train,
    holdout = holdout,
    model = model,
    predict_fun = predict_fun,
    rough = rough,
    detailed = detailed,
    polished = polished
  )
}

build_classification_workflow <- function(seed = 202) {
  set.seed(seed)
  df <- mtcars
  df$am <- factor(df$am)
  idx <- sample(seq_len(nrow(df)), size = 20)
  train <- df[idx, ]
  holdout <- df[-idx, ]
  features <- c("mpg", "wt")
  model <- glm(am ~ mpg + wt, data = train, family = binomial())
  predict_fun <- function(x) predict(model, newdata = x, type = "link")
  pm <- sample_marginals(train[features], n = 60, seed = 19)
  rough <- sculpt_rough(pm, predict_fun, n_ice = 5, seed = 23, verbose = 0)
  detailed <- sculpt_detailed_gam(rough)
  polished <- sculpt_polished(detailed, k = 1)
  list(
    features = features,
    train = train,
    holdout = holdout,
    model = model,
    predict_fun = predict_fun,
    rough = rough,
    detailed = detailed,
    polished = polished
  )
}

build_discrete_rough <- function() {
  df <- head(mtcars, 12)
  df$cyl <- factor(df$cyl)
  df$vs <- factor(df$vs)
  features <- c("cyl", "vs")
  model <- lm(hp ~ cyl + vs, data = df)
  pm <- sample_marginals(df[features], n = 40, seed = 31)
  pm$cyl <- factor(pm$cyl, levels = levels(df$cyl))
  pm$vs <- factor(pm$vs, levels = levels(df$vs))
  predict_fun <- function(x) {
    x$cyl <- factor(x$cyl, levels = levels(df$cyl))
    x$vs <- factor(x$vs, levels = levels(df$vs))
    predict(model, newdata = x)
  }
  rough <- sculpt_rough(pm, predict_fun, n_ice = 4, seed = 29, verbose = 0)
  list(rough = rough, features = features)
}
