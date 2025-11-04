## Test fixture helpers
##
## These functions cache small rough-sculpture objects so multiple tests can
## reuse a consistent setup without repeatedly fitting models or sampling
## product marginals. They mirror the workflows in modsculpt_example, providing
## both continuous and discrete features for plotting and evaluation tests.

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
