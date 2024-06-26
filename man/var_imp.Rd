% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{var_imp}
\alias{var_imp}
\alias{calc_dir_var_imp}
\alias{calc_cumul_R2}
\title{Various metrics related to model sculpting}
\usage{
calc_dir_var_imp(object, newdata = NULL)

calc_cumul_R2(object, newdata = NULL)
}
\arguments{
\item{object}{\code{sculpture}}

\item{newdata}{(Optional) Data to calculate the importance from.
If omitted, the data that were provided to build the sculpture are used.}
}
\value{
\code{data.table} with direct requested metrics.
}
\description{
Various metrics related to model sculpting
}
\section{Functions}{
\itemize{
\item \code{calc_dir_var_imp()}: Direct variable importance

\item \code{calc_cumul_R2()}: Calculate cumulative approximation of R^2

}}
\examples{
df <- mtcars
df$vs <- as.factor(df$vs)
model <- rpart::rpart(
  hp ~ mpg + carb + vs,
  data = df,
  control = rpart::rpart.control(minsplit = 10)
)
model_predict <- function(x) predict(model, newdata = x)
covariates <- c("mpg", "carb", "vs")
pm <- sample_marginals(df[covariates], n = 50, seed = 5)

rs <- sculpt_rough(
  dat = pm,
  model_predict_fun = model_predict,
  n_ice = 10,
  seed = 1,
  verbose = 0
)

# show direct variable importance
calc_dir_var_imp(rs)

# show cumulative approximation R^2
calc_cumul_R2(rs)
}
