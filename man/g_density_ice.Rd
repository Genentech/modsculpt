% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots_density.R
\name{g_density_ice}
\alias{g_density_ice}
\alias{g_density_ice_plot}
\alias{g_density_ice_plot_list}
\title{Density plots overlaid with ICE curves}
\usage{
g_density_ice_plot(
  object,
  new_data,
  var_name,
  var_label = NULL,
  qtiles = seq(0, 1, by = 0.1),
  task = c("regression", "classification")
)

g_density_ice_plot_list(
  object,
  new_data,
  var_names,
  var_labels = NULL,
  qtiles = seq(0, 1, by = 0.1),
  task = c("regression", "classification")
)
}
\arguments{
\item{object}{Object of class sculpture (rough, detailed)}

\item{new_data}{Data to make quantiles on}

\item{var_name}{String specifying which variable to generate ICE}

\item{var_label}{String (optional) specifying variable label (x label of the plot)}

\item{qtiles}{Quantiles to generate ICE curves}

\item{task}{Prediction task type (regression or classification)}

\item{var_names}{Vector of strings specifying which variables to generate ICE}

\item{var_labels}{Named vector of strings specifying variable labels.}
}
\value{
\code{\link[=g_density_ice_plot]{g_density_ice_plot()}}: ggplot object

\code{\link[=g_density_ice_plot_list]{g_density_ice_plot_list()}}: list of ggplot objects
}
\description{
Create density plot for the data, overlaid with ICE curves at quantiles
of the variable(s) of interest.
}
\details{
\code{\link[=g_density_ice_plot]{g_density_ice_plot()}} creates a density plot for a single variable.

\code{\link[=g_density_ice_plot_list]{g_density_ice_plot_list()}} creates a list of density plots for multiple variables.

These functions should be amenable to any 1st-order model without interaction terms,
however not implemented yet, such as handling \code{predict()} function output
for binary endpoint
}
\examples{
\dontrun{
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

g_density_ice_plot(rs, new_data = pm, var_name = "mpg")
g_list <- g_density_ice_plot_list(
  rs, new_data = pm, var_names = c("mpg", "cyl", "disp", "drat")
)
grid::grid.draw(gridExtra::arrangeGrob(grobs = g_list))
}

}
