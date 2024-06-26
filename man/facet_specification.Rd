% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{facet_specification}
\alias{facet_specification}
\title{Instructions for facet vizualisations}
\usage{
facet_specification(
  labels = NULL,
  ncol = NULL,
  sort = "alphabetical",
  top_k = NULL,
  subset = NULL,
  scales = "free_x"
)
}
\arguments{
\item{labels}{(\code{NULL}) or named character vector with variable labels.}

\item{ncol}{(\code{NULL}) or number of columns in the facet.}

\item{sort}{One of "alphabetical", "importance", or "range" - sorting of the facets.}

\item{top_k}{(\code{NULL}) or number of most important features to show.}

\item{subset}{(\code{NULL}) or a vector of variables to show.}

\item{scales}{One of "free", "free_x", or "free_y" - axis scales of the graphs.}
}
\value{
List of class \code{facet_specification}.
}
\description{
Instructions for facet vizualisations
}
\examples{
\dontrun{
g_ice(
  sculpture,
  facet_spec = facet_specification(
    ncol = 3, # display 3 columns
    sort = "importance" # sort by importance
  )
)
}

}
