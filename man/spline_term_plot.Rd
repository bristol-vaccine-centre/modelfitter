% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-spline-term-plot.R
\name{spline_term_plot}
\alias{spline_term_plot}
\title{Spline term marginal effects plot}
\usage{
spline_term_plot(
  coxmodel,
  var_name,
  xlab = var_name,
  max_y = NULL,
  n_breaks = 7
)
}
\arguments{
\item{coxmodel}{an output of a coxph model}

\item{var_name}{a variable that is involved in a spline term}

\item{xlab}{x axis label}

\item{max_y}{maximium hazard ratio to display on y axis. Inferred from the
central estimates if missing, which will most likely cut off confidence
intervals}

\item{n_breaks}{The number of divisions on the y axis}
}
\value{
a ggplot
}
\description{
Spline term marginal effects plot
}
