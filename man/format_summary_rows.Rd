% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{format_summary_rows}
\alias{format_summary_rows}
\title{Predict the potential coefficient names for a model output}
\usage{
format_summary_rows(rawData, ..., label_fn = ~.x)
}
\arguments{
\item{rawData}{a \code{modelfit} or a dataframe(s)}

\item{...}{the columns that we are using as predictors, as a
list of formulae (rhs), a tidyselect call, a dplyr::vars() specification or a
list of characters}

\item{label_fn}{a function that converts column names into readable labels}
}
\value{
a dataframe with the likely names of the coefficients in a model which
can be joined to a model coefficients to
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
}
\examples{
# Disabling this example as function deprecated
if (FALSE) {

boot = run_model(
  ggplot2::diamonds \%>\% dplyr::mutate(cut = factor(cut, ordered=FALSE)), 
  price ~ ., 
  stats::lm
)
tmp = format_summary_rows(boot)



# logistic regression (OR)
diamonds2 = ggplot2::diamonds \%>\% dplyr::mutate(
   is_coloured = color <= "F",
   dplyr::across(dplyr::where(is.ordered), ~ factor(.x,ordered=FALSE))
) \%>\% dplyr::select(-color)

format_summary_rows(diamonds2, is_coloured ~ cut + carat + clarity*price)
model = logistic_regression(diamonds2, is_coloured ~ cut + carat + clarity * price)
model = logistic_regression(diamonds2, is_coloured ~ cut + I(cut=="D") + clarity*price)

stats::coef(model)
}
}
\keyword{internal}
