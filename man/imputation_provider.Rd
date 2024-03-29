% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/providers.R
\name{imputation_provider}
\alias{imputation_provider}
\title{Provide access to missing data imputations}
\usage{
imputation_provider(data, max_n, formulae = ~., ...)
}
\arguments{
\item{data}{the data frame with missing rows}

\item{max_n}{the maximum number of different imputations to}

\item{formulae}{a lit of formulae with all the columns used (defaults to everything)}

\item{...}{cache control}
}
\value{
a function that returns a dataset for inputs between \code{1:max_n}
}
\description{
Provide access to missing data imputations
}
\examples{
ip = imputation_provider(mice::nhanes2, 10, list(~ hyp + bmi, ~ age + chl))
ip(1) \%>\% head(10)

}
