% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format-output.R
\name{format_summary}
\alias{format_summary}
\title{Format a summary of multiple fits into a table.}
\usage{
format_summary(
  summfit,
  ...,
  statistic = "OR",
  global.p = getOption("modelfitter.global_p_values", TRUE),
  inv_link = exp,
  col_header = "{model_name} (N={sprintf('\%d',max(n_obs_summary))})",
  row_design =
    "{format_ci(value.median,value.lower,value.upper,reference,fmt.ratio = '\%1.2g')}",
  p_format = NULL,
  font_size = getOption("modelfitter.font_size", 8),
  font = getOption("modelfitter.font", "Arial"),
  footer_text = NULL,
  summarise_fn = NULL
)
}
\arguments{
\item{summfit}{A set of fitted, and summarised configured models}

\item{...}{
  Arguments passed on to \code{\link[=model_labels]{model_labels}}
  \describe{
    \item{\code{model}}{a model or list of models.}
    \item{\code{label_fn}}{a function that allows a predictor label to be renamed. This
should expect a vector and return a vector of the same length. Levels will be
terms in the model function and may be column names, or combinations thereof}
    \item{\code{subgroup_label_fn}}{a function that allows a subgroup label to be renamed. This
should expect a vector and return a vector of the same length. The
input to this function will be either a factor level name or a combination
of them or whatever else the model decides to name it's coefficients.}
  }}

\item{statistic}{what model output is this table presenting? Is it an OR, a
RR or a HR? or something else?}

\item{global.p}{present the global p value (anova III) rather than the line by line values.}

\item{inv_link}{the inverse of the link function employed in the models. This is
almost always the inverse \code{exp(...)} unless we are dealing with a linear model.}

\item{col_header}{a glue spec using columns from the summfit data table to label the model columns.
\code{model_name} and \code{n_obs_summary} should be defined as a minimum. Other bits
of metadata will be present if the table has been configured using
\code{configure_models(...)} including \code{model_type_name}, \code{data_subset_name},
\code{model_base_name}, \code{model_update_name}, \code{n_boots}.}

\item{row_design}{a glue spec for presenting the statistic. valid columns are
\code{reference} - the referent status, \code{group.type}, \code{beta.lower},
\code{beta.median}, \code{beta.upper}, \code{value.lower},
\code{value.median}, \code{value.upper}, \code{p.value.mixture}, \code{global.p.mixture},
\code{global.p.method}. The helper functions \code{format_ratio(x,fmt.ratio = "\%1.3g")} and \code{format_ci(med,low,hi,ref)}
may be useful in this glue string}

\item{p_format}{a function (or lambda) converting a number into a p-value string}

\item{font_size}{(optional) the font size for the table in points}

\item{font}{(optional) the font family for the table (which will be matched to
closest on your system)}

\item{footer_text}{any text that needs to be added at the end of the table,
setting this to FALSE dsables the whole footer (as does
\code{options("tableone.hide_footer"=TRUE)}).}

\item{summarise_fn}{in the event that we want to present multiple models in
the same column of a table it is possible that there are multiple entries
for each variable. This function will combine them (at a text level) so they
can be placed in a table. Examples could be \code{dplyr::first} or
\code{~ paste0(.x,collapse="\\n")}}
}
\value{
a huxtable tabular output of the model(s)
}
\description{
Format a summary of multiple fits into a table.
}
\examples{
cfg  = configure_models(
   formula_provider(
      "<F" = I(color < "F") ~ cut + carat + clarity + price,
      "<H" = I(color < "H") ~ cut + carat + clarity + price
   ),
   bootstrap_provider(ggplot2::diamonds, max_n = 10),
   model_function_provider(
     "Log reg" = modelfitter::logistic_regression,
     "Poisson" = modelfitter::quasi_poisson
   )
)

exectn = cfg \%>\% execute_configuration(cache = TRUE)
summfit = exectn \%>\% summarise_fits()
hux = summfit \%>\% format_summary()
hux
}
