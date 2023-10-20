#' Summarise a modelfitter execution 
#' 
#' A `modelfitter` execution may consist of multiple models, multiple data sets, 
#' and multiple bootstraps of data or imputations of data. This summarises the 
#' coefficients, global p values and performance metrics of each model fit over 
#' the multiple data bootstraps to get to a single set of coefficients, and
#' metrics for each different model. 
#'
#' Coefficients are combined assuming normally distributed beta coefficients,
#' prior to link function inversion, as mixtures of normal distributions and
#' 95% confidence intervals calculated. performance metrics and global p values
#' are given as means of the values of bootstrap.
#'
#' @param exectn a modelfitter execution of multiple model fits.
#' @param ... not used at present
#'
#' @return a nested dataframe with a single row per model and summary columns in
#'   `coef_summary`, `global_p_summary` and 
#' @export
#'
#' @examples
#' 
#' # Complex example, multiple statsistical models, multiple formulae,
#' # bootstrapped data.
#' cfg  = configure_models(
#'    formula_provider(
#'       "<F" = I(color < "F") ~ cut + carat + clarity + price,
#'       "<H" = I(color < "H") ~ cut + carat + clarity + price
#'    ),
#'    bootstrap_provider(ggplot2::diamonds, max_n = 100),
#'    model_function_provider(
#'      "Log reg" = modelfitter::logistic_regression,
#'      "Poisson" = modelfitter::quasi_poisson
#'    )
#' )
#' 
#' exectn = cfg %>% execute_configuration(cache = TRUE)
#' summfit = exectn %>% summarise_fits()
#'  
#' # Simple example
#' tmp = configure_model(
#'   "Iris", I(Species == "versicolor") ~ ., iris, logistic_regression)
#' tmp2 = tmp %>% execute_configuration()
#' summfit = tmp2 %>% summarise_fits()
#' 
summarise_fits = function(exectn, ...) {
  
  # .x will be all the fits for a single configuration
  # it is only differing by data bootstraps.
  # need to summarise multiple fits into a single row tibble
  # to get summaries by configuration
  
  exectn = exectn %>% dplyr::mutate(
    n_obs_summary = purrr::map_dbl(model_fit, ~ {
      mean(unlist(.x$n_obs),na.rm=TRUE)
    })
  )
  
  exectn = exectn %>% dplyr::mutate(
    
    coef_summary = purrr::map(model_fit, ~ {
      .x %>% tidyr::unnest(coef) %>%
        dplyr::group_by(term) %>%
        dplyr::summarise(
          beta.lower = qmixnorm(p=0.025, means=estimate, sds=std.error),
          beta.median = qmixnorm(p=0.5, means=estimate, sds=std.error),
          beta.upper = qmixnorm(p=0.975, means=estimate, sds=std.error),
          p.value.mixture = suppressWarnings(mean(p.value))
        )
    })
  )  
  
  exectn = exectn %>% dplyr::mutate(
    global_p_summary = purrr::map(model_fit, ~ {
      .x %>% tidyr::unnest(global_p) %>%
        dplyr::group_by(predictor) %>%
        dplyr::summarise(
          global.p.mixture = suppressWarnings(mean(global.p)),
          global.p.method = paste0(unique(global.p.method),collapse=", ")
        )
    })
  )
  
  if (attributes(exectn)$performance) {
    exectn = exectn %>% dplyr::mutate( 
      quality_summary = purrr::map(model_fit, ~ {
        .x %>% tidyr::unnest(performance) %>%
          dplyr::ungroup() %>%
          dplyr::summarise(
            dplyr::across(dplyr::where(is.numeric), ~ mean(.x, na.rm = TRUE))
          )
      })
    )
  }
}
