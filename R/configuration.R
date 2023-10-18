#' Configure a set of model fits from a specification of different combinations
#'
#' @param model_formula the model formula or formulae you want to fit as a
#'   `formula_provider(...)`.
#' @param dataset the dataset imputed or bootstrapped if need be as a
#'   `bootstrap_provider(...)` or `imputation_provider(...)`
#' @param model_function the model functions to fit as a
#'   `model_function_provider(...)`
#' @param data_subset the data subsets to fit as a `data_subset_provider(...)`
#' @param formula_update and model formula updates to apply before fitting as a
#'   `formula_provider(...)`.
#'
#' @return a configuration dataframe and data provider to run
#' @export
#'
#' @examples
#' 
#' form = ~ bmi + age + chl
#' 
#' fp = formula_provider(
#'   `Univariate` = modelfitter::univariate_from_multivariate(form),
#'   `Adj Model 1` = form,
#'   `Adj Model 2` = update(form, . ~ . - chl),
#' )
#' 
#' mfp = model_function_provider(
#'    `Logistic` = modelfitter::logistic_regression,
#'    `Poisson` = modelfitter::quasi_poisson
#' )
#' 
#' ip = imputation_provider(
#'     mice::nhanes2 %>% dplyr::mutate(death=TRUE), 10)
#' 
#' fup = formula_provider(
#'   hypertension = hyp ~ .,
#'   death = death ~ .
#' )
#' 
#' dsp = data_subset_provider(
#'   all = TRUE,
#'   hypertensives = hyp == "yes",
#'   nonhypertensives = hyp == "no" 
#' )
#' 
#' tmp = configure_models(fp, ip, mfp, dsp, fup) %>% dplyr::glimpse()
#' 
#' # the number of models to fit will be this:
#' sum(tmp$n_boots)
#' 
configure_models = function(
    model_formula, 
    dataset,
    model_function,
    data_subset = data_subset_provider(default = TRUE),
    formula_update = formula_provider(default = . ~ .)
) {
  
  model_base_name = model_formula()
  model_type_name = model_function()
  data_subset_name = data_subset()
  model_update_name = formula_update()
  
  bootstrap_names = dataset()
  
  configuration = tidyr::crossing(
    model_base_name = model_base_name,
    model_type_name = model_type_name,
    data_subset_name = data_subset_name,
    model_update_name = model_update_name
  ) %>% dplyr::mutate(
    tmp_formula_base = purrr::map(model_base_name, ~ model_formula(.x)),
    tmp_formula_update = purrr::map(model_update_name, ~ formula_update(.x)),
    data_subset_fn = purrr::map(data_subset_name, ~ data_subset(.x)),
    model_fn = purrr::map(model_type_name, ~ model_function(.x)),
    n_boots = max(bootstrap_names)
  ) %>% tidyr::unnest(cols = c(
    tmp_formula_base, tmp_formula_update
  )) %>% dplyr::mutate(
    model_formula = purrr::map2( tmp_formula_base, tmp_formula_update, ~ update(.x,.y)),
    model_name = ifelse(
      model_update_name != "default",
      sprintf("%s (%s)",model_base_name, model_update_name),
      model_base_name
    ),
  ) %>% dplyr::select(-tmp_formula_update, -tmp_formula_base)
  
  return(structure(
    configuration,
    bootstraps = tibble::tibble(boot = bootstrap_names),
    dataset_provider = dataset
  ))
}

#' Configure a single model fit using the `modelfitter` framework
#'
#' @param model_name a label for the model
#' @param model_formula the model formula you want to fit
#' @param dataset the dataset
#' @param model_function the model function to fit, e.g. logistic_regression
#'
#' @return a configuration dataframe and data provider to execute the fit with
#' @export
#'
#' @examples
#' # Simple example
#' tmp = configure_model(
#'   "Iris model", 
#'   I(Species == "versicolor") ~ ., 
#'   iris, 
#'   modelfitter::logistic_regression
#' )
#' tmp2 = tmp %>% execute_configuration()
#' summfit = tmp2 %>% summarise_fits()
configure_model = function(
    model_name,
    model_formula, 
    dataset,
    model_function
) {
  
  configuration = tibble::tibble(
    model_name = model_name,
    model_fn = list(model_function),
    data_subset_fn = list(function(x) x),
    n_boots = 1,
    model_formula = list(model_formula)
  )
  
  return(structure(
    configuration,
    bootstraps = tibble::tibble(boot = 1),
    dataset_provider = bootstrap_provider(dataset,1)
  ))
}
