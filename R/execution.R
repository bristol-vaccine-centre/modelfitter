


#' Fit statistical models defined in a `modelfitter` configuration 
#'
#' @param cfg a 4 column dataframe with
#' * model_name - minumum
#' * model_fn
#' * data_subset_fn
#' * model_formula
#' @param retain_fit keep the model fit object? If the config results in 
#'   less than 50 models then the default of this is TRUE. Otherwise we summarise 
#'   the models as we go saving memory.
#' @param performance calculate performance metrics for all the models. These
#'   will be summarised over bootstrap replicates. Setting this to false may
#'   be a good idea if you have lots of models.
#' @inheritDotParams model_labels
#' @param cache cache model output
#'
#' @return a 5 column dataframe with additional nested fitted results in 
#'   `model_fit`. This nested dataframe will have the columns: 
#' * `boot` - an id for the data bootstrap or imputation; 
#' * `fit` (optional) - the model fit itself; 
#' * `coef` the model coefficients; 
#' * `global_p` the global p values for this model (see global_p_value()); 
#' * `performance` (optional) the performance metrics for this model; 
#' @export
#'
#' @examples
#' 
#' tmp = configure_model(
#'   "Iris", I(Species == "versicolor") ~ ., iris, logistic_regression)
#' tmp2 = tmp %>% execute_configuration()
#'  
#' tmp3  = configure_models(
#'    formula_provider(
#'       "<F" = I(color < "F") ~ cut + carat + clarity + price,
#'       "<H" = I(color < "H") ~ cut + carat + clarity + price
#'    ),
#'    bootstrap_provider(ggplot2::diamonds, max_n = 10),
#'    model_function_provider(
#'      "Log reg" = modelfitter::logistic_regression,
#'      "Poisson" = modelfitter::quasi_poisson
#'    )
#')
#' 
#' tmp4 = tmp3 %>% execute_configuration(cache = TRUE)
#' 
execute_configuration = function(cfg, retain_fit = sum(cfg$n_boots)<50, performance = TRUE, ..., cache=FALSE) {
  
  dots = rlang::list2(...)
  a = attributes(cfg)
  boot_tmpl = a$bootstraps
  data_prov = a$dataset_provider
  
  steps = nrow(cfg)
  
  pid = cli::cli_progress_bar("Fitting models",total = sum(cfg$n_boots))
  # env = environment()
  
  out = .cached(cfg %>% dplyr::mutate(
    # model_fit is a nested dataframe with a row per bootstrap
    # each row will have
    # boot; ?fit; coef; global_p; quality
    # TODO: implement caching?
    model_fit = purrr::pmap(., .f = function(model_fn, data_subset_fn, model_formula, ...) {
      
      
      # do the fit
      tmp = boot_tmpl %>% dplyr::mutate(
        
        fit = purrr::map(boot, function(.x, ...) {
          data = data_prov(.x) %>% dplyr::ungroup() %>% data_subset_fn()
          cli::cli_progress_update(id = pid) #.envir = env)
          tmp = try( data %>% model_fn(model_formula, !!!dots), silent= TRUE)
          if (inherits(tmp,"try-error")) browser()
          return(tmp)
        
        })
      )
      
      # summarise results for each boot
      tmp = tmp %>% dplyr::mutate(
        coef = purrr::map(fit, ~ suppressWarnings(broom::tidy(.x))),
        n_obs = purrr::map(fit, ~ tryCatch(stats::nobs(.x), error=function(e) NA))
      )
      
      # global_p_values
      tmp = tmp %>% dplyr::mutate(
        global_p = purrr::map(fit, global_p_value)
      )
      
      #quality metrics
      if (performance) {
        tmp = tmp %>% dplyr::mutate(
          performance = purrr::map(fit, ~ 
              # suppressWarnings(broom::glance(.x))
              suppressMessages(performance::model_performance(.x, verbose=FALSE))
          )
        )
      }
      
      # get the model output for the first fit
      ml = model_labels(tmp$fit[[1]], ...)
      if (!retain_fit) tmp = tmp %>% dplyr::select(-fit)
      return(structure(tmp, "labels" = ml))
      
    })
  ), .config_hash(cfg), .nocache = !cache)
  
  cli::cli_progress_done(id = pid)
  
  return(structure(out, 
    fits_retained = retain_fit,
    performance = performance
  ))
  
}






