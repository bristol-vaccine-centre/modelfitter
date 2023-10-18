

#' Calculate a global P-value for a model fit for each of the predictors.
#'
#' In the summary output of a model there is a p-value for each level of the factors
#' whereas usually we want to know what the overall contribution of the individual
#' predictor to the model is.
#' 
#' This function usually calculates a Type 2 Anova for the predictors within the models
#' but sometimes falls back to different methods if the model is not supported
#' by the `car::Anova` or `stats::anova` packages. The method used is reported 
#' in the output. This is designed as an automated process and may not work in 
#' all situations, or produce appropriate output for all model types. Interaction
#' terms may be particularly problematic.
#'
#' @param x a model 
#' @param ... not used.
#'
#' @return a dataframe of the predictors of the model (but not the levels) in 
#' one column and a global p value in another. The global.p will be given as zero 
#' or one despite this being not really possible. It si assumed that it will
#' be formatted to truncate very low or very high values.
#' @export
#'
#' @examples
#' diamonds2 = ggplot2::diamonds %>% dplyr::mutate(
#'    is_coloured = color <= "F",
#'    dplyr::across(dplyr::where(is.ordered), ~ factor(.x,ordered=FALSE))
#' ) %>% dplyr::select(-color)
#' 
#' lmodel = stats::lm(Petal.Width ~ ., iris)
#' glmodel = logistic_regression(diamonds2, is_coloured ~ cut + carat + clarity * price)
#' coxfit = cox_model(survival::flchain, survival::Surv(futime, death) ~ sex * age)
#' 
#' # Error conditions
#' try(anova(stats::lm( cut ~ carat + clarity * price, diamonds2)))
#' global_p_value(stats::lm( cut ~ carat + clarity * price, diamonds2))
#' 
#' 
#' # cox model
#' global_p_value(coxfit)
#' 
#' # linear model
#' global_p_value(lmodel)
#' 
#' # logistic regression (OR)
#' global_p_value(
#'   stats::glm(is_coloured ~ cut + carat + clarity + price, diamonds2,  family="binomial"))
#' global_p_value(
#'   logistic_regression(diamonds2, is_coloured ~ cut + carat + clarity * price))
#' 
#' # poisson regression (RR)
#' global_p_value(
#'   quasi_poisson(diamonds2, is_coloured ~ cut + carat + clarity * price))
#' global_p_value(
#'   robust_poisson(diamonds2, is_coloured ~ cut + carat + clarity * price))
#' global_p_value(
#'   robust_poisson_2(diamonds2, is_coloured ~ cut + carat + clarity * price))
#' 
#' # log binomial regression (RR)
#' # TODO: this does not work at the moment as an example as the log_binomial starting value is
#' # a problem
#' global_p_value(
#'   log_binomial(diamonds2, is_coloured ~ cut + carat + clarity + price))
#' 
#' # global_p_value(
#' #  log_binomial_2(diamonds2, is_coloured ~ cut + carat + clarity + price))
#' 
#' 
global_p_value = function(x,...) {
  
  # for a list of models. It is assumed and not checked that 
  #   the models are all the same design. The list format is designed for 
  #   bootstrapped imputations.
  # if (inherits(x,"list")) {
  #   tmp = lapply(x, global_p_value)
  #   dplyr::bind_rows(tmp) %>%
  #     dplyr::group_by(predictor) %>%
  #     dplyr::summarise(global.p.value = mean(global.p.value,na.rm=TRUE),.groups = "drop") %>%
  #     dplyr::mutate(global.p.method = "Aggregated")
  # }
  
  suppressWarnings(tryCatch(
    {
      if (inherits(x,"coeftest")) {
        # These are the result of "robust" sandwich estimators from the `sandwich` package
        # they need unwrapping. 
        y = car::Anova(attr(x, "object"), vcov. = function(m) sandwich::vcovHC(m, type="HC1"), type="III", test.statistic="Wald")
        y = y %>% tibble::as_tibble(rownames = "predictor") %>% 
          dplyr::select(predictor, global.p = dplyr::starts_with("Pr(")[1]) %>%
          dplyr::mutate(global.p.method = "Wald Chi-squared test (III)") %>%
          dplyr::filter(!predictor %in% c("(Intercept)","Residuals","NULL"))
        
      } else {
        # This won't always work:
        y = car::Anova(x, type="II", test.statistic="LR") 
        y = y %>% tibble::as_tibble(rownames = "predictor") %>% 
          dplyr::select(predictor, global.p = dplyr::starts_with("Pr(")[1]) %>%
          dplyr::mutate(global.p.method = "Likelihood ratio test (II)") %>%
          dplyr::filter(!predictor %in% c("(Intercept)","Residuals","NULL"))
      }
      y
    },
    error = function(e) {
      # car::Anova did not work. Lets try base
      tryCatch({
      tibble::as_tibble(stats::anova(x),  rownames = "predictor") %>% 
        dplyr::select(predictor, global.p = dplyr::starts_with("Pr(")[1]) %>%
        dplyr::mutate(global.p.method = "Likelihood ratio test (I)") %>%
        dplyr::filter(!predictor %in% c("(Intercept)","Residuals","NULL"))
      }, 
      error = function(e2) {
        # emm seems to always produce an output even if an error
        return(
          emmeans::joint_tests(x) %>%
            dplyr::transmute(
              predictor = `model term`, 
              global.p = dplyr::case_when(
                  p.value %>% stringr::str_starts("<") ~ 0,
                  p.value %>% stringr::str_starts(">") ~ 1,
                  TRUE ~ suppressWarnings(as.numeric(p.value)))
            ) %>%
            dplyr::mutate(global.p.method = "Joint tests (Wald - III)")
        )
      })
    }))
}

