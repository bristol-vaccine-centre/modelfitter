## Run analysis ----

#' @importFrom stats lm
NULL

## Model standard interfaces ----


#' Fit standard logistic regression
#'
#' @param data a data frame
#' @param formula a formula of the form `binary_outcome ~ obs1 + obs2 + ...`
#'
#' @return a model object
#' @export
logistic_regression = function(data, formula) {
  stats::glm(formula = formula, family = stats::binomial(), data = data)
}

#' Fit standard log binomial regression using stats::glm
#'
#' @param data a data frame
#' @param formula a formula of the form `binary_outcome ~ obs1 + obs2 + ...`
#'
#' @return a model object
#' @export
log_binomial = function(data, formula) {
  #browser()
  outcome = rlang::f_lhs(formula)
  if (data %>% dplyr::pull(!!outcome) %>% is.factor()) {
    frac = sum(data %>% dplyr::pull(!!outcome) %>% as.numeric() != 1)/nrow(data) # 1 is referent
  } else {
    frac = sum(data %>% dplyr::pull(!!outcome) %>% as.numeric() != 1)/nrow(data) # 1 is positive outcome
  }
  terms = # length(all.vars(rlang::f_rhs(formula)))
    suppressWarnings(length(stats::coefficients(lm(formula,data))))
  suppressWarnings(stats::glm(formula = formula, data = data, family=stats::binomial(link="log"), start=c(log(frac),rep(1e-4,terms-1)),
                       maxit = 1000
  ))
}

#' Fit standard log binomial regression using logbin::logbin
#'
#' @param data a data frame
#' @param formula a formula of the form `binary_outcome ~ obs1 + obs2 + ...`
#'
#' @return a model object
#' @export
log_binomial_2 = function(data, formula) {
  # browser()
  outcome = rlang::f_lhs(formula)
  if (data %>% dplyr::pull(!!outcome) %>% is.factor()) {
    frac = sum(data %>% dplyr::pull(!!outcome) %>% as.numeric() != 1)/nrow(data) # 1 is referent
  } else {
    frac = sum(data %>% dplyr::pull(!!outcome) %>% as.numeric() != 1)/nrow(data) # 1 is positive outcome
  }
  terms = # length(all.vars(rlang::f_rhs(formula)))
    suppressWarnings(length(stats::coefficients(lm(formula,data))))
  starts = c(log(frac),rep(1e-4,terms-1))
  # starts = log(c(frac,rep(1e-4,terms-1)))
  logbin::logbin(formula = formula, data = data, start=starts,
                 maxit = 10000, method = "em", accelerate = "squarem"
  )
}

#' Fit standard poisson regression
#' 
#' equivalent to a log_binomial model (RR)
#'
#' @param data a data frame
#' @param formula a formula of the form `binary_outcome ~ obs1 + obs2 + ...`
#'
#' @return a model object
#' @export
quasi_poisson = function(data, formula) {
  outcome = rlang::f_lhs(formula)
  # shape the data so that a positive outcome has a poisson "count" of 1 and a negative outcome is a 0
  if (data %>% dplyr::pull(!!outcome) %>% is.factor()) {
    if(data %>% dplyr::pull(!!outcome) %>% levels() %>% length() != 2) stop("outcome has is a factor and does not have exactly two levels: ",rlang::as_label(outcome))
    data = data %>% dplyr::mutate(!!outcome := as.numeric(!!outcome) != 1) # factor 1 is referrent
  }
  stats::glm(formula = formula, family = stats::quasipoisson(link = log), data = data)
}

# needed for robust estimators
# https://data.library.virginia.edu/understanding-robust-standard-errors/
# heteroskedasticity-consistent (HC) standard errors using sandwich

# TODO: investigate glmRob from robust package which has an anova implementation (but not a car::Anova one)

#' Fit robust poisson regression using sandwich estimators
#' 
#' equivalent to a log_binomial model
#'
#' @param data a data frame
#' @param formula a formula of the form `binary_outcome ~ obs1 + obs2 + ...`
#'
#' @return a model object
#' @export
robust_poisson = function(data, formula) {
  outcome = rlang::f_lhs(formula)
  # shape the data so that a positive outcome has a poisson "count" of 1 and a negative outcome is a 0
  if (data %>% dplyr::pull(!!outcome) %>% is.factor()) {
    if(data %>% dplyr::pull(!!outcome) %>% levels() %>% length() != 2) stop("outcome has is a factor and does not have exactly two levels: ",rlang::as_label(outcome))
    data = data %>% dplyr::mutate(!!outcome := as.numeric(!!outcome) != 1) # factor 1 is referrent
  }
  m = stats::glm(formula = formula, data = data, family = stats::poisson(link = "log"))
  lmtest::coeftest(m, vcov. = sandwich::vcovHC(m, type="HC1"), save = TRUE)
}

#' Fit robust poisson regression using geepack::glm
#' 
#' equivalent to a log_binomial model
#'
#' @param data a data frame
#' @param formula a formula of the form `binary_outcome ~ obs1 + obs2 + ...`
#'
#' @return a model object
#' @export
robust_poisson_2 = function(data, formula) {
  outcome = rlang::f_lhs(formula)
  # shape the data so that a positive outcome has a poisson "count" of 1 and a negative outcome is a 0
  if (data %>% dplyr::pull(!!outcome) %>% is.factor()) {
    if(data %>% dplyr::pull(!!outcome) %>% levels() %>% length() != 2) stop("outcome has is a factor and does not have exactly two levels: ",rlang::as_label(outcome))
    data = data %>% dplyr::mutate(!!outcome := as.numeric(!!outcome) != 1) # factor 1 is referrent
  }
  geepack::geeglm(formula = formula,
                  data    = data %>% dplyr::mutate(.seqno = dplyr::row_number()),
                  family  = stats::poisson(link = "log"),
                  id      = .seqno,
                  corstr  = "exchangeable")
}


## Data imputation ----

#' Take a list of predictors and impute data for those predictors
#'
#' @param rawData a raw data frame of observations
#' @param predictorVars a lit of columns that we are using
#'
#' @return a mice object containing imputed data
#' @export
impute_data = function(rawData, predictorVars) {
  imputeFrom = rawData %>% dplyr::select(c(where(is.numeric),where(is.factor))) %>% colnames()
  # Configure mice imputation columns
  init = mice::mice(rawData, maxit=0)
  meth = init$method
  predM = init$predictorMatrix
  predictorNames = sapply(predictorVars, as_label)
  # only impute values in the model:
  meth[!(names(meth) %in% predictorNames)] = ""
  # don't use for imputation
  # predM[!(names(meth) %in% predictorNames)] = 0
  # only use factors and numerics for imputation
  predM[!(names(meth) %in% imputeFrom)] = 0
  set.seed(103)
  imputeModel = mice::mice(rawData, method=meth, predictorMatrix=predM, m=10,maxit = 5,printFlag=FALSE)
  return(imputeModel)
}

## Model output formatter ----

#' Predict the potential coefficient names for a model output 
#' 
#' @param rawData the data the model will be fitted to 
#' @param predictorVars the parameters of the model.
#'
#' @return a dataframe with the likely names of the coefficients
#' @export
format_summary_rows = function(rawData, predictorVars) {
  if (is.character(predictorVars)) {
    predictorVars = lapply(predictorVars,as.symbol)
  }
  # Get the labels as a dataframe, and ensure the ordering of factors is preserved
  predictorList = readable_label_mapping(predictorVars) %>% unname()
  predictorKeys = sapply(predictorVars,as_label)
  levelList = predictorVars %>% lapply(function(.x) try(rawData %>% dplyr::pull(!!.x) %>% levels())) %>% unlist() %>% unname() %>% unique(fromLast=TRUE)
  keys = tibble::tibble(predictor = predictorKeys) %>%
    dplyr::mutate(
      group_order = dplyr::row_number(),
      type = purrr::map_chr(predictor, ~ dplyr::case_when(
        length(levels(rawData[[.x]]))==2 ~ "binomial",
        length(levels(rawData[[.x]]))>2 ~ "multinomial",
        length(unique(rawData[[.x]]))==2 ~ "binomial",
        is.integer(rawData[[.x]]) ~ "discrete",
        is.numeric(rawData[[.x]]) ~ "continuous",
        is.ordered(rawData[[.x]]) ~ "ordinal",
        is.character(rawData[[.x]]) ~ "discrete",
        TRUE ~ "unknown"
      )),
      subgroup = purrr::map(predictor, ~ levels(rawData[[.x]]))
    ) %>%
    tidyr::unnest(subgroup,keep_empty=TRUE) %>%
    dplyr::mutate(
      subgroup_order = dplyr::row_number(),
      term=paste0(predictor, ifelse(is.na(subgroup),"",subgroup)),
      group = readable_label_mapping(predictor)
    )
  inter = keys %>%
    dplyr::inner_join(keys, by=character(), suffix=c("",".int")) %>%
    dplyr::filter(subgroup != subgroup.int & group != group.int) %>%
    dplyr::mutate(
      predictor = paste0(predictor,":",predictor.int),
      type = "interaction",
      group_order = pmax(group_order,group_order.int)+0.1,
      subgroup_order = subgroup_order*1000+subgroup_order.int,
      subgroup = paste0(ifelse(is.na(subgroup),type,subgroup)," AND ",ifelse(is.na(subgroup.int),type.int,subgroup.int)),
      group = paste0(group," AND ",group.int),
      term = paste0(term,":",term.int)
    ) %>% dplyr::select(-ends_with(".int"))

  keyAndInt = keys %>% dplyr::bind_rows(inter) %>% dplyr::arrange(group_order,subgroup_order)
  predictorList = unique(keyAndInt$group)
  levelList = unique(keyAndInt$subgroup)
  keyAndInt = keyAndInt %>% dplyr::mutate(
    group = group %>% ordered(predictorList),
    subgroup = subgroup %>% ordered(levels = levelList)
  )
  return(keyAndInt %>% dplyr::select(c(-group_order,-subgroup_order)))
  # TODO: this will not work for ordered inputs
  # TODO: need to include predictors as a formula and look into interaction terms and transformed model inputs etc.
  #
}



#' Run a set of models 
#'
#' @param imputedData a mice imputed data
#' @param modelConfig a dataframe with column `form` continaing formulae to test, with other columns containing metsdata
#' @param modelFunction the type of test as a function call. the function must accept `data` and `formula` inputs
#' @param ... not used
#'
#' @return a dataframe with result of running all formulae on all bootstrapped imputations adn summary stats
#' @export
run_models = function(imputedData, modelConfig, modelFunction = logistic_regression, ...) {

  modelConfig = modelConfig %>% dplyr::mutate(
    dependent = purrr::map_chr(form, ~ all.vars(rlang::f_lhs(.x))),
    predictors = purrr::map(form, ~ all.vars(rlang::f_rhs(.x)))
  )

  boots = tibble::tibble(bootstrap = 1:10) %>% tidyr::crossing(modelConfig) %>%
    # pull the imputed data as a bootstrap
    dplyr::mutate(impute = purrr::map(bootstrap, ~ mice::complete(imputedData,.x))) %>%
    # TODO: filter content to look at age... dplyr::mutate(impute = purrr::map(bootstrap, ~ mice::complete(imputedData,.x) %>% dplyr::filter(...))) %>%
    # fit the model to the data
    dplyr::mutate(model = purrr::map2(impute, form, ~  modelFunction(data=.x, formula=.y))) %>%
    dplyr::mutate(model.coef = purrr::map(model, ~ suppressWarnings(broom::tidy(.x)))) %>%
    dplyr::mutate(model.stats = purrr::map(model, ~ suppressWarnings(broom::glance(.x))))

  # This use of car::Anova is based on gtsummary::add_global_p which I didn't use directly because of the bootstrapping.
  boots = tryCatch({
    if (any(class(boots$model[[1]]) == "coeftest")) {
      # use the stored model object when wrapped with robust errors from the sandwich package
      # boots %>% dplyr::mutate(
      #   # In the situation of use of the sandwich package the variance-covariance matrix is changed. It is unclear if this affects
      #   # global.p = purrr::map(model, ~ car::Anova(attr(.x, "object"), vcov. = function(m) sandwich::vcovHC(m, type="HC1"), type="III") %>% dplyr::mutate(predictor = rownames(.))),
      #   # This does not do anything for glm models vcov. is only an option for lm's
      #   # Its unclear whether the car::Anova package would produce a different result for global significance tests
      #   global.p = purrr::map(model, ~ car::Anova(attr(.x, "object"), type="III") %>% dplyr::mutate(predictor = rownames(.)))
      # )
      boots %>% dplyr::mutate(global.p = purrr::map(predictors, ~ tibble::tibble(predictor = .x,`Pr(>Chisq)`=NA_character_)))
    } else {
      boots %>% dplyr::mutate(global.p = purrr::map(model, ~ car::Anova(.x, type="III") %>% dplyr::mutate(predictor = rownames(.))))
    }
  }, error = function(e) {
    # try a emmeans::joint_tests
    # browser()
    tryCatch({
      boots %>% dplyr::mutate(global.p = purrr::map(model, ~ emmeans::joint_tests(.x) %>%
                                               dplyr::mutate(predictor = `model term`, `Pr(>Chisq)` = dplyr::case_when(
                                                 p.value %>% stringr::str_starts("<") ~ 0,
                                                 p.value %>% stringr::str_starts(">") ~ 1,
                                                 TRUE ~ suppressWarnings(as.numeric(p.value))
                                               ))))
      # geepack::anova: P(>|Chi|)
    }, error = function(e) {
      # fall back to a default empty tibble
      boots %>% dplyr::mutate(global.p = purrr::map(predictors, ~ tibble::tibble(predictor = .x,`Pr(>Chisq)`=NA_character_)))
    })
  })

  return(boots)
}

oob_or = function(x, limit = 50) {
  dplyr::case_when(
    x > limit ~ Inf,
    x < 1/limit ~ 0,
    TRUE ~ x
  )
}


#' Combine bootstraps 
#'
#' @param boots a bootstrapped model output
#' @param predictorVars all the variables used in all the models.
#'
#' @return a collapsed summary of all bootstraps
#' @export
combine_boots = function(boots, predictorVars) {
  keys = format_summary_rows(boots$impute[[1]], predictorVars)
  pooled.HR = boots %>% tidyr::unnest(model.coef) %>% dplyr::group_by(modelName, term) %>% dplyr::summarise(
    statistic.mixture = sprintf_list("%1.2f [%1.2f\u2013%1.2f]", oob_or(exp(qmixnorm(p=c(0.5,0.025,0.975), means=estimate, sds=std.error)))),
    beta.lower = qmixnorm(p=0.025, means=estimate, sds=std.error),
    beta.median = qmixnorm(p=0.5, means=estimate, sds=std.error),
    beta.upper = qmixnorm(p=0.975, means=estimate, sds=std.error),
    p.value.mixture = scales::pvalue(.mean(p.value)),
    .groups = "drop"
  )

  pooled.global.p = boots %>% tidyr::unnest(global.p) %>% dplyr::group_by(modelName, predictor) %>%
    dplyr::summarise(
      global.p.mixture = scales::pvalue(.mean(`Pr(>Chisq)`)),
      .groups = "drop"
    )

  out = keys %>% tidyr::crossing(boots %>% dplyr::select(modelName) %>% dplyr::distinct()) %>% dplyr::left_join(pooled.HR, by=c("term","modelName")) %>% dplyr::left_join(pooled.global.p, by=c("predictor","modelName")) %>%
    dplyr::select(modelName,group,subgroup,statistic = statistic.mixture, p.value = global.p.mixture, p.component = p.value.mixture, type, tidyselect::starts_with("beta")) %>%
    dplyr::group_by(modelName,group) %>%
    dplyr::filter(type!="interaction" | !is.na(statistic)) %>%
    dplyr::mutate(
      statistic = dplyr::case_when(
        sum(!is.na(statistic))==0 ~ "\u2014",
        is.na(statistic) ~ "ref",
        TRUE ~ statistic
      ),
      p.value = dplyr::if_else(row_number() == 1, p.value,NA_character_)
    )
}

.mean = function(x,...) suppressWarnings(mean(x,...))


#' Printable output of model combined
#'
#' @param boots a bootstrapped model output
#' @param predictorVars all the variables used in all the models.
#' @param statistic was this an OR or an RR (or a HR) just used for labelling
#'
#' @return a printable summary
#' @export
#' @examples 
#' # out = boots %>% summarise_boots(...predictors..., "OR" )
#' # out %>% ggrrr::hux_tidy(
#' #    rowGroupVars = dplyr::vars(Characteristic,Group),
#' #   colGroupVars = dplyr::vars(modelName)
#' #)
summarise_boots = function(boots, predictorVars, statistic="OR") {
  out = combine_boots(boots,predictorVars) %>% 
    dplyr::select(c(-tidyselect::starts_with("beta"),-type,-p.value)) %>% 
    dplyr::rename(Characteristic = group, Group = subgroup, !!statistic := statistic, `P-value` = p.component)
  return(out)
}


.pool_performance = function(boots, statistics = NULL) {
  if (inherits(boots$model[[1]], "coeftest")) {
    # unwrap the saved model if using a robust sandwich estimator
    boots = boots %>% dplyr::mutate(model = purrr::map(model, ~ attr(.x, "object")))
  }
  boots = boots %>%
    dplyr::mutate(model.perf = purrr::map(model, ~ performance::model_performance(.x) %>% as.data.frame() %>% tidyr::pivot_longer(cols=everything())))
  # browser()
  pooled.performance = boots %>%
    tidyr::unnest(model.perf) %>%
    dplyr::group_by(modelName,statistic = name) %>%
    dplyr::summarise(value = sprintf("%1.5g",.mean(value)),.groups = "drop")
  if (!is.null(statistics)) pooled.performance = pooled.performance %>% dplyr::filter(statistic %in% statistics)
  pooled.performance
}

#' Performance metrics of bootstrapped models
#'
#' @param boots bootstrapped model output
#' @param statistics model performance statistics
#'
#' @return a printable summary
#' @examples 
#' # pooled = boots %>% summarise_boots()
#' #  pooled %>% ggrrr::hux_tidy(
#' #    rowGroupVars = dplyr::vars(statistic),
#' #    colGroupVars = dplyr::vars(modelName)
#' #  )
analyse_performance = function(boots, statistics = c("AIC","BIC","R2_Tjur","R2_Nagelkerke","RMSE","Sigma")) {
  pooled.performance = .pool_performance(boots, statistics)
}

