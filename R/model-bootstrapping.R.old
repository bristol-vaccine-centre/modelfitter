## Run analysis ----


## Model standard interfaces ----

logistic_regression = function(data, formula) {
  glm(formula = formula, family = binomial(), data = data)
}

log_binomial = function(data, formula) {
  #browser()
  outcome = rlang::f_lhs(formula)
  if (data %>% pull(!!outcome) %>% is.factor()) {
    frac = sum(data %>% pull(!!outcome) %>% as.numeric() != 1)/nrow(data) # 1 is referent
  } else {
    frac = sum(data %>% pull(!!outcome) %>% as.numeric() != 1)/nrow(data) # 1 is positive outcome
  }
  terms = # length(all.vars(rlang::f_rhs(formula)))
    suppressWarnings(length(coefficients(lm(formula,data))))
  suppressWarnings(glm(formula = formula, data = data, family=binomial(link="log"), start=c(log(frac),rep(1e-4,terms-1)),
                       maxit = 1000
  ))
}

log_binomial_2 = function(data, formula) {
  # browser()
  outcome = rlang::f_lhs(formula)
  if (data %>% pull(!!outcome) %>% is.factor()) {
    frac = sum(data %>% pull(!!outcome) %>% as.numeric() != 1)/nrow(data) # 1 is referent
  } else {
    frac = sum(data %>% pull(!!outcome) %>% as.numeric() != 1)/nrow(data) # 1 is positive outcome
  }
  terms = # length(all.vars(rlang::f_rhs(formula)))
    suppressWarnings(length(coefficients(lm(formula,data))))
  starts = c(log(frac),rep(1e-4,terms-1))
  # starts = log(c(frac,rep(1e-4,terms-1)))
  logbin::logbin(formula = formula, data = data, start=starts,
                 maxit = 10000, method = "em", accelerate = "squarem"
  )
}

quasi_poisson = function(data, formula) {
  outcome = rlang::f_lhs(formula)
  # shape the data so that a positive outcome has a poisson "count" of 1 and a negative outcome is a 0
  if (data %>% pull(!!outcome) %>% is.factor()) {
    if(data %>% pull(!!outcome) %>% levels() %>% length() != 2) stop("outcome has is a factor and does not have exactly two levels: ",as_label(outcome))
    data = data %>% mutate(!!outcome := as.numeric(!!outcome) != 1) # factor 1 is referrent
  }
  glm(formula = formula, family = quasipoisson(link = log), data = data)
}

# needed for robust estimators
# https://data.library.virginia.edu/understanding-robust-standard-errors/
# heteroskedasticity-consistent (HC) standard errors using sandwich

# TODO: investigate glmRob from robust package which has an anova implementation (but not a car::Anova one)
robust_poisson = function(data, formula) {
  outcome = rlang::f_lhs(formula)
  # shape the data so that a positive outcome has a poisson "count" of 1 and a negative outcome is a 0
  if (data %>% pull(!!outcome) %>% is.factor()) {
    if(data %>% pull(!!outcome) %>% levels() %>% length() != 2) stop("outcome has is a factor and does not have exactly two levels: ",as_label(outcome))
    data = data %>% mutate(!!outcome := as.numeric(!!outcome) != 1) # factor 1 is referrent
  }
  m = glm(formula = formula, data = data, family = poisson(link = "log"))
  lmtest::coeftest(m, vcov. = sandwich::vcovHC(m, type="HC1"), save = TRUE)
}

robust_poisson_2 = function(data, formula) {
  outcome = rlang::f_lhs(formula)
  # shape the data so that a positive outcome has a poisson "count" of 1 and a negative outcome is a 0
  if (data %>% pull(!!outcome) %>% is.factor()) {
    if(data %>% pull(!!outcome) %>% levels() %>% length() != 2) stop("outcome has is a factor and does not have exactly two levels: ",as_label(outcome))
    data = data %>% mutate(!!outcome := as.numeric(!!outcome) != 1) # factor 1 is referrent
  }
  geepack::geeglm(formula = formula,
                  data    = data %>% mutate(.seqno = row_number()),
                  family  = poisson(link = "log"),
                  id      = .seqno,
                  corstr  = "exchangeable")
}


## Data imputation ----

impute_data = function(rawData, predictorVars) {
  imputeFrom = rawData %>% select(c(where(is.numeric),where(is.factor))) %>% colnames()
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

format_summary_rows = function(rawData, predictorVars) {
  if (is.character(predictorVars)) {
    predictorVars = lapply(predictorVars,as.symbol)
  }
  # Get the labels as a dataframe, and ensure the ordering of factors is preserved
  predictorList = readable_label_mapping(predictorVars) %>% unname()
  predictorKeys = sapply(predictorVars,as_label)
  levelList = predictorVars %>% lapply(function(.x) try(rawData %>% pull(!!.x) %>% levels())) %>% unlist() %>% unname() %>% unique(fromLast=TRUE)
  keys = tibble(predictor = predictorKeys) %>%
    mutate(
      group_order = row_number(),
      type = purrr::map_chr(predictor, ~ case_when(
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
    unnest(subgroup,keep_empty=TRUE) %>%
    mutate(
      subgroup_order = row_number(),
      term=paste0(predictor, ifelse(is.na(subgroup),"",subgroup)),
      group = readable_label_mapping(predictor)
    )
  inter = keys %>%
    inner_join(keys, by=character(), suffix=c("",".int")) %>%
    filter(subgroup != subgroup.int & group != group.int) %>%
    mutate(
      predictor = paste0(predictor,":",predictor.int),
      type = "interaction",
      group_order = pmax(group_order,group_order.int)+0.1,
      subgroup_order = subgroup_order*1000+subgroup_order.int,
      subgroup = paste0(ifelse(is.na(subgroup),type,subgroup)," AND ",ifelse(is.na(subgroup.int),type.int,subgroup.int)),
      group = paste0(group," AND ",group.int),
      term = paste0(term,":",term.int)
    ) %>% select(-ends_with(".int"))

  keyAndInt = keys %>% bind_rows(inter) %>% arrange(group_order,subgroup_order)
  predictorList = unique(keyAndInt$group)
  levelList = unique(keyAndInt$subgroup)
  keyAndInt = keyAndInt %>% mutate(
    group = group %>% ordered(predictorList),
    subgroup = subgroup %>% ordered(levels = levelList)
  )
  return(keyAndInt %>% select(c(-group_order,-subgroup_order)))
  # TODO: this will not work for ordered inputs
  # TODO: need to include predictors as a formula and look into interaction terms and transformed model inputs etc.
  #
}



run_models = function(imputedData, modelConfig, modelFunction = logistic_regression, ...) {

  modelConfig = modelConfig %>% mutate(
    dependent = purrr::map_chr(form, ~ all.vars(rlang::f_lhs(.x))),
    predictors = purrr::map(form, ~ all.vars(rlang::f_rhs(.x)))
  )

  boots = tibble(bootstrap = 1:10) %>% crossing(modelConfig) %>%
    # pull the imputed data as a bootstrap
    mutate(impute = purrr::map(bootstrap, ~ mice::complete(imputedData,.x))) %>%
    # TODO: filter content to look at age... mutate(impute = purrr::map(bootstrap, ~ mice::complete(imputedData,.x) %>% filter(...))) %>%
    # fit the model to the data
    mutate(model = purrr::map2(impute, form, ~  modelFunction(data=.x, formula=.y))) %>%
    mutate(model.coef = purrr::map(model, ~ suppressWarnings(broom::tidy(.x)))) %>%
    mutate(model.stats = purrr::map(model, ~ suppressWarnings(broom::glance(.x))))

  # This use of car::Anova is based on gtsummary::add_global_p which I didn't use directly because of the bootstrapping.
  boots = tryCatch({
    if (any(class(boots$model[[1]]) == "coeftest")) {
      # use the stored model object when wrapped with robust errors from the sandwich package
      # boots %>% mutate(
      #   # In the situation of use of the sandwich package the variance-covariance matrix is changed. It is unclear if this affects
      #   # global.p = purrr::map(model, ~ car::Anova(attr(.x, "object"), vcov. = function(m) sandwich::vcovHC(m, type="HC1"), type="III") %>% mutate(predictor = rownames(.))),
      #   # This does not do anything for glm models vcov. is only an option for lm's
      #   # Its unclear whether the car::Anova package would produce a different result for global significance tests
      #   global.p = purrr::map(model, ~ car::Anova(attr(.x, "object"), type="III") %>% mutate(predictor = rownames(.)))
      # )
      boots %>% mutate(global.p = purrr::map(predictors, ~ tibble(predictor = .x,`Pr(>Chisq)`=NA_character_)))
    } else {
      boots %>% mutate(global.p = purrr::map(model, ~ car::Anova(.x, type="III") %>% mutate(predictor = rownames(.))))
    }
  }, error = function(e) {
    # try a emmeans::joint_tests
    # browser()
    tryCatch({
      boots %>% mutate(global.p = purrr::map(model, ~ emmeans::joint_tests(.x) %>%
                                               mutate(predictor = `model term`, `Pr(>Chisq)` = case_when(
                                                 p.value %>% stringr::str_starts("<") ~ 0,
                                                 p.value %>% stringr::str_starts(">") ~ 1,
                                                 TRUE ~ suppressWarnings(as.numeric(p.value))
                                               ))))
      # geepack::anova: P(>|Chi|)
    }, error = function(e) {
      # fall back to a default empty tibble
      boots %>% mutate(global.p = purrr::map(predictors, ~ tibble(predictor = .x,`Pr(>Chisq)`=NA_character_)))
    })
  })

  return(boots)
}

oob_or = function(x, limit = 50) {
  case_when(
    x > limit ~ Inf,
    x < 1/limit ~ 0,
    TRUE ~ x
  )
}


combine_boots = function(boots, predictorVars) {
  keys = format_summary_rows(boots$impute[[1]], predictorVars)
  pooled.HR = boots %>% unnest(model.coef) %>% group_by(modelName, term) %>% summarise(
    statistic.mixture = sprintf_list("%1.2f [%1.2f\u2013%1.2f]", oob_or(exp(qmixnorm(p=c(0.5,0.025,0.975), means=estimate, sds=std.error)))),
    beta.lower = qmixnorm(p=0.025, means=estimate, sds=std.error),
    beta.median = qmixnorm(p=0.5, means=estimate, sds=std.error),
    beta.upper = qmixnorm(p=0.975, means=estimate, sds=std.error),
    p.value.mixture = scales::pvalue(.mean(p.value)),
    .groups = "drop"
  )

  pooled.global.p = boots %>% unnest(global.p) %>% group_by(modelName, predictor) %>%
    summarise(
      global.p.mixture = scales::pvalue(.mean(`Pr(>Chisq)`)),
      .groups = "drop"
    )

  out = keys %>% crossing(boots %>% select(modelName) %>% distinct()) %>% left_join(pooled.HR, by=c("term","modelName")) %>% left_join(pooled.global.p, by=c("predictor","modelName")) %>%
    select(modelName,group,subgroup,statistic = statistic.mixture, p.value = global.p.mixture, p.component = p.value.mixture, type, starts_with("beta")) %>%
    group_by(modelName,group) %>%
    filter(type!="interaction" | !is.na(statistic)) %>%
    mutate(
      statistic = case_when(
        sum(!is.na(statistic))==0 ~ "\u2014",
        is.na(statistic) ~ "ref",
        TRUE ~ statistic
      ),
      p.value = if_else(row_number() == 1, p.value,NA_character_)
    )
}

.mean = function(x,...) suppressWarnings(mean(x,...))

summarise_boots = function(boots, predictorVars, statistic="OR") {

  out = combine_boots(boots,predictorVars) %>% select(c(-starts_with("beta"),-type,-p.value)) %>% rename(Characteristic = group, Group = subgroup, !!statistic := statistic, `P-value` = p.component)
  out = out %>% hux_tidy(rowGroupVars = vars(Characteristic,Group),colGroupVars = vars(modelName))
  return(out)
}

pool_performance = function(boots, statistics = NULL) {
  if (class(boots$model[[1]]) == "coeftest") {
    # unwrap the saved model if using a robust sandwich estimator
    boots = boots %>% mutate(model = purrr::map(model, ~ attr(.x, "object")))
  }
  boots = boots %>%
    mutate(model.perf = purrr::map(model, ~ performance::model_performance(.x) %>% as.data.frame() %>% pivot_longer(cols=everything())))
  # browser()
  pooled.performance = boots %>%
    unnest(model.perf) %>%
    group_by(modelName,statistic = name) %>%
    summarise(value = sprintf("%1.5g",.mean(value)),.groups = "drop")
  if (!is.null(statistics)) pooled.performance = pooled.performance %>% filter(statistic %in% statistics)
  pooled.performance
}

analyse_performance = function(boots, statistics = c("AIC","BIC","R2_Tjur","R2_Nagelkerke","RMSE","Sigma")) {
  pooled.performance = pool_performance(boots, statistics)
  pooled.performance %>% hux_tidy(rowGroupVars = vars(statistic),colGroupVars = vars(modelName))
}

