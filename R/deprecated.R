#' Predict the potential coefficient names for a model output 
#' 
#' `r lifecycle::badge("deprecated")`
#' 
#' @param rawData a `modelfit` or a dataframe(s)
#' @inheritParams impute_data
#' @param label_fn a function that converts column names into readable labels
#'
#' @return a dataframe with the likely names of the coefficients in a model which
#'   can be joined to a model coefficients to 
#' 
#' @keywords internal
#' @export
#' @examples
#' boot = run_model(
#'   ggplot2::diamonds %>% dplyr::mutate(cut = factor(cut, ordered=FALSE)), 
#'   price ~ ., 
#'   stats::lm
#' )
#' tmp = format_summary_rows(boot)
#' 
#' 
#' 
#' # logistic regression (OR)
#' diamonds2 = ggplot2::diamonds %>% dplyr::mutate(
#'    is_coloured = color <= "F",
#'    dplyr::across(dplyr::where(is.ordered), ~ factor(.x,ordered=FALSE))
#' ) %>% dplyr::select(-color)
#' 
#' format_summary_rows(diamonds2, is_coloured ~ cut + carat + clarity*price)
#' model = logistic_regression(diamonds2, is_coloured ~ cut + carat + clarity * price)
#' model = logistic_regression(diamonds2, is_coloured ~ cut + I(cut=="D") + clarity*price)
#' 
#' stats::coef(model)
#' 
format_summary_rows = function(rawData, ..., label_fn = ~ .x) {
  
  lifecycle::deprecate_soft("0.0.0.9011")
  
  predictorVars = .predictorVars(rawData, ...)
  if (inherits(rawData,"modelfit")) {
    rawData = dplyr::bind_rows(rawData$impute)
  } else {
    rawData = dplyr::bind_rows(rawData)
  }
  
  predictorNames = sapply(predictorVars, rlang::as_label)
  label_fn = getOption("tableone.labeller",label_fn)
  label_fn = purrr::as_mapper(label_fn)
  
  # Get the labels as a dataframe, and ensure the ordering of factors is preserved
  predictorList = label_fn(predictorNames) %>% unname() %>% unlist()
  predictorKeys = sapply(predictorVars,rlang::as_label)
  # TODO: look at this as maybe ordering can be done using character rather than
  # factor level
  levelList = predictorVars %>% 
    lapply(function(.x) try(
      rawData %>% dplyr::pull(!!.x) %>% levels(), silent=TRUE
    )) %>% unlist() %>% unname() %>% unique(fromLast=TRUE)
  
  keys = tibble::tibble(
    predictor = predictorKeys,
    group = predictorList
  ) %>%
    dplyr::mutate(
      group_order = dplyr::row_number(),
      type = purrr::map_chr(predictor, ~ dplyr::case_when(
        is.ordered(rawData[[.x]]) ~ "ordinal",
        length(levels(rawData[[.x]]))==2 ~ "binomial",
        length(levels(rawData[[.x]]))>2 ~ "multinomial",
        length(unique(rawData[[.x]]))==2 ~ "binomial",
        is.integer(rawData[[.x]]) ~ "discrete",
        is.numeric(rawData[[.x]]) ~ "continuous",
        is.character(rawData[[.x]]) ~ "discrete",
        TRUE ~ "unknown"
      )),
      .tmp = purrr::map2(predictor, type, ~ 
                           tibble::tibble(
                             subgroup = levels(rawData[[.x]]),
                             subterm = if (.y == "ordinal") {
                               c(NA,".L",".Q",".C", paste0("^",4:100))[1:length(levels(rawData[[.x]]))]
                             } else {
                               levels(rawData[[.x]])
                             }
                           ))
    ) %>%
    tidyr::unnest(.tmp,keep_empty=TRUE) %>%
    dplyr::mutate(
      subgroup_order = dplyr::row_number(),
      term=paste0(predictor, ifelse(is.na(subterm),"",subterm))
    ) %>% 
    dplyr::select(-subterm)
  
  inter = keys %>%
    dplyr::cross_join(keys, suffix=c("",".int")) %>%
    dplyr::filter(term != term) %>%
    dplyr::mutate(
      predictor = paste0(predictor,":",predictor.int),
      type = "interaction",
      group_order = pmax(group_order,group_order.int)+0.1,
      subgroup_order = subgroup_order*1000+subgroup_order.int,
      subgroup = paste0(ifelse(is.na(subgroup),type,subgroup)," AND ",ifelse(is.na(subgroup.int),type.int,subgroup.int)),
      group = paste0(group," AND ",group.int),
      term = paste0(term,":",term.int)
    ) %>% dplyr::select(-tidyselect::ends_with(".int"))
  
  keyAndInt = keys %>% dplyr::bind_rows(inter) %>% 
    dplyr::arrange(group_order,subgroup_order)
  predictorList = unique(keyAndInt$group)
  levelList = unique(keyAndInt$subgroup)
  keyAndInt = keyAndInt %>% dplyr::mutate(
    group = group %>% ordered(predictorList),
    subgroup = subgroup %>% ordered(levels = levelList)
  )
  return(keyAndInt %>% dplyr::select(c(-group_order,-subgroup_order)))
  
}



#' Take a list of predictors and impute data for those predictors
#' 
#' `r lifecycle::badge("deprecated")`
#'
#' @keywords internal
#' @param rawData a raw data frame of observations
#' @param ... the columns that we are using as predictors, as a 
#'   list of formulae (rhs), a tidyselect call, a dplyr::vars() specification or a
#'   list of characters
#' @param m the number of imputations (default 10)
#'
#' @return a mice object containing imputed data
#' @export
impute_data = function(rawData, ..., m=getOption("tableone.imputations",10)) {
  
  lifecycle::deprecate_soft("0.0.0.9011")
  
  predictorVars = .parse_unique(rawData, ..., .side="rhs") %>% .sort_symbols()
  imputeModel = .cached({
    imputeFrom = rawData %>% dplyr::select(c(tidyselect::where(is.numeric),tidyselect::where(is.factor),tidyselect::where(is.logical))) %>% colnames()
    # Configure mice imputation columns
    init = mice::mice(rawData, maxit=0)
    meth = init$method
    predM = init$predictorMatrix
    predictorNames = sapply(predictorVars, rlang::as_label)
    # only impute values in the model:
    meth[!(names(meth) %in% predictorNames)] = ""
    # don't use for imputation
    # predM[!(names(meth) %in% predictorNames)] = 0
    # only use factors and numerics for imputation
    predM[!(names(meth) %in% imputeFrom)] = 0
    set.seed(103)
    return(mice::mice(rawData, method=meth, predictorMatrix=predM, m=m,maxit = 5,printFlag=FALSE))
  }, rawData, predictorVars, ...)
  return(imputeModel)
}



#' Run a model through the `modelfitter` pipeline
#' 
#' `r lifecycle::badge("deprecated")`
#' 
#' @keywords internal
#' 
#' @param data a single dataframe
#' @param formula a model formula
#' @param modelFunction the type of test as a function call. the function must
#'   accept `data` and `formula` inputs and produce an output compatible with
#'   `broom::tidy` and `broom::glance`
#' @param ... can be used to provide parameters to the `modelFunction`.
#' @param modelName an optional (but recommended) name for the model
#'
#' @return a dataframe with result of running all formulae on all bootstrapped imputations adn summary stats
#' @export
#' @examples
#' # stats::lm( Petal.Length ~ Species + Petal.Width, iris)
#' 
#' boots = iris %>% run_model(Petal.Length ~ Species + Petal.Width, stats::lm)
#' 
run_model = function(data, formula, modelFunction = logistic_regression, ..., modelName = .fmt_form(formula)) {
  
  lifecycle::deprecate_soft("0.0.0.9011")
  
  if (rlang::is_formula(formula)) formula = list(formula)
  modelConfig = tibble::tibble(form = formula, modelName = modelName)
  run_models(data, modelConfig, modelFunction, ...)
}

.fmt_form = function(x) {
  paste0(trimws(format(x)),collapse = " ")
}

#' Run a set of models 
#' 
#' `r lifecycle::badge("deprecated")`
#' 
#' @keywords internal
#'
#' @param imputedData a mice imputed data source
#' @param modelConfig a dataframe with column `form` containing formulae to
#'   test, with other columns containing any other metadata. if provided 
#'   `modelName` will be used to label the model in plots (otherwise the formula)
#'   will be used
#' @param modelFunction the type of test as a function call. the function must accept `data` and `formula` inputs
#' @param pipeline a pipeline function (i.e. takes a dataframe and returns a dataframe) that can be used to modify the imputed data
#' @param ... can be used to provide parameters to the `modelFunction`.
#'
#' @return a dataframe with result of running all formulae on all bootstrapped imputations adn summary stats
#' @export
run_models = function(imputedData, modelConfig, modelFunction = logistic_regression, pipeline = ~ .x, ...) {
  
  lifecycle::deprecate_soft("0.0.0.9011")
  
  modelConfig = modelConfig %>% 
    dplyr::mutate(
      dependent = purrr::map(form, ~ f_lhs_vars(.x)),
      predictors = purrr::map(form, ~ f_rhs_vars(.x))
    )
  
  used = c(unlist(modelConfig$dependent), unlist(modelConfig$predictors)) %>% unique()
  imputedData = imputedData %>% dplyr::select(dplyr::all_of(used))
  
  if (any(lapply(imputedData, is.ordered))) {
    warning("Ordered factors detected. Modelfitter does not support these at present.")
    imputedData = imputedData %>% dplyr::mutate(dplyr::across(dplyr::where(is.ordered), ~ factor(.x,ordered=FALSE)))
  }
  
  if (!"modelName" %in% colnames(modelConfig)) {
    modelConfig = modelConfig %>% dplyr::mutate(modelName = ~ purrr::map(form, .fmt_form))
  }
  
  if (inherits(imputedData, "mids")) {
    # a mice data set
    extrcfn = function(x,i) mice::complete(x,i)
    size = x$m
  } else {
    # assume a plain data frame
    extrcfn = function(x,i) x
    size = 1
  }
  
  modelFunction2 = function(x,y) modelFunction(data=x, formula=y, ...)
  
  map_pipeline = purrr::as_mapper(pipeline)
  
  boots = tibble::tibble(bootstrap = 1:size) %>% 
    tidyr::crossing(modelConfig) %>%
    # pull the imputed data as a bootstrap
    dplyr::mutate(impute = purrr::map(bootstrap, ~ extrcfn(imputedData,.x))) %>%
    dplyr::mutate(impute = purrr::map(impute, map_pipeline)) %>%
    # TODO: filter content to look at age... dplyr::mutate(impute = purrr::map(bootstrap, ~ mice::complete(imputedData,.x) %>% dplyr::filter(...))) %>%
    # fit the model to the data
    dplyr::mutate(model = purrr::map2(impute, form, modelFunction2)) %>%
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
      boots %>% dplyr::mutate(
        global.p = purrr::map(predictors, ~ tibble::tibble(predictor = .x,`Pr(>Chisq)`=NA_character_)),
        global.p.method = "None"
      )
      
    } else {
      boots %>% dplyr::mutate(
        global.p = purrr::map(model, ~ car::Anova(.x, type="III") %>% dplyr::mutate(predictor = rownames(.))),
        global.p.method = "Anova type III (car)"
      )
    }
  }, error = function(e) {
    # try a emmeans::joint_tests
    # browser()
    tryCatch({
      boots %>% dplyr::mutate(
        global.p = purrr::map(model, ~ emmeans::joint_tests(.x) %>%
                                dplyr::mutate(predictor = `model term`, `Pr(>Chisq)` = dplyr::case_when(
                                  p.value %>% stringr::str_starts("<") ~ 0,
                                  p.value %>% stringr::str_starts(">") ~ 1,
                                  TRUE ~ suppressWarnings(as.numeric(p.value))
                                ))),
        global.p.method = "Anova type III (emm)"
      )
      # geepack::anova: P(>|Chi|)
    }, error = function(e) {
      # fall back to a default empty tibble
      boots %>% dplyr::mutate(
        global.p = purrr::map(predictors, ~ tibble::tibble(predictor = .x,`Pr(>Chisq)`=NA_character_)),
        global.p.method = "None"
      )
    })
  })
  
  return(structure(
    boots,
    class = c("modelfit",class(boots))))
}

.predictorVars = function(boots, ...) {
  if (inherits(boots,"modelfit")) {
    if (length(rlang::list2(...))==0) {
      predictorVars = .parse_unique(dplyr::bind_rows(boots$impute), boots$form, .side="rhs")
    } else {
      predictorVars = .parse_unique(dplyr::bind_rows(boots$impute), ..., .side="rhs")
    }
  } else {
    predictorVars = .parse_unique(dplyr::bind_rows(boots), ..., .side="rhs")
  }
  
  return(predictorVars)
}

#' Combine bootstraps 
#' 
#' `r lifecycle::badge("deprecated")`
#' 
#' @keywords internal
#'
#' @param boots a bootstrapped model output
#' @inheritParams impute_data
#'
#' @return a collapsed summary of all bootstraps
#' @export
#' @examples
#' boots = iris %>% run_model(Petal.Length ~ Species + Petal.Width, stats::lm)
#' combine_boots(boots,  inv_link = ~ .x)
combine_boots = function(boots,..., inv_link = exp) {
  
  lifecycle::deprecate_soft("0.0.0.9011")
  
  inv_link = rlang::as_function(inv_link)
  keys = format_summary_rows(boots, ...)
  
  pooled.HR = boots %>% 
    tidyr::unnest(model.coef) %>% 
    dplyr::group_by(modelName, term) %>% 
    dplyr::summarise(
      statistic.mixture = sprintf_list("%1.2f [%1.2f\u2013%1.2f]", 
                                       inv_link(qmixnorm(p=c(0.5,0.025,0.975), means=estimate, sds=std.error))
      ),
      beta.lower = qmixnorm(p=0.025, means=estimate, sds=std.error),
      beta.median = qmixnorm(p=0.5, means=estimate, sds=std.error),
      beta.upper = qmixnorm(p=0.975, means=estimate, sds=std.error),
      p.value.mixture = scales::pvalue(.mean(p.value)),
      .groups = "drop"
    )
  
  pooled.global.p = boots %>% tidyr::unnest(global.p) %>% 
    # first matching column:
    dplyr::rename_with(~ "global.p", .cols = dplyr::starts_with("Pr(")[[1]]) %>%
    dplyr::group_by(modelName, predictor) %>%
    dplyr::summarise(
      global.p.mixture = scales::pvalue(.mean(global.p)),
      .groups = "drop"
    )
  
  out = keys %>% 
    tidyr::crossing(boots %>% dplyr::select(modelName) %>% dplyr::distinct()) %>% 
    dplyr::left_join(pooled.HR, by=c("term","modelName")) %>% 
    dplyr::left_join(pooled.global.p, by=c("predictor","modelName")) %>%
    dplyr::select(modelName,group,subgroup,
                  statistic = statistic.mixture, 
                  p.value = global.p.mixture, 
                  p.component = p.value.mixture, 
                  type, 
                  tidyselect::starts_with("beta")) %>%
    dplyr::group_by(modelName,group) %>%
    dplyr::filter(type!="interaction" | !is.na(statistic)) %>%
    dplyr::mutate(
      statistic = dplyr::case_when(
        sum(!is.na(statistic))==0 ~ "\u2014",
        is.na(statistic) ~ "ref",
        TRUE ~ statistic
      )#,
      #p.value = dplyr::if_else(dplyr::row_number() == 1, p.value,NA_character_)
    )
  
  return(out)
}


.mean = function(x) suppressWarnings(mean(x,na.rm=TRUE))

#' Printable output of model combined
#' 
#' `r lifecycle::badge("deprecated")`
#' 
#' @keywords internal
#'
#' @param boots a bootstrapped model output
#' @inheritParams impute_data
#' @param statistic was this an OR or an RR (or a HR) just used for labelling
#' @param global.p present the global p value (anova) rather than the line by line value.
#'
#' @return a printable summary
#' @export
#' @examples 
#' # out = boots %>% summarise_boots(...predictors..., "OR" )
#' boot = iris %>% 
#'   dplyr::mutate(is_versicolor = Species == "versicolor") %>% 
#'   run_model(is_versicolor ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width)
#'   
#' out = summarise_boots(boot)
#' #out %>% ggrrr::hux_tidy(
#' #    rowGroupVars = dplyr::vars(Characteristic,Group),
#' #    colGroupVars = dplyr::vars(modelName)
#' #)
summarise_boots = function(boots, ..., statistic="OR",global.p=TRUE, inv_link = NULL) {
  
  lifecycle::deprecate_soft("0.0.0.9011")
  
  if (is.null(inv_link)) {
    if (statistic %in% c("OR","RR","HR")) {
      message("defaulting to log link function")
      inv_link = exp
    } else {
      message("defaulting to identity link function")
      inv_link = function (x) x
    }
  }
  
  
  if (global.p) {
    out = combine_boots(boots,...,inv_link = inv_link) %>% dplyr::ungroup() %>%
      dplyr::transmute(modelName=modelName,Characteristic = group, Group = subgroup, !!statistic := statistic, `P-value` = p.value)
    
  } else {
    out = combine_boots(boots,...,inv_link = inv_link) %>% dplyr::ungroup() %>%
      dplyr::transmute(modelName=modelName,Characteristic = group, Group = subgroup, !!statistic := statistic, `P-value` = p.component)
  }
  
  multiple = dplyr::n_distinct(out$modelName) > 1
  out = out %>% tidyr::nest(table = -modelName) %>% 
    dplyr::mutate(hux = purrr::map2(modelName, table, function(.x,.y) {
      
      cgv = if (multiple) dplyr::vars(modelName) else dplyr::vars()
      rgv = if (global.p) dplyr::vars(Characteristic,`P-value`,Group) else dplyr::vars(Characteristic,Group)
      if (multiple) .y = .y %>% dplyr::mutate(modelName = .x)
      
      tmp = .y %>% dplyr::ungroup() %>% .hux_tidy(
        rowGroupVars = rgv,
        colGroupVars = cgv,
        displayRedundantColumnNames = TRUE
      )
      
      tmp = huxtable::add_columns(tmp[-2],tmp[2])
      
      
    }))
  
  #TODO: This hasn;t been properly tested in multiple models with differing
  # structures. missing values might not be aligned properly.
  init = out$hux[[1]]
  if (multiple) {
    hux2 = lapply(out$hux[-1], `[`, 3:4)
    init = Reduce(huxtable::add_columns, hux2, init)
  }
  
  return(init)
}


.pool_performance = function(boots, statistics = NULL) {
  if (inherits(boots$model[[1]], "coeftest")) {
    # unwrap the saved model if using a robust sandwich estimator
    boots = boots %>% dplyr::mutate(model = purrr::map(model, ~ attr(.x, "object")))
  }
  boots = boots %>%
    dplyr::mutate(model.perf = purrr::map(model, ~ performance::model_performance(.x) %>% as.data.frame() %>% tidyr::pivot_longer(cols=tidyselect::everything())))
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
#' `r lifecycle::badge("deprecated")`
#'
#' @keywords internal
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
  
  lifecycle::deprecate_soft("0.0.0.9011")
  
  pooled.performance = .pool_performance(boots, statistics)
}




# devtools::load_all("~/Git/tableone")
# devtools::load_all("~/Git/modelfitter")

#' Forest plot from a set of bootstrapped models
#' 
#' `r lifecycle::badge("deprecated")`
#'
#' @keywords internal
#' 
#' @param boots a set of bootstrapped model fits as output by `run_models`
#' @param facet a faceting variable (usually `modelName``)
#' @param stat_label what to call the x axis
#' @param report_fit which components of the model performance statistics do we
#'   want to report
#' @param limit x axis limits
#' @param p.component indicate which component parts of the fit are significant
#'   and which are not
#' @param label_fn a function (or lambda) that accepts an vector and returns a vector
#'
#' @return a ggplot
#' @export
plot_regression = function(boots, facet = NULL, stat_label="Odds Ratio", report_fit = FALSE, limit = c(NA,NA), p.component=FALSE, label_fn = ~ .x) {
  
  lifecycle::deprecate_soft("0.0.0.9011")
  
  facet = tryCatch(rlang::ensym(facet), error = function(e) NULL)
  facets = list(facet)
  is_facetted = is.null(facet)
  
  label_fn = purrr::as_mapper(getOption("tableone.labeller",label_fn))
  
  predictorVars = .parse_unique(boots$impute[[1]], boots$form, .side="rhs")
  keys = format_summary_rows(boots$impute[[1]], predictorVars)
  
  out1 = boots %>% combine_boots(predictorVars)
  
  
  out2 = out1 %>% 
    dplyr::group_by(group) %>%
    # striped groups
    dplyr::mutate(g = dplyr::cur_group_id() %% 2) %>%
    dplyr::group_by(group,subgroup) %>% dplyr::mutate(
      # height
      y=dplyr::cur_group_id(),
      # coefficient 
      x=exp(unname(beta.median)), 
      xmin=exp(unname(beta.lower)), 
      xmax=exp(unname(beta.upper))
    ) %>% dplyr::ungroup() %>% dplyr::mutate(
      y = max(y)-y,
    )
  
  statistic_label = stat_label
  
  groups = out2 %>% dplyr::select(!!!facets,group,g,y,x,xmin,xmax,statistic,p.value,p.component,type) %>% dplyr::mutate(
    y=y+0.1, 
    label=as.character(group), 
    label_x=0,
    x=ifelse(type=="continuous", x, NA_real_),
    xmin=ifelse(type=="continuous", xmin, NA_real_),
    xmax=ifelse(type=="continuous", xmax, NA_real_),
  ) %>% dplyr::group_by(!!!facets,group) %>% dplyr::filter(y==max(y)) %>% dplyr::ungroup()
  subgroups = out2 %>% dplyr::select(!!!facets,subgroup,g,y,x,xmin,xmax,statistic,p.value,p.component) %>% dplyr::filter(!is.na(subgroup)) %>% dplyr::mutate(label=as.character(subgroup), label_x=1)
  
  longer = dplyr::bind_rows(groups, subgroups) %>% dplyr::mutate(y=as.factor(dplyr::dense_rank(y))) %>%
    dplyr::mutate(
      p.flag = dplyr::case_when(
        p.component %>% stringr::str_starts("<") ~ "\u2020\u2020\u2020",
        suppressWarnings(as.numeric(p.component)) < 0.01 ~ "\u2020\u2020",
        suppressWarnings(as.numeric(p.component)) < 0.05 ~ "\u2020",
        TRUE ~ ""
      ),
      .facet = !!facet
    )
  
  lim_breaks = tryCatch(2^(seq(floor(log(limit[1],2)), ceiling(log(limit[2],2)), 1)), error = function(e) NULL)
  
  p1 = ggplot2::ggplot(longer, ggplot2::aes(y=y, x=x,xmin=xmin,xmax=xmax,colour=.facet))+
    ggplot2::geom_tile(mapping=ggplot2::aes(fill=as.character(g),x=1),width=Inf,colour=NA)+
    ggplot2::geom_point(position=ggstance::position_dodgev(height=0.5,preserve = "total"),size=0.4)+
    ggstance::geom_errorbarh(position=ggstance::position_dodgev(height=0.5,preserve = "total"),height=0.4)+ 
    (if(!is.null(lim_breaks)) ggplot2::scale_x_log10(breaks = lim_breaks) else ggplot2::scale_x_log10()) +
    ggplot2::coord_cartesian(xlim=limit)+
    ggplot2::geom_vline(xintercept = 1)+
    ggplot2::geom_text(data = longer %>% dplyr::select(y,!!!facets,subgroup,label,statistic) %>% dplyr::distinct(), mapping=ggplot2::aes(y=y,x=1.05,label=ifelse(!is.na(subgroup) & statistic=="ref","ref",NA)),inherit.aes = FALSE,size=.gg_label_size(5),hjust=0,colour="black")+
    ggplot2::geom_point(data = longer %>% dplyr::select(y,!!!facets,subgroup,label,statistic) %>% dplyr::filter(!is.na(subgroup) & statistic=="ref") %>% dplyr::distinct(), mapping=ggplot2::aes(y=y,x=1),inherit.aes = FALSE,colour="black")+
    .gg_hide_Y_axis()+
    ggplot2::xlab(statistic_label)+
    ggplot2::scale_fill_manual(values = c("0"="grey90","1"="white"), guide="none")+
    ggplot2::scale_color_brewer(palette = "Dark2")+
    ggplot2::facet_wrap(facets, nrow=1)+
    .gg_hide_legend()+
    .gg_set_X_angle(0)+
    if(p.component) {
      list(
        ggplot2::geom_text(data = longer %>% dplyr::filter(!is.na(p.component)), mapping=ggplot2::aes(y=y,x=0,label=p.flag),inherit.aes = FALSE,size=.gg_label_size(5),hjust=-0.1,colour="black"),
        ggplot2::labs(caption="\u2020\u2020\u2020 indicates p-value < 0.001, \u2020\u2020 p-value < 0.01 and \u2020 p-value < 0.05.")
      )
    } else {
      list()
    }
  
  if (longer %>% dplyr::filter(label==as.character(group) & !is.na(p.value)) %>% nrow() > 0) {
    p1 = p1+ggplot2::geom_text(data = longer %>% dplyr::filter(label==as.character(group) & !is.na(p.value)) %>% dplyr::mutate(label=paste0("(p: ",p.value,") ")), mapping=ggplot2::aes(y=y,x=Inf,label=label),inherit.aes = FALSE,size=.gg_label_size(5),hjust=1,colour="black")
  }
  
  p2 = ggplot2::ggplot(longer %>% dplyr::select(y,g,label_x,label) %>% dplyr::distinct(),ggplot2::aes(y=y))+
    ggplot2::geom_tile(mapping=ggplot2::aes(fill=as.character(g),x=1),width=Inf,colour=NA)+
    ggplot2::geom_text(mapping=ggplot2::aes(x=label_x, hjust=label_x, label=label),size=.gg_label_size(6))+
    ggplot2::coord_cartesian(xlim=c(0,1))+
    ggplot2::theme_void()+ggplot2::scale_fill_manual(values = c("0"="grey90","1"="white"), guide="none")
  
  
  if (!isFALSE(report_fit)) {
    perf = boots %>% .pool_performance(statistics = report_fit)
    perf2 = perf %>% dplyr::group_by(statistic) %>% dplyr::mutate(y=dplyr::cur_group_id(),g = dplyr::cur_group_id() %% 2) %>% dplyr::ungroup() %>% dplyr::mutate(y=max(y)-y)
    
    p3 = ggplot2::ggplot(perf2 %>% dplyr::select(statistic,y,g) %>% dplyr::distinct(), ggplot2::aes(y=y,label=statistic,x=1))+
      ggplot2::geom_tile(mapping=ggplot2::aes(fill=as.character(g)),width=Inf,colour=NA)+
      ggplot2::scale_fill_manual(values = c("0"="grey90","1"="white"), guide="none")+
      ggplot2::geom_text(size=.gg_label_size(6), hjust=1)+ggplot2::theme_void()+
      ggplot2::coord_cartesian(xlim = c(0,1))
    
    p4 = ggplot2::ggplot(perf2, ggplot2::aes(y=y,label=value,x=0))+
      ggplot2::geom_tile(mapping=ggplot2::aes(fill=as.character(g),x=0),width=Inf,colour=NA)+
      ggplot2::geom_text(size=.gg_label_size(6))+
      ggplot2::scale_fill_manual(values = c("0"="grey90","1"="white"), guide="none")+
      ggplot2::facet_wrap(facets,nrow=1)+.gg_hide_X_axis()+.gg_hide_Y_axis()+
      #ggplot2::theme_void()
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),axis.ticks.y = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(), strip.text = ggplot2::element_blank(), strip.background = ggplot2::element_blank())
    
    height_ratio= c(length(levels(longer$y)),length(report_fit))
    
    p = p2+p1+p3+p4+patchwork::plot_layout(ncol=2,nrow=2,widths = c(2.5,10),heights = height_ratio)
    
  } else {
    
    p = p2+p1+patchwork::plot_layout(ncol=2,nrow=1,widths = c(2.5,10))
    
  }
  
  p
}