# The general process of fitting a single model:
# * decide on model type (logistic etc)
# * construct model formula
# * prepare data
# * fit model to data
# * check behaviour / fit quality
# * extract coefficients (model term)
# * format them & transform
# * extract global p-values (predictors)
# * extract quality of fit metrics (model)
# * tabulate or plot

# Dimensions to scale over
# * decide on model type: (logistic / poisson) (M model types)
# * construct model formula: multiple univariate models + multivaraite adjusted models  (N formulae - T(N) terms, P(N) predictors)
# * prepare data: Missing data bootstrapping (O bootstraps) also though: 
# * Subsets of data for sensitivity analyses - concept of data filters as function or better data providers? (S subsets)
# * fit model to data: (M*N*O*S) fitted models
# * check behaviour / fit quality: one per fit
# * extract coefficients (model term): one per fit: M*T(N)*O*S
# * summarise model term coefficients over bootstraps: M*T(N)*S
# * format them & transform : M*T(N)*S
# * extract global p-values (predictors) M*P(N)*O*S
# * summarise global p-values (predictors) over bootstraps (M*P(N)*S
# * extract quality of fit metrics (per fit) (M*N*O*S)
# * summarise quality of fit over bootstraps (M*N*S)
# * tabulate or plot - each plot contains potentially multiple model formulae in 
# columns (Univ / Adj 1 / Undj 2 etc) + P(N) and (row groups) + T(N) (rows)
# results in M*S plots or tables most likely. Not all combinations of M and S are
# likely needed.


# Formulae ----

#' Construct a configuration for multiple formulae
#' 
#' multiple formulae for a parameterised model fitting pipeline. Different models
#' may be a key part of the analysis or a sensitivity. A single name maybe associated
#' with one or many models - This is because it is frequent that we want to group 
#' together models such as univariate comparisons into the same format table as
#' another single multivarible model. Another use case for multiple models
#' is a single set of predictors 
#' 
#' @param ... a named list of formulae or formulae lists. In the simplest use
#'   case this is a single formula, see examples for more possibilities.
#'
#' @export
#' @examples
#' form = is_coloured ~ cut + carat + clarity * price
#' fp = formula_provider(
#'   `Univariate` = modelfitter::univariate_from_multivariate(form),
#'   `Adj Model 1` = form,
#'   `Adj Model 2` = update(form, . ~ . - clarity * price),
#' )
#' 
#' 
#' predictors = ~ x + y + z 
#' fp2 = formula_provider(
#'   `Death` = death ~ .,
#'   `ICU admission` = icu ~ .,
#'   `Delayed discharge` = delayed_discharge ~ .,
#' )
#' sapply(fp2(), fp2) %>% sapply(update, predictors)
#' 
#' # the simplest configuration
#' fp3 = formula_provider(outcome ~ x + y + z)
#' fp3()
#' 
#' fp4 = formula_provider(outcome1 ~ x + y + z, outcome2 ~ x + y + z)
#' fp4()
#' try(fp4("blah blah"))
#' 
#' # must define at least one forumula
#' try(formula_provider())
#' 
#' formula_provider(default = . ~ .)
formula_provider = function(...) {
  dots = rlang::list2(...)
  if (length(dots) == 0) stop("you must supply a model formula.",call. = FALSE)
  if (is.null(names(dots)) && length(dots)==1) names(dots)="default"
  dots = rlang::exprs_auto_name(dots)
  return(.prov(function(name) {
    if (rlang::is_missing(name)) return(names(dots))
    tmp = .check_index(dots, name, "formula_provider")
    if (!inherits(tmp,"list")) tmp = list(tmp)
    return(tmp)
  }))
}

# Model functions ----

#' Construct a configuration for multiple statistical models
#' 
#' multiple statistical models for a parameterised model fitting pipeline.
#' 
#' @param ... a named list of functions. In the simplest use
#'   case this is a single function, see examples for more possibilities.
#'   the functions must accept `data` as first parameter and `formula` as second.
#'
#' @export
#' @examples
#' # in the simplest version the name is pulled from the input
#' mfp = model_function_provider(logistic_regression)
#' mfp
#' mfp()
#' 
#' mfp = model_function_provider(logistic_regression, quasi_poisson)
#' mfp()
#' 
#' mfp2 = model_function_provider(
#'    Logistic = modelfitter::logistic_regression,
#'    Poisson = modelfitter::quasi_poisson
#' )
#' mfp2()
#'  
#' # the functions are named the other way round to normal model functions
#' # this was by design to fit into a tidy pipeline:
#' mfp = model_function_provider(linear = ~ stats::lm(.y,.x))
#' linear_model = mfp("linear")
#' iris %>% linear_model(Petal.Length ~ Petal.Width)
model_function_provider = function(...) {
  dots = rlang::list2(...)
  if (length(dots) == 0) stop("you must supply a model function.",call. = FALSE)
  if (is.null(names(dots))) names(dots) = format(rlang::enexprs(...))
  dots = rlang::exprs_auto_name(dots,repair_auto = "unique",repair_quiet = TRUE)
  dots = lapply(dots, rlang::as_function)
  
  return(.prov(function(name) {
    if (rlang::is_missing(name)) return(names(dots))
    tmp = .check_index(dots, name, "model_function_provider")
    return(tmp)
  }))
}

# Data set boostraps ----

#' Provide a access to a dataset
#'
#' @param data the data frame
#' @param formulae a list of formulae with all the columns used
#'
#' @return a function that returns a dataset
#' @export
raw_data_provider = function(data, formulae = ~ .) {
  return(bootstrap_provider(data, max_n=1, formulae = formulae))
}

#' Provide a access to bootstrap resamples of a dataset
#'
#' @param data the data frame
#' @param formulae a list of formulae with all the columns used
#' @param max_n the maximum number of different bootstraps
#'
#' @return a function that returns a dataset for inputs between `1:max_n`
#' @export
#'
#' @examples
#' bp = iris %>% bootstrap_provider(10)
#' bp(1) %>% head()
#' tmp = bp(1)
#' tmp2 = bp(1)
#' identical(tmp,tmp2)
bootstrap_provider = function(data, max_n, formulae = ~ .) {
   
  vars = lapply(formulae, all.vars) %>% unlist() %>% unique()
  if (any(vars==".")) vars = colnames(data)
  
  if (length(setdiff(vars, colnames(data)))>0) {
    warning("Unmatched columns in formulae")
    vars = vars[vars %in% colnames(data)]
  }
  
  if (is.provider(formulae)) formulae = .get_all(formulae)
  data2 = data %>% dplyr::select(dplyr::all_of(vars))
  
  return(.prov(function(i) {
    if (rlang::is_missing(i)) return(1:max_n)
    if (max_n == 1) return(data)
    oldseed = try(.Random.seed, silent = TRUE)
    set.seed(i)
    tmp = dplyr::sample_n(data, nrow(data),replace=TRUE)
    if (!inherits(oldseed,"try-error")) .Random.seed = oldseed
    return(tmp)
  }))
}

#' Provide access to missing data imputations
#'
#' @param data the data frame with missing rows
#' @param formulae a lit of formulae with all the columns used (defaults to everything)
#' @param max_n the maximum number of different imputations to 
#' @param ... cache control
#'
#' @return a function that returns a dataset for inputs between `1:max_n`
#' @export
#'
#' @examples
#' ip = imputation_provider(mice::nhanes2, 10, list(~ hyp + bmi, ~ age + chl))
#' ip(1) %>% head(10)
#' 
imputation_provider = function(data, max_n,  formulae = ~ ., ...) {
  if (!inherits(formulae,"list")) formulae = list(formulae)
  predictors = lapply(formulae, rlang::f_rhs) %>% lapply(all.vars) %>% unlist() %>% unique()
  if (any(predictors==".")) predictors = colnames(data)
  
  imputeModel = .cached({
    message("imputing missing data...")
    imputeFrom = data %>% dplyr::select(c(tidyselect::where(is.numeric),tidyselect::where(is.factor),tidyselect::where(is.logical))) %>% colnames()
    # Configure mice imputation columns
    init = mice::mice(data, maxit=0)
    meth = init$method
    predM = init$predictorMatrix
    # only impute values in the model:
    meth[!(names(meth) %in% predictors)] = ""
    # don't use for imputation
    # predM[!(names(meth) %in% predictorNames)] = 0
    # only use factors and numerics for imputation
    predM[!(names(meth) %in% imputeFrom)] = 0
    oldseed = try(.Random.seed,silent = TRUE)
    set.seed(103)
    tmp = mice::mice(data, method=meth, predictorMatrix=predM, m=max_n, maxit = 5,printFlag=FALSE)
    if (!inherits(oldseed,"try-error")) .Random.seed = oldseed
    tmp
  }, data, predictors, ...)
  
  return(.prov(function(i) {
    if (rlang::is_missing(i)) return(1:max_n)
    tmp = mice::complete(imputeModel,i)
    return(tmp)
  }))
}


# Data sensitivity subgroups ----

#' Provide a mechanism for subsetting data 
#' 
#' A data subset may be used for a sensitivity analysis. This provides a 
#' mechanism to filter the data within the pipeline. 
#'
#' @param ... a set of named data filter criteria. In most cases you will want 
#'   to include a `default = TRUE` option in this list. (this is the default value)
#'
#' @return a data subset provider which can fiter data based on a named set of subset criteria
#' @export 
#'
#' @examples
#' dsp = data_subset_provider(one = TRUE, two = Species == "versicolor", three = Sepal.Width < 2.6)
#' dsp()
#' f = dsp("three")
#' f(iris) 
#' 
#' dsp2 = data_subset_provider()
#' dsp2("default")(mtcars) %>% head(10)
data_subset_provider = function(...) {
  dots = rlang::enexprs(...)
  if (length(dots) == 0) dots = list(default = TRUE)
  if (is.null(names(dots))) names(dots) = format(rlang::enexprs(...))
  dots = rlang::exprs_auto_name(dots,repair_auto = "unique",repair_quiet = TRUE)
  dots = lapply(dots, function(x) {
    function(data) data %>% 
      dplyr::ungroup() %>%
      dplyr::filter(!!x) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.factor), forcats::fct_drop))
  })
  
  return(.prov(function(name) {
    if (rlang::is_missing(name)) return(names(dots))
    tmp = .check_index(dots, name, "data_subset_provider")
    return(tmp)
  }))
  
}




# Provider utils ----

.get_all = function(x) {
  sapply(x(), x)
}

is.provider = function(x,...) {
  inherits(x,"provider")
}

format.provider = function(x, ...) {
  tmp = x()
  if (is.numeric(tmp)) return(sprintf("range: 1..%d", max(tmp)))
  return(paste0("`",tmp,"`",collapse=", "))
}

print.provider = function(x,...) {
  cat(format.provider(x))
}

.prov = function(f) {
  return(structure(f,class = c("provider",class(f))))
}


.check_index = function(lst, name, func_name) {
  if (is.numeric(name)) {
    if (name < 1 || name > length(lst)) stop(
      sprintf("%s: index out of range: (max %d)", func_name,length(lst)),call. = FALSE)
    return(lst[[name]])
  }
  if (!name %in% names(lst)) stop(
    sprintf("%s: `%s` is not a known index of out: %s",
            func_name, name,paste0(names(lst),collapse = ", ")),call. = FALSE)
  return(lst[[name]])
}