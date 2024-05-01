#' @importFrom stats lm
NULL

## Model standard interfaces ----

#' Fit standard cox model.
#'
#' @param data a data frame
#' @param formula a formula of the form `survival::Surv(time, event) ~ obs1 + obs2 + ...`
#' @param ... not used
#'
#' @return a model object
#' @export
cox_model = function(data, formula, ...) {
  data = data %>%
    .fix_ordered_predictors(formula)
  return(survival::coxph(formula,data,...,model = TRUE))
}

#' Fit standard logistic regression.
#' 
#' This makes an effort to cast the result column into a logical from a factor or 
#' other data type. It also will model ordered factors as contrasts rather than as
#' a polynomial expansion, allowing an interpretable output from ordered factors.
#'
#' @param data a data frame
#' @param formula a formula of the form `binary_outcome ~ obs1 + obs2 + ...`
#' @param positive test strings to interpret as true if outcome is not a factor or logical.
#' @param ... not used
#'
#' @return a model object
#' @export
#' @examples
#' 
#' diamonds3 = ggplot2::diamonds %>% dplyr::mutate(
#'   is_coloured = color <= "F",
#'   cut = factor(cut,ordered=FALSE),
#'   price_cat = cut(price, 
#'       breaks = c(0,500,1000,2000,4000,Inf),
#'       labels = c("<500","500-999","1000-1999","2000-3999","4000+"),
#'       ordered_result = TRUE)
#' ) %>% dplyr::select(-color)
#' 
#' model5 = diamonds3 %>% 
#'   logistic_regression_ordered(is_coloured ~ cut + carat + clarity * price)
#' 
logistic_regression_ordered = function(data, formula, positive=c("yes","true","present","confirmed"), ...) {
  
  data = data %>% .fix_binary_outcome(formula, positive)
  stats::glm(
    formula = formula, 
    family = stats::binomial(), 
    data = data,
    contrasts = .ordered_contrasts(data,formula))
}

#' Fit standard logistic regression with unordered factors.
#' 
#' This makes an effort to cast the result column into a logical from a factor or 
#' other data type. It also will convert ordered predictors into unordered before
#' running.
#'
#' @param data a data frame
#' @param formula a formula of the form `binary_outcome ~ obs1 + obs2 + ...`
#' @param positive test strings to interpret as true if outcome is not a factor or logical.
#' @param ... not used
#'
#' @return a model object
#' @export
#' @examples
#' 
#' diamonds3 = ggplot2::diamonds %>% dplyr::mutate(
#'   is_coloured = color <= "F",
#'   cut = factor(cut,ordered=FALSE),
#'   price_cat = cut(price, 
#'       breaks = c(0,500,1000,2000,4000,Inf),
#'       labels = c("<500","500-999","1000-1999","2000-3999","4000+"),
#'       ordered_result = TRUE)
#' ) %>% dplyr::select(-color)
#' 
#' model5 = diamonds3 %>% 
#'   logistic_regression(is_coloured ~ cut + carat + clarity * price)
#' model6 = ggplot2::diamonds %>% 
#'   logistic_regression(I(color <= "F") ~ cut + carat + clarity * price)
#' 
#' # this wont work as the factors are converted to unordered
#' # model6 = ggplot2::diamonds %>% 
#' #   logistic_regression(I(color <= "F") ~ I(cut<"Very Good") + carat + clarity * price)
#' 
#' 
logistic_regression = function(data, formula, positive=c("yes","true","present","confirmed"), ...) {
  
  data = data %>% 
    .fix_binary_outcome(formula, positive) %>% 
    .fix_ordered_predictors(formula)
  
  stats::glm(
    formula = formula, 
    family = stats::binomial(), 
    data = data
  )
}

#' Fit exact logistic regression
#'
#' @param data a data frame
#' @param formula a formula of the form `binary_outcome ~ obs1 + obs2 + ...`
#' @param positive test strings to interpret as true if outcome is not a factor or logical.
#' @param ... not used
#'
#' @return a model object
exact_logistic_regression = function(data, formula, positive=c("yes","true","present","confirmed"), ...) {
  
  data = data %>% 
    .fix_binary_outcome(formula, positive) %>% 
    .fix_ordered_predictors(formula)
  
  # TODO: this is problematic as only seems to work with binary data.
  # probably everything needs one-hot encoding
  browser()
  data2 = data %>% dplyr::select(dplyr::any_of(all.vars(formula))) %>%
    dplyr::group_by(dplyr::across(-!!outcome)) %>% 
    dplyr::summarise(.pos = sum(!!outcome), .n = dplyr::n())
  formula2 = stats::update(formula, .pos/.n ~ .)
  
  elrm::elrm(formula = formula2, interest = rlang::f_rhs(formula2), dataset = data2)
}



#' Fit standard log binomial regression using `stats::glm`
#' 
#' The log binomial model using standard glm is less that satisfactory and 
#' hard to make converge. The algorithm needs starting values to have any hope
#' and these do make a difference to the outcome. This needs more investigation 
#' before being used.
#'
#' @param data a data frame
#' @param formula a formula of the form `binary_outcome ~ obs1 + obs2 + ...`
#' @param positive test strings to interpret as true if outcome is not a factor or logical.
#' @param ... not used
#'
#' @return a model object
#' @export
#' @examples
#' 
#' diamonds3 = ggplot2::diamonds %>% dplyr::mutate(
#'   is_coloured = color <= "F",
#'   cut = factor(cut,ordered=FALSE),
#'   price_cat = cut(price, 
#'       breaks = c(0,500,1000,2000,4000,Inf),
#'       labels = c("<500","500-999","1000-1999","2000-3999","4000+"),
#'       ordered_result = TRUE)
#' ) %>% dplyr::select(-color)
#' 
#' model5 = diamonds3 %>% log_binomial(is_coloured ~ cut + carat + clarity * price)
#' global_p_value(model5)
log_binomial = function(data, formula,  positive=c("yes","true","present","confirmed"), ...) {
  #browser()
  data = data %>% 
    .fix_binary_outcome(formula, positive) %>% 
    .fix_ordered_predictors(formula)
  
  outcome = rlang::f_lhs(formula)
  frac = data %>% dplyr::transmute(x = !!outcome) %>% dplyr::pull(x) %>% mean(na.rm=TRUE)
  nterms = suppressWarnings(length(stats::coefficients(stats::lm(formula,data))))
  starts = c(log(frac),rep(0,nterms-1))
  
  stats::glm(
    formula = formula, 
    data = data, 
    family=stats::binomial(link="log"), 
    start=starts,
    ...
  )
}

#' Fit standard log binomial regression using `logbin::logbin`
#' 
#' This can be very slow for simple models, and will not handle interaction terms
#' It does not seem to report std error. This is very much a work in progress,
#'
#' @param data a data frame
#' @param formula a formula of the form `binary_outcome ~ obs1 + obs2 + ...`
#' @param positive test strings to interpret as true if outcome is not a factor or logical.
#' @param ... not used
#'
#' @return a model object
#' @export
#' @examples
#' 
#' diamonds3 = ggplot2::diamonds %>% dplyr::mutate(
#'   is_coloured = color <= "F",
#'   cut = factor(cut,ordered=FALSE),
#'   price_cat = cut(price, 
#'       breaks = c(0,500,1000,2000,4000,Inf),
#'       labels = c("<500","500-999","1000-1999","2000-3999","4000+"),
#'       ordered_result = TRUE)
#' ) %>% dplyr::select(-color)
#' 
#' model5 = diamonds3 %>% log_binomial_2(is_coloured ~ cut + carat + clarity + price)
#' global_p_value(model5)
log_binomial_2 = function(data, formula,  positive=c("yes","true","present","confirmed"), ...) {
  
  data = data %>% 
    .fix_binary_outcome(formula, positive,num = TRUE) %>% 
    .fix_ordered_predictors(formula)
  
  outcome = rlang::f_lhs(formula)
  frac = data %>% dplyr::transmute(x = !!outcome) %>% dplyr::pull(x) %>% mean(na.rm=TRUE)
  nterms = suppressWarnings(length(stats::coefficients(stats::lm(formula,data))))
  starts = c(log(frac),rep(0,nterms-1))
  
  logbin::logbin(formula = formula, data = data, start=starts,
                 maxit = 10000, method = "em", accelerate = "squarem"
  )
}

#' Fit standard poisson regression
#' 
#' This is roughly equivalent to a log_binomial model but converges well. Its 
#' output can be interpreted at a RR. It is using a stats::glm with a log link function 
#' and a family of quasipoisson. The binary outcome is interpreted as a count 
#' where true is 1 and false is 0.
#'
#' @param data a data frame
#' @param formula a formula of the form `binary_outcome ~ obs1 + obs2 + ...`
#' @param positive test strings to interpret as true if outcome is not a factor or logical.
#' @param ... not used
#'
#' @return a model object
#' @export
#' @examples
#' 
#' diamonds3 = ggplot2::diamonds %>% dplyr::mutate(
#'   is_coloured = color <= "F",
#'   cut = factor(cut,ordered=FALSE),
#'   price_cat = cut(price, 
#'       breaks = c(0,500,1000,2000,4000,Inf),
#'       labels = c("<500","500-999","1000-1999","2000-3999","4000+"),
#'       ordered_result = TRUE)
#' ) %>% dplyr::select(-color)
#' 
#' model5 = diamonds3 %>% quasi_poisson(is_coloured ~ cut + carat + clarity + price)
#' global_p_value(model5)
quasi_poisson = function(data, formula,  positive=c("yes","true","present","confirmed"), ...) {
  data = data %>% 
    .fix_binary_outcome(formula, positive,num = TRUE) %>% 
    .fix_ordered_predictors(formula)
  
  stats::glm(formula = formula, family = stats::quasipoisson(link = log), data = data)
}


#' Fit standard poisson regression with ordered contrasts
#' 
#' This is roughly equivalent to a log_binomial model but converges well. Its 
#' output can be interpreted at a RR. It is using a stats::glm with a log link function 
#' and a family of quasipoisson. The binary outcome is interpreted as a count 
#' where true is 1 and false is 0. Ordered variables are represented as contrasts
#' between adjacent levels
#'
#' @param data a data frame
#' @param formula a formula of the form `binary_outcome ~ obs1 + obs2 + ...`
#' @param positive test strings to interpret as true if outcome is not a factor or logical.
#' @param ... not used
#'
#' @return a model object
#' @export
#' @examples
#' 
#' diamonds3 = ggplot2::diamonds %>% dplyr::mutate(
#'   is_coloured = color <= "F",
#'   cut = factor(cut,ordered=FALSE),
#'   price_cat = cut(price, 
#'       breaks = c(0,500,1000,2000,4000,Inf),
#'       labels = c("<500","500-999","1000-1999","2000-3999","4000+"),
#'       ordered_result = TRUE)
#' ) %>% dplyr::select(-color)
#' 
#' model5 = diamonds3 %>% quasi_poisson_ordered(is_coloured ~ cut + carat + clarity + price)
#' summary(model5)
quasi_poisson_ordered = function(data, formula,  positive=c("yes","true","present","confirmed"), ...) {
  data = data %>% 
    .fix_binary_outcome(formula, positive,num = TRUE)
  
  stats::glm(formula = formula, family = stats::quasipoisson(link = log), data = data,
             contrasts = .ordered_contrasts(data,formula))
}

#' Fit robust poisson regression using sandwich estimators
#' 
#' This is roughly equivalent to a log_binomial model but converges well. Its
#' output can be interpreted at a RR. It is using a glm with a log link function
#' and a family of poisson with robust sandwich estimators. The binary outcome
#' is interpreted as a count where true is 1 and false is 0.
#' 
#' heteroskedasticity-consistent (HC) standard errors using sandwich:
#' https://data.library.virginia.edu/understanding-robust-standard-errors/
#'
#' @param data a data frame
#' @param formula a formula of the form `binary_outcome ~ obs1 + obs2 + ...`
#' @param positive test strings to interpret as true if outcome is not a factor or logical.
#' @param ... not used
#'
#' @return a model object
#' @export
#' @examples
#' 
#' diamonds3 = ggplot2::diamonds %>% dplyr::mutate(
#'   is_coloured = color <= "F",
#'   cut = factor(cut,ordered=FALSE),
#'   price_cat = cut(price, 
#'       breaks = c(0,500,1000,2000,4000,Inf),
#'       labels = c("<500","500-999","1000-1999","2000-3999","4000+"),
#'       ordered_result = TRUE)
#' ) %>% dplyr::select(-color)
#' 
#' model5 = diamonds3 %>% robust_poisson(is_coloured ~ cut + carat + clarity + price)
#' global_p_value(model5)
robust_poisson = function(data, formula,  positive=c("yes","true","present","confirmed"), ...) {
  data = data %>% 
    .fix_binary_outcome(formula, positive,num = TRUE) %>% 
    .fix_ordered_predictors(formula)
  
  m = stats::glm(formula = formula, data = data, family = stats::poisson(link = "log"))
  lmtest::coeftest(m, vcov. = sandwich::vcovHC(m, type="HC1"), save = TRUE)
}

#' Fit robust poisson regression using `geepack::glm`
#' 
#' equivalent to a log_binomial model
#'
#' @param data a data frame
#' @param formula a formula of the form `binary_outcome ~ obs1 + obs2 + ...`
#' @param positive test strings to interpret as true if outcome is not a factor or logical.
#' @param ... not used
#'
#' @return a model object
#' @export
#' @examples
#' 
#' diamonds3 = ggplot2::diamonds %>% dplyr::mutate(
#'   is_coloured = color <= "F",
#'   cut = factor(cut,ordered=FALSE),
#'   price_cat = cut(price, 
#'       breaks = c(0,500,1000,2000,4000,Inf),
#'       labels = c("<500","500-999","1000-1999","2000-3999","4000+"),
#'       ordered_result = TRUE)
#' ) %>% dplyr::select(-color)
#' 
#' model5 = diamonds3 %>% robust_poisson_2(is_coloured ~ cut + carat + clarity + price)
#' global_p_value(model5)
robust_poisson_2 = function(data, formula,  positive=c("yes","true","present","confirmed"), ...) {
  data = data %>% 
    .fix_binary_outcome(formula, positive,num = TRUE) %>% 
    .fix_ordered_predictors(formula)
  
  geepack::geeglm(formula = formula,
                  data    = data %>% dplyr::mutate(.seqno = dplyr::row_number()),
                  family  = stats::poisson(link = "log"),
                  id      = .seqno,
                  corstr  = "exchangeable")
}


# Utils ----

.fix_binary_outcome = function(df, formula, positive, num = FALSE) {
  outcome = rlang::f_lhs(formula)
  if (rlang::as_label(outcome) %in% colnames(data)) {
    df = df %>% dplyr::mutate(!!outcome := .make_logical(!!outcome, positive))
    if (num) {
      df = df %>% dplyr::mutate(!!outcome := as.numeric(!!outcome))
    }
  }
  return(df)
}

# .make_logical(iris$Species)
.make_logical = function(x, ref = levels(x)[1]) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x != 0)
  if (is.character(x) && !any(x %in% ref)) stop("Not able to convert to logical")
  x = factor(x)
  levels(x) = tolower(levels(x))
  ref = tolower(ref)
  ref = ref[ref %in% levels(x)]
  if (length(ref)==0) ref = levels(x)[1]
  if (length(ref)!=1) stop("More than one value matches true condition")
  if (length(levels(x) != 2)) {
    rlang::warn(sprintf("More than 2 levels found in logical value: %s",paste0(levels(x),collapse=", ")),.frequency="once",.frequency_id = paste0(levels(x),collapse=""))
  }
  return(x == ref)
}

# Gives a list suitable to passing to stats::glm(contrasts=...) to control ordered factors
# .ordered_contrasts(ggplot2::diamonds)
.ordered_contrasts = function(df, formula) {
  cols = df %>% dplyr::select(dplyr::where(is.ordered)) %>% colnames()
  cols = cols[cols %in% all.vars(rlang::f_rhs(formula))]
  sapply(cols, function(...) MASS::contr.sdif, )
}



# .fix_ordered_predictors(ggplot2::diamonds, cut ~ color) %>% glimpse()
.fix_ordered_predictors = function(df, formula) {
  vars = all.vars(rlang::f_rhs(formula))
  if (any(vars==".")) vars = setdiff(colnames(df),all.vars(rlang::f_lhs(formula)))
  df %>% dplyr::mutate(
    dplyr::across(dplyr::where(is.ordered) & dplyr::all_of(vars), 
      ~ factor(.x,ordered=FALSE))
                       )
}