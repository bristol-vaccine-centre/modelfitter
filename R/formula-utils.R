# package depends
# c("tidyselect","rlang") %>% lapply(usethis::use_package)

# look for a dataframe as the first argument of a function in the call stack
# z = function(a) {.search_call_stack()}
# y = function(a) {a}
# x = function(df, a_default="a") {return(y(y(y(z()))))}
# x(iris)
.search_call_stack = function(nframe = sys.nframe()-1) {
  frame = sys.frame(nframe)
  first_arg_name = names(formals(sys.function(nframe)))[[1]]
  try({
    data = suppressWarnings(get(first_arg_name, envir=frame))
    if(is.data.frame(data)) return(data)
  },silent = TRUE)
  nframe = nframe-1
  if (nframe < 1) stop("no data frame found")
  .search_call_stack(nframe)
}


#' Reuse tidy-select syntax outside of a tidy-select function
#'
#' @param tidyselect a tidyselect syntax which will be evaluated in context by looking for a call in the call stack that includes a dataframe as the first argument
#' @param data (optional) a specific dataframe with which to evaluate the tidyselect
#'
#' @return a list of symbols resulting from the evaluation of the tidyselect in the context of the current call stack (or a provided data frame)
#' @export
as_vars = function(tidyselect, data=NULL) {
  expr = rlang::enquo(tidyselect)
  if(is.null(data)) data = .search_call_stack()
  res = tidyselect::eval_select(expr,data)
  lapply(names(res), as.symbol)
}

#' Convert multivariate formula to list of univariate formulae
#'
#' @param formula a formula of the type y ~ x1 + x2 + x3 + ....
#'
#' @return a list of formulae of the type y ~ x1, y ~ x2, y ~ x3, ....
#' @export
#' @examples
#' univariate_from_multivariate(y ~ x1 + x2 + x3)
#' univariate_from_multivariate(~ x1 + x2 + x3)
univariate_from_multivariate = function(formula) {
  tmp = stats::terms(formula)
  rhs = rlang::f_rhs(formula) %>% all.vars()
  lhs = rlang::f_lhs(formula) %>% format()
  if (lhs != "NULL") {
    lapply(rhs, function(x) stats::as.formula(sprintf("%s ~ `%s`",lhs,x)))
  } else {
    lhs = rhs[[1]]
    rhs = rhs[-1]
    lapply(rhs, function(x) stats::as.formula(sprintf("~ `%s` + `%s`",lhs,x)))
  }
}

#' Get all used variables on LHS of a formula
#'
#' @param formula a formula or list of formulae
#'
#' @return the variables on the LHS of the formulae as a character vector
#' @export
#'
#' @examples
#' f_rhs_vars(survival::Surv(time,event) ~ x + z +y)
#' f_rhs_vars(survival::Surv(time,event) ~ pspline(x) + z + y)
f_rhs_vars = function(formula) {
  if (is.list(formula)) return(purrr::list_c(lapply(formula, f_rhs_vars)) %>% unique())
  return(all.vars(rlang::f_rhs(formula)))
}

#' Get all used variables on LHS of a formula
#'
#' @param formula a formula or list of formulae
#'
#' @return the variables on the RHS of the formulae as a character vector
#' @export
#'
#' @examples
#' f_lhs_vars(survival::Surv(time,event) ~ x + z + y)
f_lhs_vars = function(formula) {
  if (is.list(formula)) return(unique(purrr::list_c(lapply(formula, f_lhs_vars))))
  f = formula %>% 
    rlang::f_lhs() %>% 
    format() %>%
    sprintf(fmt="~ %s") %>%
    stats::as.formula()
  return(f_rhs_vars(f))
}

# .parse_formulae(iris, ~ Species + Petal.Width + Missing, a ~ b+Sepal.Width)
# .parse_formulae(iris, Species ~ Petal.Width + Missing, a ~ b+Sepal.Width, side="lhs")
# .parse_formulae(iris, list(Species ~ Petal.Width + Missing, a ~ b+Sepal.Width), side="rhs")
# .parse_formulae(iris, c(Species ~ Petal.Width + Missing, a ~ b+Sepal.Width), side="rhs")
# form =  ~ Species + Petal.Width + Missing
# form2 =  ~ Species + Sepal.Width
# .parse_formulae(iris, list(form,form2))
# .parse_formulae(iris, Species ~ .) # everything except species
# .parse_formulae(iris, dplyr::vars(Sepal.Width,b,Sepal.Length)) %>% purrr::discard(~ is.null(.x) | length(.x) == 0) %>% lapply(`[[`,1)
.parse_formulae = function(df, ..., side="rhs") {
  list_form = unlist(rlang::list2(...)) #unlist required to support list input
  lapply(list_form, function(form) {

    if (side == "lhs") {
      vars = f_lhs_vars(form)
    } else if (side == "rhs") {
      vars = f_rhs_vars(form)
      if (any(vars == c("."))) vars = setdiff(colnames(df),f_lhs_vars(form))
    } else {
      vars = unique(c(f_rhs_vars(form),f_lhs_vars(form)))
      if (any(vars == c("."))) vars = colnames(df)
    }

    wronguns = setdiff(vars, colnames(df))
    if (length(wronguns) > 0) warning("Removing variables in formula but not in dataframe: `", wronguns %>% paste0(collapse = " + "), "`; formula was: `", rlang::as_label(form), "`")
    vars = intersect(vars, colnames(df))
    vars = vars %>% sapply(as.symbol, USE.NAMES = FALSE)
    return(vars)
  })
}

# .parse_tidyselect(iris,tidyselect::everything())
.parse_tidyselect = function(df, ...) {
  # zero inputs and formulae should have been dealt with.
  # anything else is a tidyselect error?
  # evaluate as a tidyselect
  expr = rlang::expr(c(...))
  pos = tidyselect::eval_select(expr, data = df)
  cols = colnames(df)[pos]
  cols = cols %>% sapply(as.symbol, USE.NAMES = FALSE)
  return(cols)
}

# works for a single formula or a tidyselect input.
# where dots is either a function (in which case we only want rhs) or a tidyselect.
# .parse_vars(iris, tidyselect::everything())
# .parse_vars(iris, ~ Species + Petal.Width + Missing)
# .parse_vars(iris, dplyr::vars(Sepal.Width, b, Sepal.Length))
# form =  ~ Species + Petal.Width + Missing
# form2 =  ~ Species + Sepal.Width
# .parse_vars(iris, form)
# .parse_vars(iris, list(form,form2))
# .parse_vars(iris, "Petal.Width", "b", "Sepal.Width")
# .parse_vars(iris, Species ~ Petal.Width + Missing)
.parse_vars = function(df, ..., .side="rhs") {
  if (.is_character_list(...)) {
    return(.parse_character(...))
  }
  if (.is_vars_interface(...)) {
    return(c(...) %>% sapply(rlang::as_label) %>% lapply(as.symbol))
  }
  if (.is_formula_interface(...)) {
    list_vars = .parse_formulae(df, ..., side = .side)
    if (length(list_vars) == 0) stop("No columns given: please supply a formula or a tidyselect expression e.g. `tidyselect::everything()`")
    if (length(list_vars) > 1) {
      warning("This function only supports single formulae or multiple formulae with single item on RHS in input. We are only using the first one.")
      return(list_vars[[1]])
    }
    return(list_vars[[1]])
  } else {
    return(.parse_tidyselect(df,...))
  }
}

.sort_symbols = function(symbols) {
  s = order(sapply(symbols, rlang::as_label))
  symbols[s]
}

# all variables in df and in one of: a set of characters, a set of formulae,
# a tidyselect spec or a dplyr::vars() call.
# .parse_unique(iris, Sepal.Width ~ Species + Sepal.Length, Sepal.Width ~ Species + Petal.Length)
# .parse_unique(iris, c(Sepal.Width ~ Species + Sepal.Length, Sepal.Width ~ Species + Petal.Length))
# .parse_unique(iris, list(Sepal.Width ~ Species + Sepal.Length, Sepal.Width ~ Species + Petal.Length))
# .parse_unique(iris, list(Sepal.Width ~ Species + Sepal.Length, Sepal.Width ~ Species + Petal.Length), .side="all")
# .parse_unique(iris, tidyselect::everything())
# .parse_unique(iris %>% dplyr::group_by(Species), tidyselect::everything(), .side="both")
# .parse_unique(iris %>% dplyr::group_by(Species), dplyr::vars(Sepal.Width,Sepal.Length), .side="rhs")
# .parse_unique(iris %>% dplyr::group_by(Species), dplyr::vars(Sepal.Width,Sepal.Length), .side="lhs")
.parse_unique = function(df, ..., .side = "rhs") {
  predictorVars = list()
  if (.is_character_list(...)) {
    if (.side != "rhs") predictorVars = df %>% dplyr::groups()
    if (.side != "lhs") predictorVars = c(predictorVars,.parse_character(df,...))
  } else if(.is_vars_interface(...)) {
    if (.side != "rhs") predictorVars = df %>% dplyr::groups()
    tmp = c(...) %>% sapply(rlang::as_label) %>% lapply(as.symbol)
    if (.side != "lhs") predictorVars = c(predictorVars,tmp)
  } else if(.is_formula_interface(...)) {
    predictorVars = .parse_formulae(df, ..., side = .side) %>%
      purrr::discard(~ is.null(.x) | length(.x) == 0) %>%
      unlist() %>% unique()
  } else {
    if (.side != "rhs") predictorVars = df %>% dplyr::groups()
    if (.side != "lhs") predictorVars = c(predictorVars,.parse_tidyselect(df, ...))
  }
  return(unique(predictorVars))
}

# a list of symbols
# .parse_character(iris, "Species", "Sepal.Width", "b")
# .parse_character(iris, c("Species", "Sepal.Width", "b"))
# .parse_character(iris, list("Species", "Sepal.Width", "b"))
.parse_character = function(df, ...) {
  wrong = setdiff(unlist(c(...)), colnames(df)) %>% unique()
  out = intersect(unlist(c(...)), colnames(df)) %>% unique() %>% sapply(as.symbol,USE.NAMES = FALSE)
  if (length(wrong>0)) warning("ignoring columns given that are not in dataframe: ",paste0(wrong,collapse=", "))
  return(out)
}

# where dots is either a function (in which case we only want rhs) or a tidyselect.
# .is_formula_interface(~ Species + Petal.Width + Missing)
# .is_formula_interface(~ Species + Petal.Width + Missing, a ~ b+c)
# .is_formula_interface(c(~ Species + Petal.Width + Missing, a ~ b+c))
# .is_formula_interface(list(~ Species + Petal.Width + Missing, a ~ b+c))
# .is_formula_interface(list(~ Species + Petal.Width + Missing, "not a formula"))
# .is_formula_interface(tidyselect::everything())
# .is_formula_interface()
.is_formula_interface = function(...) {
  out = tryCatch({
    tmp = suppressWarnings(sapply(c(...),rlang::is_bare_formula))
    return(all(tmp))
  }, error = function(e) {
    # could have been a tidyselect.
    FALSE
  })
  return(out)
}

# .is_vars_interface(list(~ Species + Petal.Width + Missing, "not a formula"))
# .is_vars_interface(dplyr::vars(Species, Petal.Width))
.is_vars_interface = function(...) {
  out = tryCatch({
    suppressWarnings(
      rlang::is_quosures(c(...)) ||
        all(sapply(c(...),rlang::is_symbol))
    )
  }, error = function(e) {
    # could have been a tidyselect.
    FALSE
  })
  return(out)
}

# check for a list of character column names from ... where the alternatives
# could include a tidyselect.
# .is_character_list("a","b","c")
# .is_character_list(c("a","b","c"))
# .is_character_list(list("a","b","c"))
# .is_character_list(a~b,"b","c") # no
# .is_character_list(tidyselect::everything())
# .is_character_list(colnames(iris))
.is_character_list = function(...) {
  out = tryCatch({
    tmp = suppressWarnings(sapply(c(...),is.character))
    return(all(tmp))
  }, error = function(e) {
    # could have been a tidyselect.
    FALSE
  })
  return(out)
}


# .pair_apply(diamonds, chi = ~ stats::chisq.test(.x,.y), .cols = tidyselect::where(is.factor)) %>% dplyr::mutate(pvalue = purrr::map_dbl(chi, ~.x$p.value))
# .pair_apply(diamonds, chi = chisq.test, .cols = tidyselect::where(is.factor)) %>% dplyr::mutate(pvalue = purrr::map_dbl(chi, ~.x$p.value))
# .pair_apply(diamonds, chi = ~ stats::chisq.test(.x,.y)$p.value, method = ~ "chisq", .cols = tidyselect::where(is.factor))
# .pair_apply(diamonds, error = ~ stop("error in cols"), .cols_x = tidyselect::where(is.factor),.cols_y = tidyselect::where(is.numeric))
# iris %>% dplyr::group_by(Species) %>% .pair_apply(cor = cor, method = ~ "chisq", .cols = tidyselect::where(is.numeric))
.pair_apply = function(df, ..., .cols = tidyselect::everything(), .cols_x = NULL, .cols_y = NULL, .diagonal=FALSE) {
  .cols = rlang::enexpr(.cols)
  .cols_x = rlang::enexpr(.cols_x)
  .cols_y = rlang::enexpr(.cols_y)
  if (dplyr::is.grouped_df(df)) return(df %>% dplyr::group_modify(function(d,g,...) .pair_apply(d, ...), ..., .cols=!!.cols, .cols_x=!!.cols_x, .cols_y=!!.cols_y))
  if (is.null(.cols_x)) .cols_x = .cols
  if (is.null(.cols_y)) .cols_y = .cols
  dfx = df %>% dplyr::select(!!.cols_x)
  dfy = df %>% dplyr::select(!!.cols_y)
  dots = rlang::list2(...)

  err2 = character()
  out2 = dplyr::bind_rows(
    lapply(colnames(dfx), function(xcol) {
      x = dplyr::pull(dfx,xcol)
      dplyr::bind_rows(
        lapply(colnames(dfy), function(ycol) {
          if (.diagonal || xcol != ycol) {
            y = dplyr::pull(dfy,ycol)
            out = tibble::tibble(var1 = xcol, var2 = ycol)
            for (name in names(dots)) {
              fn = purrr::as_mapper(dots[[name]])
              res = try(fn(x, y), silent = TRUE)
              if (inherits(res, "try-error")) {
                reason = attr(res,"condition")$message
                err2 <<- c(err2,reason)
                res = NA
              }
              if (is.atomic(res)) {
                out = out %>% dplyr::mutate(!!name := res)
              } else {
                out = out %>% dplyr::mutate(!!name := list(res))
              }
            }
            return(out)
          } else {
            return(NULL)
          }
        })
      )
    })
  )
  if (length(err2) > 0) warning(unique(err2))
  return(out2)
}
