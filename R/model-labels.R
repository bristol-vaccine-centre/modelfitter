## Model output formatter ----

#' Creates a table or plot row template for a set of models
#' 
#' This produces a dataframe which can be used to arrange the coefficients
#' and p-values from one statistical model, or set of models. The 
#' rows are a superset of all the coefficients of the models and is designed
#' to be used to left_join the outputs of `broom::tidy` (by `term`) or 
#' `global_p_value` (by `predictor`) to construct a tabular output.
#' 
#' It is expect that use cases such as multiple univariate models + a few 
#' fully adjusted models are passed to this function and the result is 
#' to be displayed in a single plot or figure.
#'
#' @param model a model or list of models.
#' @param label_fn a function that allows a predictor label to be renamed. This 
#'   should expect a vector and return a vector of the same length. Levels will be
#'   terms in the model function and may be column names, or combinations thereof
#' @param subgroup_label_fn a function that allows a subgroup label to be renamed. This 
#'   should expect a vector and return a vector of the same length. The
#'   input to this function will be either a factor level name or a combination
#'   of them or whatever else the model decides to name it's coefficients.
#' @param ... not used
#'
#' @return a data frame with `model.order`, `group.order`, `subgroup.order`,
#'   `characteristic`, `subgroup`, dplaye columns and  `predictor` and `term` 
#'   key columns to link to the output of `broom::tidy` or `global_p_value` 
#'   (i.e. Anova II/III outputs)
#' @export
#'
#' @examples
#' diamonds3 = ggplot2::diamonds %>% dplyr::mutate(
#'   is_coloured = color <= "F",
#'   cut = factor(cut,ordered=FALSE),
#'   price_cat = tableone::cut_integer(price, c(500,1000,2000,4000))
#' ) %>% dplyr::select(-color)
#' 
#' model = stats::glm(is_coloured ~ cut + carat + clarity * price, diamonds3, family="binomial")
#' model_labels(model, toupper, tolower)
#' 
#' 
#' model2 = logistic_regression(diamonds3, is_coloured ~ I(cut=="Good") + carat + clarity * price)
#' model_labels(model2, toupper, tolower)
#' 
#' model3 = stats::glm(is_coloured ~ carat + cut * clarity +  price, diamonds3, family="binomial")
#' model_labels(model3, toupper, tolower)
#' 
#' 
#' model4 = stats::glm(
#'   is_coloured ~ cut + carat + clarity + price, 
#'   diamonds3, 
#'   family="binomial", 
#'   contrasts=list(clarity=MASS::contr.sdif))
#'   
#' coef(model4)
#' model_labels(model4, toupper, tolower)
#' 
#' # tmp = .ordered_contrasts(diamonds3, )
#' # model5 = stats::glm(
#' #   is_coloured ~ cut + carat + clarity * price, 
#' #   diamonds3, 
#' #   family="binomial", contrasts=tmp)
#' #   
#' # model_labels(model5, toupper, tolower)
#' 
#' model6 = stats::glm(
#'   is_coloured ~ cut + carat + clarity + price_cat, 
#'   diamonds3, 
#'   family="binomial", 
#'   contrasts=list(clarity=MASS::contr.sdif, price_cat=MASS::contr.sdif)
#' )
#' 
#' model_labels(model6, toupper, tolower)
#' 
#' model7 = stats::glm(
#'   is_coloured ~ cut + carat + clarity + splines::ns(price,df=2), 
#'   diamonds3, 
#'   family="binomial", 
#'   contrasts=list(clarity=MASS::contr.sdif))
#'   
#' model_labels(model7, toupper)
#' 
model_labels = function(model, label_fn, subgroup_label_fn, ...) {
  
  if (rlang::is_missing(label_fn)) {
    label_fn = getOption("modelfitter.labeller", ~ .x)
  }
  if (rlang::is_missing(subgroup_label_fn)) {
    # c("18-24-25-30","18-24") %>% stringr::str_replace("^([^-]+-[^-]+)-([^-]+-[^-]+)$","\\1 vs \\2")
    subgroup_label_fn = getOption("modelfitter.level_labeller", ~ stringr::str_replace(.x, "^([^-]+-[^-]+)-([^-]+-[^-]+)$","\\1 vs. \\2"))
  }
  
  if (inherits(model,"list")) {
    tmp = purrr::imap(model, ~ model_labels(.x, label_fn, subgroup_label_fn) %>% dplyr::mutate(model.order = .y))
    return(.combine_label_df(tmp))
  }
  
  .use_tableone_defaults()
  
  # Some models provide the labels to the model coefficients
  tmp = attributes(stats::terms(model))
  
  # defaults to option output or no-op
  
  col_labeller = rlang::as_function(label_fn)
  
  
  level_labeller = rlang::as_function(subgroup_label_fn)
  
  # These are the suffixes applied to model terms if they are ordered
  # the interpretation of these is non trivial and presented as they are
  # a better result for ordered is to supply the contrasts and compare sequential
  # items
  .ord_suff = c(".L",".Q",".C", paste0("^",4:100))
  names(.ord_suff) = c("Linear","Quadratic","Cubic", paste0(scales::ordinal(4:100), " power"))
  
  mframe = stats::model.frame(model)
  # these are the predictors in the model
  mgroups = colnames(mframe)
  # these are the coefficients which have both predictor name and level value
  # as a concatenated string.
  mterms = names(stats::coef(model))
  termregex = paste0(.escape(mgroups),collapse="|")
  
  # Search for anything in the coefficients of the model that matches the names 
  # of the data columns in the model dataframe. These are named after any
  # model transformation has occurred due to the model formula and are
  # named after the mode terms. This regex is looking for XXXXyyyy where XXXX
  # is one of the column labels from the dataframe and yyyy is an optional 
  # factor label. It also looks at terms such as AAAAbbbb:XXXXyyyy for interaction
  # terms
  termlabels = mterms %>% stringr::str_match_all(sprintf("(^|:)(%s)", termregex)) %>% 
    purrr::map( ~ paste0(.x[,3],collapse = ":")) %>% unique() %>% 
    purrr::discard(~ .x == "") %>% unlist()
  
  grouplabels = mterms %>% stringr::str_match_all(sprintf("(^|:)(%s)", termregex)) %>% 
    purrr::map( ~ .x[,3]) %>% unlist() %>% unique() %>% 
    purrr::discard(~ .x == "") %>% unlist()
  
  tmp2 = tibble::tibble(
    predictors = if (is.null(tmp$term.labels)) termlabels else tmp$term.labels,
    keys = lapply(predictors, function(x) names(which(tmp$factors[,x]==1))),
  ) %>% dplyr::mutate(group.order = dplyr::row_number())
  
  mgroups = mgroups[mgroups %in% grouplabels]
  
  # Extract the levels for each predictor from model data frame
  lvls = lapply(mgroups, function(n) {
    # n = "clarity"
    x = mframe[[n]]
    type = class(x)[[1]]
    
    # Here we are extracting the levels of the individual predictors (i.e. each
    # of mgroups) as specified in the model coeficients.
    nlevels = mterms %>% stringr::str_extract(sprintf("(^|:)%s(.*?)(:(%s).*)?$",.escape(n), termregex),2) %>% 
      stats::na.omit() %>% unique()
    
    # The bare term exists in the coeffients and no level is matched
    if (length(nlevels) == 1 && nlevels == "") {
      # this controls how numeric levels are displayed
      tmp = tibble::tibble(
        group.term = n,
        group.type = type,
        level.term = "",
        subgroup.label = "",
        subgroup.order = 1,
        reference = FALSE
      )
    } else if (identical(nlevels, "TRUE")) {
      # This controls how binary levels are displayed
      # In this setup only the positive appears in the table without label
      # and there is no reference category
      tmp = tibble::tibble(
        group.term = n,
        group.type = type,
        level.term = "TRUE",
        subgroup.label = "",
        subgroup.order = 1,
        reference = FALSE
      )
      
      # This is the alternative for if we want to show FALSE as a reference level
      # tmp = tibble::tibble(
      #   group.term = x,
      #   group.type = type,
      #   level.term = c("FALSE","TRUE"),
      #   subgroup.label = c("no","yes"),
      #   subgroup.order = 1:2,
      #   reference = c(TRUE,FALSE)
      # )
      
    } else if (all(nlevels %in% levels(x))) {
      # this controls how unordered factors are displayed
      # This inserts extra possibilities to 
      tmp = tibble::tibble(
        group.term = n,
        group.type = type,
        level.term = levels(x),
        subgroup.label = level_labeller(levels(x)),
        subgroup.order = 1:length(levels(x)),
        reference = !(levels(x) %in% nlevels)
      )
      
    } else if (all(nlevels %in% .ord_suff)) {
      # this controls how ordered factors are displayed
      # if they are in a standard regression model,
      # The coefficients will be differently named if for example the model
      # contains contrasts.
      tmplv = .ord_suff[.ord_suff %in% nlevels]
      tmp = tibble::tibble(
        group.term = n,
        group.type = type,
        level.term = tmplv,
        subgroup.label = level_labeller(names(tmplv)),
        subgroup.order = 1:length(tmplv),
        reference = FALSE
      )
    } else {
      # The fallback. Present the levels as found in the model coefficients
      tmp = tibble::tibble(
        group.term = n,
        group.type = type,
        level.term = nlevels,
        subgroup.label = level_labeller(nlevels),
        subgroup.order = 1:length(nlevels),
        reference = FALSE
      )
    }
    
    tmp = tmp %>% dplyr::mutate(
      subgroup.term = paste0(group.term,level.term),
      group.label = unlist(col_labeller(group.term))
    )
    
    return(tmp)
    
  })
  names(lvls) = mgroups
  # browser()
  components = dplyr::bind_rows(lvls)
  
  # This generates the possible set of model coefficients
  # including combinations created by interaction terms as
  # a long dataframe. 
  # TODO: this does not work if there are only interaction terms.
  tmp3 = tmp2 %>% dplyr::mutate(
    lvls = purrr::map(keys, ~ purrr::map(.x, ~ lvls[[.x]]$subgroup.term)),
    combs = purrr::map(lvls, ~ expand.grid(.x) %>% 
                 dplyr::mutate(row = dplyr::row_number()) %>% 
                 tidyr::pivot_longer(-row, names_to = "order", values_to = "subgroup.term"))
  ) %>% dplyr::select(predictors, combs, group.order) %>% tidyr::unnest(combs)
  
  # Summarises long data frame into a square one combining the interaction
  # terms. 
  tmp3 %>%
    dplyr::left_join(components, by="subgroup.term") %>% 
    dplyr::group_by(predictors, row) %>%
    dplyr::summarise(
      
      group.order = min(group.order),
      model.order = 1,
      subgroup.order = sum(subgroup.order*10^(1-dplyr::row_number())),
      
      # This defines how interaction terms are labelled in the model
      characteristic = paste0(group.label, collapse = " & "),
      
      # This defines how interaction terms are labelled in the model
      # for continuous:factor interactions there is no valid label for
      # the continuous variable so the predictor label is used instead. 
      # Interpreting these terms in a model is very hard anyway.
      subgroup = paste0(ifelse(dplyr::n() > 1 & subgroup.label=="",level_labeller(group.label),subgroup.label), collapse = ":"),
      
      predictor = paste0(group.term, collapse = ":"),
      term = paste0(subgroup.term, collapse = ":"),
      # In interaction term the reference level is difficult to define
      # there will be the reference group only if both are unordered factors.
      reference = all(reference),
      group.type = paste0(unique(group.type), collapse = ":"),
      .groups = "drop"
    ) %>%
    dplyr::select(-predictors,-row) %>%
    dplyr::arrange(model.order, group.order, subgroup.order)
}

# from rex:::escape.character
# escape a regex
.escape = function (x) {
  chars <- c("*", ".", "?", "^", "+", "$", "|", "(", ")", "[",
             "]", "{", "}", "\\")
  .sanitize(x, chars)
}

.sanitize = function (x, chars) {
  gsub(paste0("([\\", paste0(collapse = "\\", chars), "])"),
       "\\\\\\1", x, perl = TRUE)
}

.combine_label_df = function(dfs) {
  tmp = dplyr::bind_rows(dfs)
  return(
    tmp %>% 
      dplyr::arrange(
        # The logic here is that when we combine multiple models we want the 
        # model with the most number of predictors to define the ordering.
        # therefore we want to keep the term with the largest group.order.
        # Some models may drop subgroup levels. If we present these with a full set
        # the set with the most number of levels defines the order.
        # if all else is equal we choose the first models definition to prevent 
        # duplicates. This means additional terms in only one model will float 
        # to the end of the table. This last behaviour is potentially able to be 
        # exposed to the user during presentation.
        dplyr::desc(group.order),
        dplyr::desc(subgroup.order),
        model.order
      ) %>%
      dplyr::group_by(characteristic, subgroup, term) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(
        group.order,
        model.order,
        subgroup.order
      )
  )
}

