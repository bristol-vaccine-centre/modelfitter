#' Format a ratio, truncating at a set level above and below 1 on log scale.
#'
#' @param x a vector of numbers
#' @param fmt.ratio a sprintf format string
#' @param max.ratio a max ratio after which to display as e.g. `>50` or `<0.02`
#' @param na.ratio a symbol in case the value is `NA`.
#'
#' @return a string of formatted ratios
#' @export
#'
#' @examples
#' format_ratio(2^(-6:6))
format_ratio = function(x, fmt.ratio="%1.2f", max.ratio=50, na.ratio="Unk") {
  return(dplyr::case_when(
    is.na(x) ~ na.ratio,
    x > max.ratio ~ sprintf(paste0(">",fmt.ratio), max.ratio),
    x < 1/max.ratio ~ sprintf(paste0("<",fmt.ratio), 1/max.ratio),
    TRUE ~ sprintf(fmt.ratio, x)
  ))
}

#' Format a confidence interval
#'
#' @param median the median value
#' @param lower the lower value
#' @param upper the upper quantile
#' @param is_reference_value is the value a reference value
#' @param fmt.ci the layout of the 3 elements as a `sprintf` using `%s` for each element
#' @param na.ci the value to show if the median ci is `NA`
#' @param fn a function to format each number
#' @inheritDotParams format_ratio
#'
#' @return a formatted CI string
#' @export
#'
#' @examples
#' format_ci(
#'   median = 2^(-5:5),
#'   lower = 2^(-5:5-1),
#'   upper = 2^(-5:5+1),
#'   fmt.ratio = "%1.3g"
#' )
#' 
format_ci = function(median, lower, upper, is_reference_value = FALSE, fmt.ci = "%s [%s \u2013 %s]", na.ci="\u2014", fn = format_ratio, ...) {
  if (!is.function(fn)) fn = rlang::as_function(fn)
  
  return(dplyr::case_when(
    is.na(median) & is_reference_value ~ "ref",
    is.na(median) ~ na.ci,
    TRUE ~ sprintf(fmt = fmt.ci, fn(median, ...), fn(lower, ...), fn(upper, ...))
  ))
}

#' Format a summary of multiple fits into a table.
#'
#' @param summfit A set of fitted, and summarised configured models
#' @inheritDotParams model_labels
#' @param statistic what model output is this table presenting? Is it an OR, a 
#'   RR or a HR? or something else? 
#' @param global.p present the global p value (anova III) rather than the line by line values.
#' @param inv_link the inverse of the link function employed in the models. This is 
#'   almost always the inverse `exp(...)` unless we are dealing with a linear model.
#' @param col_header a glue spec using columns from the summfit data table to label the model columns.
#'   `model_name` and `n_obs_summary` should be defined as a minimum. Other bits
#'   of metadata will be present if the table has been configured using 
#'   `configure_models(...)` including `model_type_name`, `data_subset_name`,
#'    `model_base_name`, `model_update_name`, `n_boots`.
#' @param row_design a glue spec for presenting the statistic. valid columns are
#'   `reference` - the referent status, `group.type`, `beta.lower`,
#'   `beta.median`, `beta.upper`, `value.lower`,
#'   `value.median`, `value.upper`, `p.value.mixture`, `global.p.mixture`,
#'   `global.p.method`. The helper functions `format_ratio(x,fmt.ratio = "%1.3g")` and `format_ci(med,low,hi,ref)`
#'   may be useful in this glue string
#' @param p_format a function (or lambda) converting a number into a p-value string
#' @param font_size (optional) the font size for the table in points
#' @param font (optional) the font family for the table (which will be matched to
#'   closest on your system)
#' @param footer_text any text that needs to be added at the end of the table,
#'   setting this to FALSE dsables the whole footer (as does
#'   `options("tableone.hide_footer"=TRUE)`).
#' @param summarise_fn in the event that we want to present multiple models in 
#'   the same column of a table it is possible that there are multiple entries
#'   for each variable. This function will combine them (at a text level) so they
#'   can be placed in a table. Examples could be `dplyr::first` or 
#'   `~ paste0(.x,collapse="\n")`
#'   
#' @return a huxtable tabular output of the model(s)
#' @export
#'
#' @examples
#' cfg  = configure_models(
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
#' exectn = cfg %>% execute_configuration(cache = TRUE)
#' summfit = exectn %>% summarise_fits()
#' hux = summfit %>% format_summary()
#' hux
format_summary = function(
    summfit, 
    ..., 
    statistic="OR", 
    global.p = getOption("modelfitter.global_p_values",TRUE), 
    inv_link = exp, 
    col_header = "{model_name} (N={sprintf('%d',max(n_obs_summary))})",
    row_design = "{format_ci(value.median,value.lower,value.upper,reference,fmt.ratio = '%1.2g')}",
    p_format = NULL,
    font_size = getOption("modelfitter.font_size",8),
    font = getOption("modelfitter.font","Arial"),
    footer_text = NULL,
    summarise_fn = NULL
) {
  
  # row design full config (not really needed I think):
  # list(
  #   "{statistic} [95% CI]" = "{sprintf('%1.2f [%1.2f \u2014 %1.2f]',value.median,value.lower,value.upper)}",
  #   "P value" = if (global.p) "{tableone::format_pvalue(global.p.mixture)}" else "{tableone::format_pvalue(p.value.mixture)}"
  # )
  
  .use_tableone_defaults()
  
  # summfit = summfit %>% dplyr::mutate(statistic = statistic)
  if (!is.null(p_format)) {
    p_format = rlang::as_function(p_format)
  } else {
    p_format = getOption("modelfitter.pvalue_formatter", scales::pvalue)
  }
  
  # TODO: need to get the statistic_name in here maybe based on "model_type_name"
  # will need a user input mapping from type to statistic name.
  summfit = summfit %>% dplyr::mutate(col_head = glue::glue(col_header))
  
  # Get the model labels from the fit
  if (attributes(summfit)$fits_retained) {
    grp_structure = summfit %>% 
      dplyr::ungroup() %>%
      tidyr::unnest(model_fit) %>% 
      dplyr::pull(fit) %>%
      model_labels(...)
    
  } else {
    grp_structure = summfit %>% 
      dplyr::ungroup() %>%
      dplyr::mutate(
        labels = purrr::imap(model_fit, ~ attr(.x,"labels") %>% dplyr::mutate(model.order = .y))
      ) %>% dplyr::reframe(
        .combine_label_df(labels)
      )
  }
  
  
  # get the link transform function
  inv_link = rlang::as_function(inv_link)
  
  # construct a formatted table for each model group
  table_data = summfit %>% dplyr::mutate(
    fmt = purrr::map2(coef_summary, global_p_summary, 
                      ~ grp_structure %>% 
                          # left join because we want to include missing levels where they are coefficients
                          dplyr::left_join(.x, by="term") %>% 
                          # inner join because we want to get rid of missing characterisitics
                          
                          dplyr::inner_join(.y, by="predictor"))
  ) %>% dplyr::select(
    -dplyr::any_of(c("coef_summary", "global_p_summary", "quality_summary", "model_fit"))
  )
  
  multiple = (summfit %>% dplyr::select(col_head) %>% 
                dplyr::distinct() %>% nrow()) > 1
  # browser()
  
  # extract the methods used to calcualte the global p values
  if (global.p) {
    method = lapply(table_data$fmt, dplyr::pull, global.p.method) %>% purrr::list_c() %>% unique()
    footer_text = c(
      sprintf("P values calculated using %s",method),
      footer_text
    )
  }
  
  # TODO: consider putting quality metrics into footer
  # This would involve getting the getting the quality_summary column from 
  # summfit if available and unnesting it. Then some glue spec and some 
  # filtering would be required to get to a list of formatted strings. It would
  # need to include model names and probably shoudl only be present if there are
  # multiple models.
  
  # This constructs the formatted table with coefficients
  table_data2 = table_data %>% 
    dplyr::mutate(
      fmt = purrr::map(fmt, ~ {
        tmp = .x %>% dplyr::mutate(
          value.lower = inv_link(beta.lower),
          value.median = inv_link(beta.median),
          value.upper = inv_link(beta.upper)
        ) %>% dplyr::mutate(
          # the row design specificiation 
          statistic = glue::glue(row_design)
        ) 
        if (global.p) {
          tmp = tmp %>% dplyr::rename(p_value = global.p.mixture)
        } else {
          tmp = tmp %>% dplyr::rename(p_value = p.value.mixture)
        }
        tmp %>% 
          dplyr::mutate(p_value = p_format(p_value)) %>%
          dplyr::select(characteristic, subgroup, statistic, p_value)
      })
    ) %>%
    dplyr::group_by(col_head) %>%
    dplyr::summarise(
      fmt = list(dplyr::bind_rows(fmt))
    )
  
  # browser()
  
  
  # check for completeness in combinations of col_head, characterisitic, and subgroup
  
  # TODO: this is an incomplete solution. Better woudl be to order it based on the
  # combination character and subgroup.
  character_order = unique(unlist(purrr::map(table_data$fmt, ~.x %>% dplyr::pull(characteristic))))
  subgroup_order = unique(unlist(purrr::map(table_data$fmt, ~.x %>% dplyr::pull(subgroup))))
  
  tmpy = table_data2 %>% tidyr::unnest(fmt) %>%
    tidyr::complete(col_head,tidyr::nesting(characteristic, subgroup), fill = list(statistic = "\u2014", p_value = "\u2014")) %>%
    dplyr::mutate(
      characteristic = factor(characteristic, levels = character_order),
      subgroup = factor(subgroup, levels = subgroup_order)
    )
  
  # check for uniqueness in  combinations of col_head, characterisitic, and subgroup
  unique = all((tmpy %>% dplyr::group_by(col_head,characteristic, subgroup) %>% 
                  dplyr::count() %>% dplyr::pull(n)) == 1)
  
  if (!unique) {
    message("There are more than one coefficients for some of the variables.")
    if (is.null(summarise_fn)) stop(
      "If this was intentional you must provide a `summarise_fn` parameter. eg. `dplyr::first`
otherwise it may be your column naming does not uniquely identify the model in which 
case updating `col_header` to include `{model_base_name}`,`{model_update_name}` and 
`{model_type_name}` may help."
    )
    
    summarise_fn = rlang::as_function(summarise_fn)
    message("Summarising the result using the `summarise_fn` parameter")
    tmpy = tmpy %>% dplyr::group_by(col_head,characteristic, subgroup) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), summarise_fn))
  }
  
  table_data2 = tmpy %>% tidyr::nest(fmt = c(-col_head))
  
  #TODO: this assumes that only one kind fo statistic can be 
  # presented in a table. The example shows where this breaks.
  # In reality we need to be able to map form col_head to the 
  # statistic for that model.
  statistic_name = sprintf("%s [95%% CI]",statistic)
  chr_name = getOption("modelfitter.characteristic_column_name","Characteristic")
  subgrp_name = getOption("modelfitter.subgroup_column_name","Subgroup")
  pvalue_name = getOption("modelfitter.pvalue_column_namel","P value")
  
  cgv = dplyr::vars(col_head)
  # cgv = if (multiple) dplyr::vars(col_head) else dplyr::vars()
  rgv = if (global.p) sapply(c(chr_name, pvalue_name, subgrp_name),as.symbol) 
  else sapply(c(chr_name, subgrp_name),as.symbol)
  dt_col = sapply(c(cgv,rgv),rlang::as_label)
  
  out = table_data2 %>% 
    dplyr::select(c(col_head,fmt)) %>%
    dplyr::mutate(hux = purrr::map2(col_head, fmt, function(.x,.y) {
      
      # browser()
      
      .y = .y %>% dplyr::select(
        !!chr_name := characteristic,
        !!subgrp_name := subgroup,
        !!statistic_name := statistic,
        !!pvalue_name := p_value
      )
      
      # if (multiple) 
      .y = .y %>% dplyr::mutate(
        col_head = .x
      )
      
      # create the table for each col head
      
      tmp = .y %>% dplyr::ungroup() %>% .hux_tidy(
        rowGroupVars = rgv,
        colGroupVars = cgv,
        displayRedundantColumnNames = TRUE,
        defaultFontSize= font_size, 
        defaultFont = font
      )
      
      # move the p-value column
      if (global.p) tmp = huxtable::add_columns(tmp[-2],tmp[2])
      tmp
      
    }))
  
  init = out$hux[[1]]
  if (multiple) {
    hux2 = lapply(out$hux[-1], `[`, 3:4)
    init = Reduce(huxtable::add_columns, hux2, init)
  }
  
  init = init %>% huxtable::set_colspan(row=1,col = seq(1,ncol(init)-2,2)+2, 2)
  
  return(init %>% .hux_add_footer(footer_text))
  
}


# when formatting a model copy any default valued across from tableone
.use_tableone_defaults = function() {
  
  for (option in names(options())) {
    if (stringr::str_starts(option,"tableone.")) {
      newopt = stringr::str_replace(option, "tableone.", "modelfitter.")
      if (is.null(getOption(newopt))) {
        o = list()
        o[[newopt]] = getOption(option)
        options(o)
      }
    }
  }
  
}



#' Generate a table for a single model output
#'
#' The `modelfitter` configuration, execution, summary, format pipeline is 
#' for the complex use cases with multiple models. This is the simple version 
#' when we have a single fitted model and we want to print it in a table. This 
#' is more or less what `gtsummary` does.
#'
#' @param model_name a title for the model.
#' @param fit the model fit.
#' @param statistic the type of stastic this model outputs (e.g. OR, HR, RR).
#' This mostly defines the label in the table.
#' @inheritDotParams format_summary
#'
#' @return a huxtable formatted model summary table
#' @export
#'
#' @examples
#' coxfit = cox_model(survival::flchain, survival::Surv(futime, death) ~ sex * age)
#' format_model("FLChain", coxfit, "HR")
format_model = function(model_name, fit, statistic, ...) {
  
  ml = model_labels(fit, ...)
  model_fit = tibble::tibble(
    n_obs = stats::nobs(fit),
    coef = list(suppressWarnings(broom::tidy(fit))),
    global_p = list(global_p_value(fit)),
    performance = list(suppressMessages(performance::model_performance(fit, verbose=FALSE))),
    fit = list(fit)
  )
  
  tmp = tibble::tibble(
    model_name = model_name,
    model_fit = list(structure(model_fit, "labels" = ml))
  )
  
  tmp2 = structure(tmp, 
                   fits_retained = TRUE,
                   performance = TRUE
  )
  
  tmp2 %>%
    summarise_fits() %>%
    format_summary(statistic = statistic, ...)
}
