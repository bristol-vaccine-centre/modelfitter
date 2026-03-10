# Generate a table for a single model output

The `modelfitter` configuration, execution, summary, format pipeline is
for the complex use cases with multiple models. This is the simple
version when we have a single fitted model and we want to print it in a
table. This is more or less what `gtsummary` does.

## Usage

``` r
format_model(model_name, fit, statistic, ...)
```

## Arguments

- model_name:

  a title for the model.

- fit:

  the model fit.

- statistic:

  the type of stastic this model outputs (e.g. OR, HR, RR). This mostly
  defines the label in the table.

- ...:

  Named arguments passed on to [`format_summary`](format_summary.md)

  `summfit`

  :   A set of fitted, and summarised configured models

  `global.p`

  :   present the global p value (anova III) rather than the line by
      line values.

  `inv_link`

  :   the inverse of the link function employed in the models. This is
      almost always the inverse `exp(...)` unless we are dealing with a
      linear model.

  `col_header`

  :   a glue spec using columns from the summfit data table to label the
      model columns. `model_name` and `n_obs_summary` should be defined
      as a minimum. Other bits of metadata will be present if the table
      has been configured using `configure_models(...)` including
      `model_type_name`, `data_subset_name`, `model_base_name`,
      `model_update_name`, `n_boots`.

  `row_design`

  :   a glue spec for presenting the statistic. valid columns are
      `reference` - the referent status, `group.type`, `beta.lower`,
      `beta.median`, `beta.upper`, `value.lower`, `value.median`,
      `value.upper`, `p.value.mixture`, `global.p.mixture`,
      `global.p.method`. The helper functions
      `format_ratio(x,fmt.ratio = "%1.3g")` and
      `format_ci(med,low,hi,ref)` may be useful in this glue string

  `p_format`

  :   a function (or lambda) converting a number into a p-value string

  `font_size`

  :   (optional) the font size for the table in points

  `font`

  :   (optional) the font family for the table (which will be matched to
      closest on your system)

  `footer_text`

  :   any text that needs to be added at the end of the table, setting
      this to FALSE dsables the whole footer (as does
      `options("tableone.hide_footer"=TRUE)`).

  `summarise_fn`

  :   in the event that we want to present multiple models in the same
      column of a table it is possible that there are multiple entries
      for each variable. This function will combine them (at a text
      level) so they can be placed in a table. Examples could be
      [`dplyr::first`](https://dplyr.tidyverse.org/reference/nth.html)
      or `~ paste0(.x,collapse="\n")`

  `...`

  :   Named arguments passed on to [`model_labels`](model_labels.md)

      `model`

      :   a model or list of models.

      `label_fn`

      :   a function that allows a predictor label to be renamed. This
          should expect a vector and return a vector of the same length.
          Levels will be terms in the model function and may be column
          names, or combinations thereof

      `subgroup_label_fn`

      :   a function that allows a subgroup label to be renamed. This
          should expect a vector and return a vector of the same length.
          The input to this function will be either a factor level name
          or a combination of them or whatever else the model decides to
          name it's coefficients.

      `...`

      :   not used

## Value

a huxtable formatted model summary table

## Examples

``` r
coxfit = cox_model(survival::flchain, survival::Surv(futime, death) ~ sex * age)
format_model("FLChain", coxfit, "HR")
#> Warning: Quosure lists can't be concatenated with objects other than quosures as of
#> rlang 0.3.0. Please call `as.list()` on the quosure list first.
#> This warning is displayed once every 8 hours.
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `hux = purrr::map2(...)`.
#> Caused by warning:
#> ! The requested font(s): [Arial], are not present on the system. Alternatives will be used.
#> This warning is displayed once per session.
#>            ─────────────────────────────────────────────────────────
#>                                          FLChain (N=2169)           
#>              Characteristic   Subgroup   HR [95% CI]       P value  
#>            ─────────────────────────────────────────────────────────
#>              sex              F          ref               <0.001   
#>                               M          2.5 [1.3 – 4.7]            
#>            ─────────────────────────────────────────────────────────
#>              age                         1.1 [1.1 – 1.1]   <0.001   
#>            ─────────────────────────────────────────────────────────
#>              sex & age        F:age      —                 0.116    
#>                               M:age      0.99 [0.98 – 1]            
#>            ─────────────────────────────────────────────────────────
#>              P values calculated using Likelihood ratio             
#>              test (II)                                              
#> 
#> Column names: Characteristic, Subgroup, 1, P value
```
