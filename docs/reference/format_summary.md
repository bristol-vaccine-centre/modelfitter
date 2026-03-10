# Format a summary of multiple fits into a table.

Format a summary of multiple fits into a table.

## Usage

``` r
format_summary(
  summfit,
  ...,
  statistic = "OR",
  global.p = getOption("modelfitter.global_p_values", TRUE),
  inv_link = exp,
  col_header = "{model_name} (N={sprintf('%d',max(n_obs_summary))})",
  row_design =
    "{format_ci(value.median,value.lower,value.upper,reference,fmt.ratio = '%1.2g')}",
  p_format = NULL,
  font_size = getOption("modelfitter.font_size", 8),
  font = getOption("modelfitter.font", "Arial"),
  footer_text = NULL,
  summarise_fn = NULL
)
```

## Arguments

- summfit:

  A set of fitted, and summarised configured models

- ...:

  Named arguments passed on to [`model_labels`](model_labels.md)

  `model`

  :   a model or list of models.

  `label_fn`

  :   a function that allows a predictor label to be renamed. This
      should expect a vector and return a vector of the same length.
      Levels will be terms in the model function and may be column
      names, or combinations thereof

  `subgroup_label_fn`

  :   a function that allows a subgroup label to be renamed. This should
      expect a vector and return a vector of the same length. The input
      to this function will be either a factor level name or a
      combination of them or whatever else the model decides to name
      it's coefficients.

  `...`

  :   not used

- statistic:

  what model output is this table presenting? Is it an OR, a RR or a HR?
  or something else?

- global.p:

  present the global p value (anova III) rather than the line by line
  values.

- inv_link:

  the inverse of the link function employed in the models. This is
  almost always the inverse `exp(...)` unless we are dealing with a
  linear model.

- col_header:

  a glue spec using columns from the summfit data table to label the
  model columns. `model_name` and `n_obs_summary` should be defined as a
  minimum. Other bits of metadata will be present if the table has been
  configured using `configure_models(...)` including `model_type_name`,
  `data_subset_name`, `model_base_name`, `model_update_name`, `n_boots`.

- row_design:

  a glue spec for presenting the statistic. valid columns are
  `reference` - the referent status, `group.type`, `beta.lower`,
  `beta.median`, `beta.upper`, `value.lower`, `value.median`,
  `value.upper`, `p.value.mixture`, `global.p.mixture`,
  `global.p.method`. The helper functions
  `format_ratio(x,fmt.ratio = "%1.3g")` and `format_ci(med,low,hi,ref)`
  may be useful in this glue string

- p_format:

  a function (or lambda) converting a number into a p-value string

- font_size:

  (optional) the font size for the table in points

- font:

  (optional) the font family for the table (which will be matched to
  closest on your system)

- footer_text:

  any text that needs to be added at the end of the table, setting this
  to FALSE dsables the whole footer (as does
  `options("tableone.hide_footer"=TRUE)`).

- summarise_fn:

  in the event that we want to present multiple models in the same
  column of a table it is possible that there are multiple entries for
  each variable. This function will combine them (at a text level) so
  they can be placed in a table. Examples could be
  [`dplyr::first`](https://dplyr.tidyverse.org/reference/nth.html) or
  `~ paste0(.x,collapse="\n")`

## Value

a huxtable tabular output of the model(s)

## Examples

``` r
cfg  = configure_models(
   formula_provider(
      "<F" = I(color < "F") ~ cut + carat + clarity + price,
      "<H" = I(color < "H") ~ cut + carat + clarity + price
   ),
   bootstrap_provider(ggplot2::diamonds, max_n = 10),
   model_function_provider(
     "Log reg" = modelfitter::logistic_regression,
     "Poisson" = modelfitter::quasi_poisson
   )
)

exectn = cfg %>% execute_configuration(cache = TRUE)
summfit = exectn %>% summarise_fits()
hux = summfit %>% format_summary()
hux
#>   ────────────────────────────────────────────────────────────────────────────
#>                           <F                   <F                   <H        
#>                           Log reg              Poisson              Log reg   
#>                           (N=53940)            (N=53940)            (N=53940  
#>                                                                     )         
#>     Characte   Subgroup   OR [95%    P value   OR [95%    P value   OR [95%   
#>     ristic                CI]                  CI]                  CI]       
#>   ────────────────────────────────────────────────────────────────────────────
#>     cut        Fair       ref        <0.001    ref        <0.001    ref       
#>                Good       0.98                 0.98                 0.66      
#>                           [0.81 –              [0.87 –              [0.54 –   
#>                           1.2]                 1.1]                 0.83]     
#>                Very       0.93                 0.96                 0.55      
#>                Good       [0.79 –              [0.86 –              [0.43 –   
#>                           1.1]                 1.1]                 0.66]     
#>                Premium    0.75                 0.84                 0.52      
#>                           [0.64 –              [0.75 –              [0.43 –   
#>                           0.9]                 0.94]                0.65]     
#>                Ideal      0.79                 0.87                 0.46      
#>                           [0.67 –              [0.78 –              [0.38 –   
#>                           0.92]                0.96]                0.56]     
#>   ────────────────────────────────────────────────────────────────────────────
#>     carat                 <0.02      <0.001    0.03       <0.001    <0.02     
#>                           [<0.02 –             [0.027 –             [<0.02 –  
#>                           <0.02]               0.033]               <0.02]    
#>   ────────────────────────────────────────────────────────────────────────────
#>     clarity    I1         ref        <0.001    ref        <0.001    ref       
#>                SI2        0.79                 0.91                 0.17      
#>                           [0.57 –              [0.75 –              [0.11 –   
#>                           1]                   1.1]                 0.26]     
#>                SI1        0.4                  0.63                 0.049     
#>                           [0.29 –              [0.51 –              [0.032 –  
#>                           0.53]                0.75]                0.077]    
#>                VS2        0.25                 0.48                 0.032     
#>                           [0.18 –              [0.4 –               [0.02 –   
#>                           0.33]                0.58]                0.051]    
#>                VS1        0.13                 0.32                 <0.02     
#>                           [0.093 –             [0.26 –              [<0.02 –  
#>                           0.17]                0.38]                0.033]    
#>                VVS2       0.12                 0.31                 <0.02     
#>                           [0.086 –             [0.25 –              [<0.02 –  
#>                           0.16]                0.37]                0.033]    
#>                VVS1       0.077                0.23                 <0.02     
#>                           [0.057 –             [0.19 –              [<0.02 –  
#>                           0.1]                 0.28]                <0.02]    
#>                IF         0.026                0.097                <0.02     
#>                           [<0.02 –             [0.077 –             [<0.02 –  
#>                           0.037]               0.12]                <0.02]    
#>   ────────────────────────────────────────────────────────────────────────────
#>     price                 1 [1 –     <0.001    1 [1 –     <0.001    1 [1 –    
#>                           1]                   1]                   1]        
#>   ────────────────────────────────────────────────────────────────────────────
#>     P values calculated using Likelihood ratio test (II)                      
#> 
#> Column names: Characteristic, Subgroup, 1, P value, 1.1, P value.1, 1.2, P
#> value.2, 1.3, P value.3
#> 
#> 7/10 columns shown.
```
