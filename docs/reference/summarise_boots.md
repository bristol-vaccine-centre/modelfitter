# Printable output of model combined

**\[deprecated\]**

## Usage

``` r
summarise_boots(boots, ..., statistic = "OR", global.p = TRUE, inv_link = NULL)
```

## Arguments

- boots:

  a bootstrapped model output

- ...:

  the columns that we are using as predictors, as a list of formulae
  (rhs), a tidyselect call, a dplyr::vars() specification or a list of
  characters

- statistic:

  was this an OR or an RR (or a HR) just used for labelling

- global.p:

  present the global p value (anova) rather than the line by line value.

## Value

a printable summary

## Examples

``` r
# out = boots %>% summarise_boots(...predictors..., "OR" )
#  boot = iris %>% 
#   dplyr::mutate(is_versicolor = Species == "versicolor") %>% 
#   run_model(is_versicolor ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width)
#   
#  out = summarise_boots(boot)
#  out %>% ggrrr::hux_tidy(
#    rowGroupVars = dplyr::vars(Characteristic,Group),
#    colGroupVars = dplyr::vars(modelName)
#  )
```
