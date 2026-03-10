# Combine bootstraps

**\[deprecated\]**

## Usage

``` r
combine_boots(boots, ..., inv_link = exp)
```

## Arguments

- boots:

  a bootstrapped model output

- ...:

  the columns that we are using as predictors, as a list of formulae
  (rhs), a tidyselect call, a dplyr::vars() specification or a list of
  characters

## Value

a collapsed summary of all bootstraps

## Examples

``` r
# boots = iris %>% run_model(Petal.Length ~ Species + Petal.Width, stats::lm)
# combine_boots(boots,  inv_link = ~ .x)
```
