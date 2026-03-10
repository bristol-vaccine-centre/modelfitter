# Predict the potential coefficient names for a model output

**\[deprecated\]**

## Usage

``` r
format_summary_rows(rawData, ..., label_fn = ~.x)
```

## Arguments

- rawData:

  a `modelfit` or a dataframe(s)

- ...:

  the columns that we are using as predictors, as a list of formulae
  (rhs), a tidyselect call, a dplyr::vars() specification or a list of
  characters

- label_fn:

  a function that converts column names into readable labels

## Value

a dataframe with the likely names of the coefficients in a model which
can be joined to a model coefficients to

## Examples

``` r
# Disabling this example as function deprecated
if (FALSE) {

boot = run_model(
  ggplot2::diamonds %>% dplyr::mutate(cut = factor(cut, ordered=FALSE)), 
  price ~ ., 
  stats::lm
)
tmp = format_summary_rows(boot)



# logistic regression (OR)
diamonds2 = ggplot2::diamonds %>% dplyr::mutate(
   is_coloured = color <= "F",
   dplyr::across(dplyr::where(is.ordered), ~ factor(.x,ordered=FALSE))
) %>% dplyr::select(-color)

format_summary_rows(diamonds2, is_coloured ~ cut + carat + clarity*price)
model = logistic_regression(diamonds2, is_coloured ~ cut + carat + clarity * price)
model = logistic_regression(diamonds2, is_coloured ~ cut + I(cut=="D") + clarity*price)

stats::coef(model)
}
```
