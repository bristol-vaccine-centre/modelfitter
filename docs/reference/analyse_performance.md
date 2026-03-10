# Performance metrics of bootstrapped models

**\[deprecated\]**

## Usage

``` r
analyse_performance(
  boots,
  statistics = c("AIC", "BIC", "R2_Tjur", "R2_Nagelkerke", "RMSE", "Sigma")
)
```

## Arguments

- boots:

  bootstrapped model output

- statistics:

  model performance statistics

## Value

a printable summary

## Examples

``` r
# pooled = boots %>% summarise_boots()
#  pooled %>% ggrrr::hux_tidy(
#    rowGroupVars = dplyr::vars(statistic),
#    colGroupVars = dplyr::vars(modelName)
#  )
```
