# Fit robust poisson regression using sandwich estimators

This is roughly equivalent to a log_binomial model but converges well.
Its output can be interpreted at a RR. It is using a glm with a log link
function and a family of poisson with robust sandwich estimators. The
binary outcome is interpreted as a count where true is 1 and false is 0.

## Usage

``` r
robust_poisson(
  data,
  formula,
  positive = c("yes", "true", "present", "confirmed"),
  ...
)
```

## Arguments

- data:

  a data frame

- formula:

  a formula of the form `binary_outcome ~ obs1 + obs2 + ...`

- positive:

  test strings to interpret as true if outcome is not a factor or
  logical.

- ...:

  not used

## Value

a model object

## Details

heteroskedasticity-consistent (HC) standard errors using sandwich:
https://data.library.virginia.edu/understanding-robust-standard-errors/

## Examples

``` r
diamonds3 = ggplot2::diamonds %>% dplyr::mutate(
  is_coloured = color <= "F",
  cut = factor(cut,ordered=FALSE),
  price_cat = cut(price, 
      breaks = c(0,500,1000,2000,4000,Inf),
      labels = c("<500","500-999","1000-1999","2000-3999","4000+"),
      ordered_result = TRUE)
) %>% dplyr::select(-color)

model5 = diamonds3 %>% robust_poisson(is_coloured ~ cut + carat + clarity + price)
global_p_value(model5)
#> Coefficient covariances computed by function(m) sandwich::vcovHC(m, type = "HC1")
#> # A tibble: 4 × 3
#>   predictor global.p global.p.method            
#>   <chr>        <dbl> <chr>                      
#> 1 cut       1.56e-36 Wald Chi-squared test (III)
#> 2 carat     0        Wald Chi-squared test (III)
#> 3 clarity   0        Wald Chi-squared test (III)
#> 4 price     0        Wald Chi-squared test (III)
```
