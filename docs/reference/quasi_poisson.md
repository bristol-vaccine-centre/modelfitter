# Fit standard poisson regression

This is roughly equivalent to a log_binomial model but converges well.
Its output can be interpreted at a RR. It is using a stats::glm with a
log link function and a family of quasipoisson. The binary outcome is
interpreted as a count where true is 1 and false is 0.

## Usage

``` r
quasi_poisson(
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

model5 = diamonds3 %>% quasi_poisson(is_coloured ~ cut + carat + clarity + price)
global_p_value(model5)
#> # A tibble: 4 × 3
#>   predictor global.p global.p.method           
#>   <chr>        <dbl> <chr>                     
#> 1 cut       1.28e-33 Likelihood ratio test (II)
#> 2 carat     0        Likelihood ratio test (II)
#> 3 clarity   0        Likelihood ratio test (II)
#> 4 price     0        Likelihood ratio test (II)
```
