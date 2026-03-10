# Fit standard log binomial regression using `stats::glm`

The log binomial model using standard glm is less that satisfactory and
hard to make converge. The algorithm needs starting values to have any
hope and these do make a difference to the outcome. This needs more
investigation before being used.

## Usage

``` r
log_binomial(
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

model5 = diamonds3 %>% log_binomial(is_coloured ~ cut + carat + clarity * price)
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated due to divergence
#> Warning: step size truncated: out of bounds
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: algorithm stopped at boundary value
global_p_value(model5)
#>       predictor      global.p          global.p.method
#> 1           cut  4.502530e-09 Joint tests (Wald - III)
#> 2         carat  0.000000e+00 Joint tests (Wald - III)
#> 3       clarity 7.553908e-188 Joint tests (Wald - III)
#> 5         price 2.365751e-295 Joint tests (Wald - III)
#> 4 clarity:price  2.758438e-25 Joint tests (Wald - III)
```
