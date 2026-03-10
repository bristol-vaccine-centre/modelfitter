# Fit robust poisson regression using `geepack::glm`

equivalent to a log_binomial model

## Usage

``` r
robust_poisson_2(
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

model5 = diamonds3 %>% robust_poisson_2(is_coloured ~ cut + carat + clarity + price)
global_p_value(model5)
#>   predictor     global.p          global.p.method
#> 1       cut 1.752424e-36 Joint tests (Wald - III)
#> 2     carat 0.000000e+00 Joint tests (Wald - III)
#> 3   clarity 0.000000e+00 Joint tests (Wald - III)
#> 4     price 0.000000e+00 Joint tests (Wald - III)
```
