# Calculate a global P-value for a model fit for each of the predictors.

In the summary output of a model there is a p-value for each level of
the factors whereas usually we want to know what the overall
contribution of the individual predictor to the model is.

## Usage

``` r
global_p_value(x, ...)
```

## Arguments

- x:

  a model

- ...:

  not used.

## Value

a dataframe of the predictors of the model (but not the levels) in one
column and a global p value in another. The global.p will be given as
zero or one despite this being not really possible. It si assumed that
it will be formatted to truncate very low or very high values.

## Details

This function usually calculates a Type 2 Anova for the predictors
within the models but sometimes falls back to different methods if the
model is not supported by the
[`car::Anova`](https://rdrr.io/pkg/car/man/Anova.html) or
[`stats::anova`](https://rdrr.io/r/stats/anova.html) packages. The
method used is reported in the output. This is designed as an automated
process and may not work in all situations, or produce appropriate
output for all model types. Interaction terms may be particularly
problematic.

## Examples

``` r
diamonds2 = ggplot2::diamonds %>% dplyr::mutate(
   is_coloured = color <= "F",
   dplyr::across(dplyr::where(is.ordered), ~ factor(.x,ordered=FALSE))
) %>% dplyr::select(-color)

lmodel = stats::lm(Petal.Width ~ ., iris)
glmodel = logistic_regression(diamonds2, is_coloured ~ cut + carat + clarity * price)
coxfit = cox_model(survival::flchain, survival::Surv(futime, death) ~ sex * age)

# Error conditions
try(anova(stats::lm( cut ~ carat + clarity * price, diamonds2)))
#> Warning: using type = "numeric" with a factor response will be ignored
#> Warning: ‘-’ not meaningful for factors
#> Warning: ‘^’ not meaningful for factors
#> Error in if (ssr < 1e-10 * mss) warning("ANOVA F-tests on an essentially perfect fit are unreliable") : 
#>   missing value where TRUE/FALSE needed
global_p_value(stats::lm( cut ~ carat + clarity * price, diamonds2))
#>       predictor global.p          global.p.method
#> 1         carat       NA Joint tests (Wald - III)
#> 2       clarity       NA Joint tests (Wald - III)
#> 4         price       NA Joint tests (Wald - III)
#> 3 clarity:price       NA Joint tests (Wald - III)


# cox model
global_p_value(coxfit)
#> # A tibble: 3 × 3
#>   predictor global.p global.p.method           
#>   <chr>        <dbl> <chr>                     
#> 1 sex       1.22e-19 Likelihood ratio test (II)
#> 2 age       0        Likelihood ratio test (II)
#> 3 sex:age   1.16e- 1 Likelihood ratio test (II)

# linear model
global_p_value(lmodel)
#> # A tibble: 4 × 3
#>   predictor    global.p global.p.method           
#>   <chr>           <dbl> <chr>                     
#> 1 Sepal.Length 3.89e- 2 Likelihood ratio test (II)
#> 2 Sepal.Width  1.20e- 6 Likelihood ratio test (II)
#> 3 Petal.Length 1.97e- 6 Likelihood ratio test (II)
#> 4 Species      5.14e-10 Likelihood ratio test (II)

# logistic regression (OR)
global_p_value(
  stats::glm(is_coloured ~ cut + carat + clarity + price, diamonds2,  family="binomial"))
#> # A tibble: 4 × 3
#>   predictor global.p global.p.method           
#>   <chr>        <dbl> <chr>                     
#> 1 cut       1.32e-41 Likelihood ratio test (II)
#> 2 carat     0        Likelihood ratio test (II)
#> 3 clarity   0        Likelihood ratio test (II)
#> 4 price     0        Likelihood ratio test (II)
global_p_value(
  logistic_regression(diamonds2, is_coloured ~ cut + carat + clarity * price))
#> # A tibble: 5 × 3
#>   predictor     global.p global.p.method           
#>   <chr>            <dbl> <chr>                     
#> 1 cut           6.14e-53 Likelihood ratio test (II)
#> 2 carat         0        Likelihood ratio test (II)
#> 3 clarity       0        Likelihood ratio test (II)
#> 4 price         0        Likelihood ratio test (II)
#> 5 clarity:price 5.77e-94 Likelihood ratio test (II)

# poisson regression (RR)
global_p_value(
  quasi_poisson(diamonds2, is_coloured ~ cut + carat + clarity * price))
#> # A tibble: 5 × 3
#>   predictor      global.p global.p.method           
#>   <chr>             <dbl> <chr>                     
#> 1 cut           4.11e- 46 Likelihood ratio test (II)
#> 2 carat         0         Likelihood ratio test (II)
#> 3 clarity       0         Likelihood ratio test (II)
#> 4 price         0         Likelihood ratio test (II)
#> 5 clarity:price 2.62e-129 Likelihood ratio test (II)
global_p_value(
  robust_poisson(diamonds2, is_coloured ~ cut + carat + clarity * price))
#> Coefficient covariances computed by function(m) sandwich::vcovHC(m, type = "HC1")
#> # A tibble: 5 × 3
#>   predictor      global.p global.p.method            
#>   <chr>             <dbl> <chr>                      
#> 1 cut           4.54e- 50 Wald Chi-squared test (III)
#> 2 carat         0         Wald Chi-squared test (III)
#> 3 clarity       0         Wald Chi-squared test (III)
#> 4 price         1.40e-213 Wald Chi-squared test (III)
#> 5 clarity:price 2.97e-159 Wald Chi-squared test (III)
global_p_value(
  robust_poisson_2(diamonds2, is_coloured ~ cut + carat + clarity * price))
#>       predictor      global.p          global.p.method
#> 1           cut  5.589847e-50 Joint tests (Wald - III)
#> 2         carat  0.000000e+00 Joint tests (Wald - III)
#> 3       clarity  0.000000e+00 Joint tests (Wald - III)
#> 5         price  0.000000e+00 Joint tests (Wald - III)
#> 4 clarity:price 3.452018e-158 Joint tests (Wald - III)

# log binomial regression (RR)
# TODO: this does not work at the moment as an example as the log_binomial starting value is
# a problem
global_p_value(
  log_binomial(diamonds2, is_coloured ~ cut + carat + clarity + price))
#>   predictor      global.p          global.p.method
#> 1       cut  3.094317e-04 Joint tests (Wald - III)
#> 2     carat  0.000000e+00 Joint tests (Wald - III)
#> 3   clarity 4.301118e-128 Joint tests (Wald - III)
#> 4     price  0.000000e+00 Joint tests (Wald - III)

# global_p_value(
#  log_binomial_2(diamonds2, is_coloured ~ cut + carat + clarity + price))

```
