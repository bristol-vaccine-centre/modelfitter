# Fit standard poisson regression with ordered contrasts

This is roughly equivalent to a log_binomial model but converges well.
Its output can be interpreted at a RR. It is using a stats::glm with a
log link function and a family of quasipoisson. The binary outcome is
interpreted as a count where true is 1 and false is 0. Ordered variables
are represented as contrasts between adjacent levels

## Usage

``` r
quasi_poisson_ordered(
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

model5 = diamonds3 %>% quasi_poisson_ordered(is_coloured ~ cut + carat + clarity + price)
summary(model5)
#> 
#> Call:
#> stats::glm(formula = formula, family = stats::quasipoisson(link = log), 
#>     data = data, contrasts = .ordered_contrasts(data, formula))
#> 
#> Coefficients:
#>                    Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)       5.815e-01  3.063e-02  18.983  < 2e-16 ***
#> cutGood          -8.685e-02  3.036e-02  -2.861  0.00423 ** 
#> cutVery Good     -1.346e-01  2.856e-02  -4.713 2.45e-06 ***
#> cutPremium       -2.238e-01  2.845e-02  -7.867 3.68e-15 ***
#> cutIdeal         -2.167e-01  2.810e-02  -7.710 1.28e-14 ***
#> carat            -2.855e+00  3.179e-02 -89.807  < 2e-16 ***
#> claritySI2-I1    -2.728e-01  4.333e-02  -6.296 3.08e-10 ***
#> claritySI1-SI2   -3.393e-01  1.371e-02 -24.739  < 2e-16 ***
#> clarityVS2-SI1   -1.802e-01  1.252e-02 -14.394  < 2e-16 ***
#> clarityVS1-VS2   -3.100e-01  1.496e-02 -20.723  < 2e-16 ***
#> clarityVVS2-VS1  -1.843e-02  1.860e-02  -0.991  0.32174    
#> clarityVVS1-VVS2 -1.659e-01  2.222e-02  -7.469 8.19e-14 ***
#> clarityIF-VVS1   -3.946e-01  3.326e-02 -11.864  < 2e-16 ***
#> price             2.505e-04  3.273e-06  76.538  < 2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for quasipoisson family taken to be 0.4898267)
#> 
#>     Null deviance: 37886  on 53939  degrees of freedom
#> Residual deviance: 33075  on 53926  degrees of freedom
#> AIC: NA
#> 
#> Number of Fisher Scoring iterations: 5
#> 
```
