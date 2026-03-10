# Provide access to missing data imputations

Provide access to missing data imputations

## Usage

``` r
imputation_provider(data, max_n, formulae = ~., ...)
```

## Arguments

- data:

  the data frame with missing rows

- max_n:

  the maximum number of different imputations to

- formulae:

  a lit of formulae with all the columns used (defaults to everything)

- ...:

  cache control

## Value

a function that returns a dataset for inputs between `1:max_n`

## Examples

``` r
ip = imputation_provider(mice::nhanes2, 10, list(~ hyp + bmi, ~ age + chl))
ip(1) %>% head(10)
#>      age  bmi hyp chl
#> 1  20-39 33.2  no 229
#> 2  40-59 22.7  no 187
#> 3  20-39 30.1  no 187
#> 4  60-99 22.7 yes 186
#> 5  20-39 20.4  no 113
#> 6  60-99 20.4 yes 184
#> 7  20-39 22.5  no 118
#> 8  20-39 30.1  no 187
#> 9  40-59 22.0  no 238
#> 10 40-59 27.4 yes 238
```
