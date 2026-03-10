# Convert multivariate formula to list of univariate formulae

Convert multivariate formula to list of univariate formulae

## Usage

``` r
univariate_from_multivariate(formula)
```

## Arguments

- formula:

  a formula of the type y ~ x1 + x2 + x3 + ....

## Value

a list of formulae of the type y ~ x1, y ~ x2, y ~ x3, ....

## Examples

``` r
univariate_from_multivariate(y ~ x1 + x2 + x3)
#> [[1]]
#> y ~ x1
#> <environment: 0x5a0680bb8ef0>
#> 
#> [[2]]
#> y ~ x2
#> <environment: 0x5a0680bba540>
#> 
#> [[3]]
#> y ~ x3
#> <environment: 0x5a0680b9e8c0>
#> 
univariate_from_multivariate(~ x1 + x2 + x3)
#> [[1]]
#> ~x1 + x2
#> <environment: 0x5a0665a04558>
#> 
#> [[2]]
#> ~x1 + x3
#> <environment: 0x5a0665a05c88>
#> 
```
