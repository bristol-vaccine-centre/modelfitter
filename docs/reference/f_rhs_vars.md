# Get all used variables on LHS of a formula

Get all used variables on LHS of a formula

## Usage

``` r
f_rhs_vars(formula)
```

## Arguments

- formula:

  a formula or list of formulae

## Value

the variables on the LHS of the formulae as a character vector

## Examples

``` r
f_rhs_vars(survival::Surv(time,event) ~ x + z +y)
#> [1] "x" "z" "y"
f_rhs_vars(survival::Surv(time,event) ~ pspline(x) + z + y)
#> [1] "x" "z" "y"
```
