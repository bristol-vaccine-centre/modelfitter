# Get all used variables on LHS of a formula

Get all used variables on LHS of a formula

## Usage

``` r
f_lhs_vars(formula)
```

## Arguments

- formula:

  a formula or list of formulae

## Value

the variables on the RHS of the formulae as a character vector

## Examples

``` r
f_lhs_vars(survival::Surv(time,event) ~ x + z + y)
#> [1] "time"  "event"
```
