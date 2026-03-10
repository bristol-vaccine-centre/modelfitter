# Sprintf with a list input

A variant of sprintf that work well with inputs that are in the format
of a list. Good examples of which are the quantile functions

## Usage

``` r
sprintf_list(format, params, na.replace = "―")
```

## Arguments

- format:

  the format string

- params:

  the inputs as a list (rather than as a set of individual numbers)

- na.replace:

  a value to replace NA values with.

## Value

the formatted string

## Examples

``` r
# generate a mixture confidence interval from a set of distributions
sprintf_list("%1.2f [%1.2f\u2013%1.2f]",
 qmixnorm(p=c(0.5,0.025,0.975),
 means=c(10,13,14), sds=c(1,1,2)))
#> [1] "12.44 [8.54–16.88]"
```
