# Fit exact logistic regression

Fit exact logistic regression

## Usage

``` r
exact_logistic_regression(
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
