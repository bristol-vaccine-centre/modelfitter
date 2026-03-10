# Fit standard cox model.

Fit standard cox model.

## Usage

``` r
cox_model(data, formula, ...)
```

## Arguments

- data:

  a data frame

- formula:

  a formula of the form
  `survival::Surv(time, event) ~ obs1 + obs2 + ...`

- ...:

  not used

## Value

a model object
