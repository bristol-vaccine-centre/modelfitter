# Spline term marginal effects plot

Spline term marginal effects plot

## Usage

``` r
spline_term_plot(
  coxmodel,
  var_name,
  xlab = var_name,
  max_y = NULL,
  n_breaks = 7
)
```

## Arguments

- coxmodel:

  an output of a coxph model

- var_name:

  a variable that is involved in a spline term

- xlab:

  x axis label

- max_y:

  maximium hazard ratio to display on y axis. Inferred from the central
  estimates if missing, which will most likely cut off confidence
  intervals

- n_breaks:

  The number of divisions on the y axis

## Value

a ggplot
