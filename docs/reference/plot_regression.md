# Forest plot from a set of bootstrapped models

**\[deprecated\]**

## Usage

``` r
plot_regression(
  boots,
  facet = NULL,
  stat_label = "Odds Ratio",
  report_fit = FALSE,
  limit = c(NA, NA),
  p.component = FALSE,
  label_fn = ~.x
)
```

## Arguments

- boots:

  a set of bootstrapped model fits as output by `run_models`

- facet:

  a faceting variable (usually \`modelName“)

- stat_label:

  what to call the x axis

- report_fit:

  which components of the model performance statistics do we want to
  report

- limit:

  x axis limits

- p.component:

  indicate which component parts of the fit are significant and which
  are not

- label_fn:

  a function (or lambda) that accepts an vector and returns a vector

## Value

a ggplot
