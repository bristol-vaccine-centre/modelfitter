# Take a list of predictors and impute data for those predictors

**\[deprecated\]**

## Usage

``` r
impute_data(rawData, ..., m = getOption("tableone.imputations", 10))
```

## Arguments

- rawData:

  a raw data frame of observations

- ...:

  the columns that we are using as predictors, as a list of formulae
  (rhs), a tidyselect call, a dplyr::vars() specification or a list of
  characters

- m:

  the number of imputations (default 10)

## Value

a mice object containing imputed data
