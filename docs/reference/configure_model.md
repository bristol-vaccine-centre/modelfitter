# Configure a single model fit using the `modelfitter` framework

Configure a single model fit using the `modelfitter` framework

## Usage

``` r
configure_model(model_name, model_formula, dataset, model_function)
```

## Arguments

- model_name:

  a label for the model

- model_formula:

  the model formula you want to fit

- dataset:

  the dataset

- model_function:

  the model function to fit, e.g. logistic_regression

## Value

a configuration dataframe and data provider to execute the fit with

## Examples

``` r
# Simple example
tmp = configure_model(
  "Iris model", 
  I(Species == "versicolor") ~ ., 
  iris, 
  modelfitter::logistic_regression
)
tmp2 = tmp %>% execute_configuration()
summfit = tmp2 %>% summarise_fits()
```
