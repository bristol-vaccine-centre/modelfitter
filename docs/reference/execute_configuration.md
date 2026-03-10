# Fit statistical models defined in a `modelfitter` configuration

Fit statistical models defined in a `modelfitter` configuration

## Usage

``` r
execute_configuration(
  cfg,
  retain_fit = sum(cfg$n_boots) < 50,
  performance = TRUE,
  ...,
  cache = FALSE
)
```

## Arguments

- cfg:

  a 4 column dataframe with

  - model_name - minumum

  - model_fn

  - data_subset_fn

  - model_formula

- retain_fit:

  keep the model fit object? If the config results in less than 50
  models then the default of this is TRUE. Otherwise we summarise the
  models as we go saving memory.

- performance:

  calculate performance metrics for all the models. These will be
  summarised over bootstrap replicates. Setting this to false may be a
  good idea if you have lots of models.

- ...:

  Named arguments passed on to [`model_labels`](model_labels.md)

  `model`

  :   a model or list of models.

  `label_fn`

  :   a function that allows a predictor label to be renamed. This
      should expect a vector and return a vector of the same length.
      Levels will be terms in the model function and may be column
      names, or combinations thereof

  `subgroup_label_fn`

  :   a function that allows a subgroup label to be renamed. This should
      expect a vector and return a vector of the same length. The input
      to this function will be either a factor level name or a
      combination of them or whatever else the model decides to name
      it's coefficients.

  `...`

  :   not used

- cache:

  cache model output

## Value

a 5 column dataframe with additional nested fitted results in
`model_fit`. This nested dataframe will have the columns:

- `boot` - an id for the data bootstrap or imputation;

- `fit` (optional) - the model fit itself;

- `coef` the model coefficients;

- `global_p` the global p values for this model (see global_p_value());

- `performance` (optional) the performance metrics for this model;

## Examples

``` r
tmp = configure_model(
  "Iris", I(Species == "versicolor") ~ ., iris, logistic_regression)
tmp2 = tmp %>% execute_configuration()
 
tmp3  = configure_models(
   formula_provider(
      "<F" = I(color < "F") ~ cut + carat + clarity + price,
      "<H" = I(color < "H") ~ cut + carat + clarity + price
   ),
   bootstrap_provider(ggplot2::diamonds, max_n = 10),
   model_function_provider(
     "Log reg" = modelfitter::logistic_regression,
     "Poisson" = modelfitter::quasi_poisson
   )
)

tmp4 = tmp3 %>% execute_configuration(cache = TRUE)
```
