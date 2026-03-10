# Run a set of models

**\[deprecated\]**

## Usage

``` r
run_models(
  imputedData,
  modelConfig,
  modelFunction = logistic_regression,
  pipeline = ~.x,
  ...
)
```

## Arguments

- imputedData:

  a mice imputed data source

- modelConfig:

  a dataframe with column `form` containing formulae to test, with other
  columns containing any other metadata. if provided `modelName` will be
  used to label the model in plots (otherwise the formula) will be used

- modelFunction:

  the type of test as a function call. the function must accept `data`
  and `formula` inputs

- pipeline:

  a pipeline function (i.e. takes a dataframe and returns a dataframe)
  that can be used to modify the imputed data

- ...:

  can be used to provide parameters to the `modelFunction`.

## Value

a dataframe with result of running all formulae on all bootstrapped
imputations adn summary stats
