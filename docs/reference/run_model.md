# Run a model through the `modelfitter` pipeline

**\[deprecated\]**

## Usage

``` r
run_model(
  data,
  formula,
  modelFunction = logistic_regression,
  ...,
  modelName = .fmt_form(formula)
)
```

## Arguments

- data:

  a single dataframe

- formula:

  a model formula

- modelFunction:

  the type of test as a function call. the function must accept `data`
  and `formula` inputs and produce an output compatible with
  [`broom::tidy`](https://generics.r-lib.org/reference/tidy.html) and
  [`broom::glance`](https://generics.r-lib.org/reference/glance.html)

- ...:

  can be used to provide parameters to the `modelFunction`.

- modelName:

  an optional (but recommended) name for the model

## Value

a dataframe with result of running all formulae on all bootstrapped
imputations adn summary stats

## Examples

``` r
# stats::lm( Petal.Length ~ Species + Petal.Width, iris)

# boots = iris %>% run_model(Petal.Length ~ Species + Petal.Width, stats::lm)
```
