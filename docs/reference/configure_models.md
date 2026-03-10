# Configure a set of model fits from a specification of different combinations

Configure a set of model fits from a specification of different
combinations

## Usage

``` r
configure_models(
  model_formula,
  dataset,
  model_function,
  data_subset = data_subset_provider(default = TRUE),
  formula_update = formula_provider(default = . ~ .)
)
```

## Arguments

- model_formula:

  the model formula or formulae you want to fit as a
  `formula_provider(...)`.

- dataset:

  the dataset imputed or bootstrapped if need be as a
  `bootstrap_provider(...)` or `imputation_provider(...)`

- model_function:

  the model functions to fit as a `model_function_provider(...)`

- data_subset:

  the data subsets to fit as a `data_subset_provider(...)`

- formula_update:

  and model formula updates to apply before fitting as a
  `formula_provider(...)`.

## Value

a configuration dataframe and data provider to run

## Examples

``` r
form = ~ bmi + age + chl

fp = formula_provider(
  `Univariate` = modelfitter::univariate_from_multivariate(form),
  `Adj Model 1` = form,
  `Adj Model 2` = update(form, . ~ . - chl),
)

mfp = model_function_provider(
   `Logistic` = modelfitter::logistic_regression,
   `Poisson` = modelfitter::quasi_poisson
)

ip = imputation_provider(
    mice::nhanes2 %>% dplyr::mutate(death=TRUE), 10)

fup = formula_provider(
  hypertension = hyp ~ .,
  death = death ~ .
)

dsp = data_subset_provider(
  all = TRUE,
  hypertensives = hyp == "yes",
  nonhypertensives = hyp == "no" 
)

tmp = configure_models(fp, ip, mfp, dsp, fup) %>% dplyr::glimpse()
#> Rows: 48
#> Columns: 9
#> $ model_base_name   <chr> "Adj Model 1", "Adj Model 1", "Adj Model 1", "Adj Mo…
#> $ model_type_name   <chr> "Logistic", "Logistic", "Logistic", "Logistic", "Log…
#> $ data_subset_name  <chr> "all", "all", "hypertensives", "hypertensives", "non…
#> $ model_update_name <chr> "death", "hypertension", "death", "hypertension", "d…
#> $ data_subset_fn    <list> function (data) , data %>% dplyr::ungroup() %>% dpl…
#> $ model_fn          <list> function (data, formula, positive = c("yes", "true"…
#> $ n_boots           <int> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, …
#> $ model_formula     <list> <death ~ bmi + age + chl>, <hyp ~ bmi + age + chl>,…
#> $ model_name        <chr> "Adj Model 1 (death)\nLogistic", "Adj Model 1 (hyper…

# the number of models to fit will be this:
sum(tmp$n_boots)
#> [1] 480
```
