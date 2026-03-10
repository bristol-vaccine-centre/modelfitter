# Construct a configuration for multiple statistical models

multiple statistical models for a parameterised model fitting pipeline.

## Usage

``` r
model_function_provider(...)
```

## Arguments

- ...:

  a named list of functions. In the simplest use case this is a single
  function, see examples for more possibilities. the functions must
  accept `data` as first parameter and `formula` as second.

## Examples

``` r
# in the simplest version the name is pulled from the input
mfp = model_function_provider(logistic_regression)
mfp
#> `logistic_regression`
mfp()
#> [1] "logistic_regression"

mfp = model_function_provider(logistic_regression, quasi_poisson)
mfp()
#> [1] "logistic_regression" "quasi_poisson"      

mfp2 = model_function_provider(
   Logistic = modelfitter::logistic_regression,
   Poisson = modelfitter::quasi_poisson
)
mfp2()
#> [1] "Logistic" "Poisson" 
 
# the functions are named the other way round to normal model functions
# this was by design to fit into a tidy pipeline:
mfp = model_function_provider(linear = ~ stats::lm(.y,.x))
linear_model = mfp("linear")
iris %>% linear_model(Petal.Length ~ Petal.Width)
#> 
#> Call:
#> stats::lm(formula = .y, data = .x)
#> 
#> Coefficients:
#> (Intercept)  Petal.Width  
#>       1.084        2.230  
#> 
```
