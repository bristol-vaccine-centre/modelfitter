# Construct a configuration for multiple formulae

multiple formulae for a parameterised model fitting pipeline. Different
models may be a key part of the analysis or a sensitivity. A single name
maybe associated with one or many models - This is because it is
frequent that we want to group together models such as univariate
comparisons into the same format table as another single multivarible
model. Another use case for multiple models is a single set of
predictors

## Usage

``` r
formula_provider(...)
```

## Arguments

- ...:

  a named list of formulae or formulae lists. In the simplest use case
  this is a single formula, see examples for more possibilities.

## Examples

``` r
form = is_coloured ~ cut + carat + clarity * price
fp = formula_provider(
  `Univariate` = modelfitter::univariate_from_multivariate(form),
  `Adj Model 1` = form,
  `Adj Model 2` = update(form, . ~ . - clarity * price),
)


predictors = ~ x + y + z 
fp2 = formula_provider(
  `Death` = death ~ .,
  `ICU admission` = icu ~ .,
  `Delayed discharge` = delayed_discharge ~ .,
)
sapply(fp2(), fp2) %>% sapply(update, predictors)
#> $Death
#> death ~ x + y + z
#> <environment: 0x5a06666cab88>
#> 
#> $`ICU admission`
#> icu ~ x + y + z
#> <environment: 0x5a06666cab88>
#> 
#> $`Delayed discharge`
#> delayed_discharge ~ x + y + z
#> <environment: 0x5a06666cab88>
#> 

# the simplest configuration
fp3 = formula_provider(outcome ~ x + y + z)
fp3()
#> [1] "default"

fp4 = formula_provider(outcome1 ~ x + y + z, outcome2 ~ x + y + z)
fp4()
#> [1] "outcome1 ~ x + y + z" "outcome2 ~ x + y + z"
try(fp4("blah blah"))
#> Error : formula_provider: `blah blah` is not a known index of out: outcome1 ~ x + y + z, outcome2 ~ x + y + z

# must define at least one forumula
try(formula_provider())
#> Error : you must supply a model formula.

formula_provider(default = . ~ .)
#> `default`
```
