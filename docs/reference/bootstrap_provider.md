# Provide a access to bootstrap resamples of a dataset

Provide a access to bootstrap resamples of a dataset

## Usage

``` r
bootstrap_provider(data, max_n, formulae = ~.)
```

## Arguments

- data:

  the data frame

- max_n:

  the maximum number of different bootstraps

- formulae:

  a list of formulae with all the columns used

## Value

a function that returns a dataset for inputs between `1:max_n`

## Examples

``` r
bp = iris %>% bootstrap_provider(10)
bp(1) %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1          5.8         2.7          4.1         1.0 versicolor
#> 2          6.4         2.8          5.6         2.1  virginica
#> 3          4.4         3.2          1.3         0.2     setosa
#> 4          4.3         3.0          1.1         0.1     setosa
#> 5          7.0         3.2          4.7         1.4 versicolor
#> 6          5.4         3.0          4.5         1.5 versicolor
tmp = bp(1)
tmp2 = bp(1)
identical(tmp,tmp2)
#> [1] TRUE
```
