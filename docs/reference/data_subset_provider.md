# Provide a mechanism for subsetting data

A data subset may be used for a sensitivity analysis. This provides a
mechanism to filter the data within the pipeline.

## Usage

``` r
data_subset_provider(...)
```

## Arguments

- ...:

  a set of named data filter criteria. In most cases you will want to
  include a `default = TRUE` option in this list. (this is the default
  value)

## Value

a data subset provider which can fiter data based on a named set of
subset criteria

## Examples

``` r
dsp = data_subset_provider(one = TRUE, two = Species == "versicolor", three = Sepal.Width < 2.6)
dsp()
#> [1] "one"   "two"   "three"
f = dsp("three")
f(iris) 
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1           4.5         2.3          1.3         0.3     setosa
#> 2           5.5         2.3          4.0         1.3 versicolor
#> 3           4.9         2.4          3.3         1.0 versicolor
#> 4           5.0         2.0          3.5         1.0 versicolor
#> 5           6.0         2.2          4.0         1.0 versicolor
#> 6           6.2         2.2          4.5         1.5 versicolor
#> 7           5.6         2.5          3.9         1.1 versicolor
#> 8           6.3         2.5          4.9         1.5 versicolor
#> 9           5.5         2.4          3.8         1.1 versicolor
#> 10          5.5         2.4          3.7         1.0 versicolor
#> 11          6.3         2.3          4.4         1.3 versicolor
#> 12          5.5         2.5          4.0         1.3 versicolor
#> 13          5.0         2.3          3.3         1.0 versicolor
#> 14          5.1         2.5          3.0         1.1 versicolor
#> 15          4.9         2.5          4.5         1.7  virginica
#> 16          6.7         2.5          5.8         1.8  virginica
#> 17          5.7         2.5          5.0         2.0  virginica
#> 18          6.0         2.2          5.0         1.5  virginica
#> 19          6.3         2.5          5.0         1.9  virginica

dsp2 = data_subset_provider()
dsp2("default")(mtcars) %>% head(10)
#>                    mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360        14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D         24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230          22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280          19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
```
