# Format a confidence interval

Format a confidence interval

## Usage

``` r
format_ci(
  median,
  lower,
  upper,
  is_reference_value = FALSE,
  fmt.ci = "%s [%s – %s]",
  na.ci = "—",
  fn = format_ratio,
  ...
)
```

## Arguments

- median:

  the median value

- lower:

  the lower value

- upper:

  the upper quantile

- is_reference_value:

  is the value a reference value

- fmt.ci:

  the layout of the 3 elements as a `sprintf` using `%s` for each
  element

- na.ci:

  the value to show if the median ci is `NA`

- fn:

  a function to format each number

- ...:

  Named arguments passed on to [`format_ratio`](format_ratio.md)

  `x`

  :   a vector of numbers

  `fmt.ratio`

  :   a sprintf format string

  `max.ratio`

  :   a max ratio after which to display as e.g. `>50` or `<0.02`

  `na.ratio`

  :   a symbol in case the value is `NA`.

## Value

a formatted CI string

## Examples

``` r
format_ci(
  median = 2^(-5:5),
  lower = 2^(-5:5-1),
  upper = 2^(-5:5+1),
  fmt.ratio = "%1.3g"
)
#>  [1] "0.0312 [<0.02 – 0.0625]" "0.0625 [0.0312 – 0.125]"
#>  [3] "0.125 [0.0625 – 0.25]"   "0.25 [0.125 – 0.5]"     
#>  [5] "0.5 [0.25 – 1]"          "1 [0.5 – 2]"            
#>  [7] "2 [1 – 4]"               "4 [2 – 8]"              
#>  [9] "8 [4 – 16]"              "16 [8 – 32]"            
#> [11] "32 [16 – >50]"          
```
