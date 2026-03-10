# Format a ratio, truncating at a set level above and below 1 on log scale.

Format a ratio, truncating at a set level above and below 1 on log
scale.

## Usage

``` r
format_ratio(x, fmt.ratio = "%1.2f", max.ratio = 50, na.ratio = "Unk")
```

## Arguments

- x:

  a vector of numbers

- fmt.ratio:

  a sprintf format string

- max.ratio:

  a max ratio after which to display as e.g. `>50` or `<0.02`

- na.ratio:

  a symbol in case the value is `NA`.

## Value

a string of formatted ratios

## Examples

``` r
format_ratio(2^(-6:6))
#>  [1] "<0.02"  "0.03"   "0.06"   "0.12"   "0.25"   "0.50"   "1.00"   "2.00"  
#>  [9] "4.00"   "8.00"   "16.00"  "32.00"  ">50.00"
```
