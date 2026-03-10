# Make a huxtable narrower

Converts row spanning columns into column spanning header rows making a
table narrower but longer. The column that is being moved is retained to
allow for the appearance of indentation.

## Usage

``` r
.hux_nest_group(t, col = 1)
```

## Arguments

- t:

  the huxtable

- col:

  the column index you want to nest into the row above

## Value

a narrower huxtable
