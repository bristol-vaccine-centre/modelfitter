# A tidy article theme for huxtables

The main aim is to get something that works with google docs when you
copy and paste.

## Usage

``` r
.hux_default_layout(
  hux,
  defaultFontSize = 8,
  defaultFont = "Roboto",
  headerRows = 1
)
```

## Arguments

- hux:

  a huxtable object

- defaultFontSize:

  default size of font in points (8)

- defaultFont:

  the font family name

- headerRows:

  the number of rows that are headers

## Value

the formatted huxtable.

## Unit tests


    hux = iris 
