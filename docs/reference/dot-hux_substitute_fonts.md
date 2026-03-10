# Pick a locally installed font family that matches requested

Pick a locally installed font family that matches requested

## Usage

``` r
.hux_substitute_fonts(family)
```

## Arguments

- family:

  the font family requested

## Value

a mapping as a named list of font families that are present on the
system (names are the requested font family)

## Unit tests


    .hux_substitute_fonts(c("Roboto","Arial","Kings","Unmatched"))
