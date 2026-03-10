# A quantile function for a mixture of normal distributions

A quantile function for a mixture of normal distributions

## Usage

``` r
qmixnorm(p, means, sds, weights = rep(1, length(means)), na.rm = FALSE)
```

## Arguments

- p:

  vector of probabilities.

- means:

  a vector of normal distribution means

- sds:

  a vector of normal distribution sds

- weights:

  a vector of weights

- na.rm:

  remove distributions with NA values for mean or sd

## Value

the value of the yth quantile

## Examples

``` r
qmixnorm(p=c(0.025,0.5,0.975), means=c(10,13,14), sds=c(1,1,2))
#>   Q.0.025     Q.0.5   Q.0.975 
#>  8.537776 12.443750 16.879802 
```
