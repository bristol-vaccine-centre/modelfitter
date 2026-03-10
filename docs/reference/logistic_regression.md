# Fit standard logistic regression with unordered factors.

This makes an effort to cast the result column into a logical from a
factor or other data type. It also will convert ordered predictors into
unordered before running.

## Usage

``` r
logistic_regression(
  data,
  formula,
  positive = c("yes", "true", "present", "confirmed"),
  ...
)
```

## Arguments

- data:

  a data frame

- formula:

  a formula of the form `binary_outcome ~ obs1 + obs2 + ...`

- positive:

  test strings to interpret as true if outcome is not a factor or
  logical.

- ...:

  not used

## Value

a model object

## Examples

``` r
diamonds3 = ggplot2::diamonds %>% dplyr::mutate(
  is_coloured = color <= "F",
  cut = factor(cut,ordered=FALSE),
  price_cat = cut(price, 
      breaks = c(0,500,1000,2000,4000,Inf),
      labels = c("<500","500-999","1000-1999","2000-3999","4000+"),
      ordered_result = TRUE)
) %>% dplyr::select(-color)

model5 = diamonds3 %>% 
  logistic_regression(is_coloured ~ cut + carat + clarity * price)
model6 = ggplot2::diamonds %>% 
  logistic_regression(I(color <= "F") ~ cut + carat + clarity * price)

# this wont work as the factors are converted to unordered
# model6 = ggplot2::diamonds %>% 
#   logistic_regression(I(color <= "F") ~ I(cut<"Very Good") + carat + clarity * price)

```
