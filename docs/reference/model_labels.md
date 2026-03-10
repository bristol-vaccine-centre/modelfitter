# Creates a table or plot row template for a set of models

This produces a dataframe which can be used to arrange the coefficients
and p-values from one statistical model, or set of models. The rows are
a superset of all the coefficients of the models and is designed to be
used to left_join the outputs of
[`broom::tidy`](https://generics.r-lib.org/reference/tidy.html) (by
`term`) or `global_p_value` (by `predictor`) to construct a tabular
output.

## Usage

``` r
model_labels(model, label_fn, subgroup_label_fn, ...)
```

## Arguments

- model:

  a model or list of models.

- label_fn:

  a function that allows a predictor label to be renamed. This should
  expect a vector and return a vector of the same length. Levels will be
  terms in the model function and may be column names, or combinations
  thereof

- subgroup_label_fn:

  a function that allows a subgroup label to be renamed. This should
  expect a vector and return a vector of the same length. The input to
  this function will be either a factor level name or a combination of
  them or whatever else the model decides to name it's coefficients.

- ...:

  not used

## Value

a data frame with `model.order`, `group.order`, `subgroup.order`,
`characteristic`, `subgroup`, dplaye columns and `predictor` and `term`
key columns to link to the output of
[`broom::tidy`](https://generics.r-lib.org/reference/tidy.html) or
`global_p_value` (i.e. Anova II/III outputs)

## Details

It is expect that use cases such as multiple univariate models + a few
fully adjusted models are passed to this function and the result is to
be displayed in a single plot or figure.

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

model = stats::glm(is_coloured ~ cut + carat + clarity * price, diamonds3, family="binomial")
model_labels(model, toupper, tolower)
#> # A tibble: 21 × 9
#>    group.order model.order subgroup.order characteristic subgroup    predictor
#>          <int>       <dbl>          <dbl> <chr>          <chr>       <chr>    
#>  1           1           1              1 CUT            "fair"      cut      
#>  2           1           1              2 CUT            "good"      cut      
#>  3           1           1              3 CUT            "very good" cut      
#>  4           1           1              4 CUT            "premium"   cut      
#>  5           1           1              5 CUT            "ideal"     cut      
#>  6           2           1              1 CARAT          ""          carat    
#>  7           3           1              1 CLARITY        "linear"    clarity  
#>  8           3           1              2 CLARITY        "quadratic" clarity  
#>  9           3           1              3 CLARITY        "cubic"     clarity  
#> 10           3           1              4 CLARITY        "4th power" clarity  
#> # ℹ 11 more rows
#> # ℹ 3 more variables: term <chr>, reference <lgl>, group.type <chr>


model2 = logistic_regression(diamonds3, is_coloured ~ I(cut=="Good") + carat + clarity * price)
model_labels(model2, toupper, tolower)
#> # A tibble: 19 × 9
#>    group.order model.order subgroup.order characteristic      subgroup predictor
#>          <int>       <dbl>          <dbl> <chr>               <chr>    <chr>    
#>  1           1           1            1   "I(CUT == \"GOOD\"… ""       "I(cut =…
#>  2           2           1            1   "CARAT"             ""       "carat"  
#>  3           3           1            1   "CLARITY"           "i1"     "clarity"
#>  4           3           1            2   "CLARITY"           "si2"    "clarity"
#>  5           3           1            3   "CLARITY"           "si1"    "clarity"
#>  6           3           1            4   "CLARITY"           "vs2"    "clarity"
#>  7           3           1            5   "CLARITY"           "vs1"    "clarity"
#>  8           3           1            6   "CLARITY"           "vvs2"   "clarity"
#>  9           3           1            7   "CLARITY"           "vvs1"   "clarity"
#> 10           3           1            8   "CLARITY"           "if"     "clarity"
#> 11           4           1            1   "PRICE"             ""       "price"  
#> 12           5           1            1.1 "CLARITY & PRICE"   "i1:pri… "clarity…
#> 13           5           1            2.1 "CLARITY & PRICE"   "si2:pr… "clarity…
#> 14           5           1            3.1 "CLARITY & PRICE"   "si1:pr… "clarity…
#> 15           5           1            4.1 "CLARITY & PRICE"   "vs2:pr… "clarity…
#> 16           5           1            5.1 "CLARITY & PRICE"   "vs1:pr… "clarity…
#> 17           5           1            6.1 "CLARITY & PRICE"   "vvs2:p… "clarity…
#> 18           5           1            7.1 "CLARITY & PRICE"   "vvs1:p… "clarity…
#> 19           5           1            8.1 "CLARITY & PRICE"   "if:pri… "clarity…
#> # ℹ 3 more variables: term <chr>, reference <lgl>, group.type <chr>

model3 = stats::glm(is_coloured ~ carat + cut * clarity +  price, diamonds3, family="binomial")
model_labels(model3, toupper, tolower)
#> # A tibble: 49 × 9
#>    group.order model.order subgroup.order characteristic subgroup    predictor
#>          <int>       <dbl>          <dbl> <chr>          <chr>       <chr>    
#>  1           1           1              1 CARAT          ""          carat    
#>  2           2           1              1 CUT            "fair"      cut      
#>  3           2           1              2 CUT            "good"      cut      
#>  4           2           1              3 CUT            "very good" cut      
#>  5           2           1              4 CUT            "premium"   cut      
#>  6           2           1              5 CUT            "ideal"     cut      
#>  7           3           1              1 CLARITY        "linear"    clarity  
#>  8           3           1              2 CLARITY        "quadratic" clarity  
#>  9           3           1              3 CLARITY        "cubic"     clarity  
#> 10           3           1              4 CLARITY        "4th power" clarity  
#> # ℹ 39 more rows
#> # ℹ 3 more variables: term <chr>, reference <lgl>, group.type <chr>


model4 = stats::glm(
  is_coloured ~ cut + carat + clarity + price, 
  diamonds3, 
  family="binomial", 
  contrasts=list(clarity=MASS::contr.sdif))
  
coef(model4)
#>      (Intercept)          cutGood     cutVery Good       cutPremium 
#>     3.1782245915    -0.1805654156    -0.3240623074    -0.5089493830 
#>         cutIdeal            carat    claritySI2-I1   claritySI1-SI2 
#>    -0.5255240571    -6.6953667226    -0.8303005659    -0.8801216212 
#>   clarityVS2-SI1   clarityVS1-VS2  clarityVVS2-VS1 clarityVVS1-VVS2 
#>    -0.4757192720    -0.6990111856    -0.1052882540    -0.3432349687 
#>   clarityIF-VVS1            price 
#>    -0.6155115732     0.0006053446 
model_labels(model4, toupper, tolower)
#> # A tibble: 14 × 9
#>    group.order model.order subgroup.order characteristic subgroup    predictor
#>          <int>       <dbl>          <dbl> <chr>          <chr>       <chr>    
#>  1           1           1              1 CUT            "fair"      cut      
#>  2           1           1              2 CUT            "good"      cut      
#>  3           1           1              3 CUT            "very good" cut      
#>  4           1           1              4 CUT            "premium"   cut      
#>  5           1           1              5 CUT            "ideal"     cut      
#>  6           2           1              1 CARAT          ""          carat    
#>  7           3           1              1 CLARITY        "si2-i1"    clarity  
#>  8           3           1              2 CLARITY        "si1-si2"   clarity  
#>  9           3           1              3 CLARITY        "vs2-si1"   clarity  
#> 10           3           1              4 CLARITY        "vs1-vs2"   clarity  
#> 11           3           1              5 CLARITY        "vvs2-vs1"  clarity  
#> 12           3           1              6 CLARITY        "vvs1-vvs2" clarity  
#> 13           3           1              7 CLARITY        "if-vvs1"   clarity  
#> 14           4           1              1 PRICE          ""          price    
#> # ℹ 3 more variables: term <chr>, reference <lgl>, group.type <chr>

# tmp = .ordered_contrasts(diamonds3, )
# model5 = stats::glm(
#   is_coloured ~ cut + carat + clarity * price, 
#   diamonds3, 
#   family="binomial", contrasts=tmp)
#   
# model_labels(model5, toupper, tolower)

model6 = stats::glm(
  is_coloured ~ cut + carat + clarity + price_cat, 
  diamonds3, 
  family="binomial", 
  contrasts=list(clarity=MASS::contr.sdif, price_cat=MASS::contr.sdif)
)

model_labels(model6, toupper, tolower)
#> # A tibble: 17 × 9
#>    group.order model.order subgroup.order characteristic subgroup      predictor
#>          <int>       <dbl>          <dbl> <chr>          <chr>         <chr>    
#>  1           1           1              1 CUT            "fair"        cut      
#>  2           1           1              2 CUT            "good"        cut      
#>  3           1           1              3 CUT            "very good"   cut      
#>  4           1           1              4 CUT            "premium"     cut      
#>  5           1           1              5 CUT            "ideal"       cut      
#>  6           2           1              1 CARAT          ""            carat    
#>  7           3           1              1 CLARITY        "si2-i1"      clarity  
#>  8           3           1              2 CLARITY        "si1-si2"     clarity  
#>  9           3           1              3 CLARITY        "vs2-si1"     clarity  
#> 10           3           1              4 CLARITY        "vs1-vs2"     clarity  
#> 11           3           1              5 CLARITY        "vvs2-vs1"    clarity  
#> 12           3           1              6 CLARITY        "vvs1-vvs2"   clarity  
#> 13           3           1              7 CLARITY        "if-vvs1"     clarity  
#> 14           4           1              1 PRICE_CAT      "500-999-<50… price_cat
#> 15           4           1              2 PRICE_CAT      "1000-1999-5… price_cat
#> 16           4           1              3 PRICE_CAT      "2000-3999-1… price_cat
#> 17           4           1              4 PRICE_CAT      "4000+-2000-… price_cat
#> # ℹ 3 more variables: term <chr>, reference <lgl>, group.type <chr>

model7 = stats::glm(
  is_coloured ~ cut + carat + clarity + splines::ns(price,df=2), 
  diamonds3, 
  family="binomial", 
  contrasts=list(clarity=MASS::contr.sdif))
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
  
model_labels(model7, toupper)
#> # A tibble: 15 × 9
#>    group.order model.order subgroup.order characteristic      subgroup predictor
#>          <int>       <dbl>          <dbl> <chr>               <chr>    <chr>    
#>  1           1           1              1 CUT                 "Fair"   cut      
#>  2           1           1              2 CUT                 "Good"   cut      
#>  3           1           1              3 CUT                 "Very G… cut      
#>  4           1           1              4 CUT                 "Premiu… cut      
#>  5           1           1              5 CUT                 "Ideal"  cut      
#>  6           2           1              1 CARAT               ""       carat    
#>  7           3           1              1 CLARITY             "SI2-I1" clarity  
#>  8           3           1              2 CLARITY             "SI1-SI… clarity  
#>  9           3           1              3 CLARITY             "VS2-SI… clarity  
#> 10           3           1              4 CLARITY             "VS1-VS… clarity  
#> 11           3           1              5 CLARITY             "VVS2-V… clarity  
#> 12           3           1              6 CLARITY             "VVS1-V… clarity  
#> 13           3           1              7 CLARITY             "IF-VVS… clarity  
#> 14           4           1              1 SPLINES::NS(PRICE,… "1"      splines:…
#> 15           4           1              2 SPLINES::NS(PRICE,… "2"      splines:…
#> # ℹ 3 more variables: term <chr>, reference <lgl>, group.type <chr>
```
