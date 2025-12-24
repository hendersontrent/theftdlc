# Use a cross validated penalized maximum likelihood generalized linear model to perform feature selection

Use a cross validated penalized maximum likelihood generalized linear
model to perform feature selection

## Usage

``` r
shrink(data, threshold = c("one", "all"), plot = FALSE, ...)
```

## Arguments

- data:

  `feature_calculations` object containing the raw feature matrix
  produced by
  [`theft::calculate_features`](https://hendersontrent.github.io/theft/reference/calculate_features.html)

- threshold:

  `character` denoting whether to retain features that have at least one
  non-zero coefficient `"one"` across all group levels or features that
  have non-zero coefficients across all group levels `"all"`. Applicable
  to multinomial case only. Defaults to `"one"` for less aggressive
  filtering

- plot:

  `Boolean` whether to draw the misclassification error lambda plot for
  a `cv.glmnet` object. Defaults to `FALSE`

- ...:

  arguments to be passed to
  [`glmnet::cv.glmnet`](https://glmnet.stanford.edu/reference/cv.glmnet.html)

## Value

`feature_calculations` object containing a data frame of the reduced
feature set

## Author

Trent Henderson

## Examples

``` r
library(theft)

features <- theft::calculate_features(theft::simData,
  feature_set = "catch22")
#> Running computations for catch22...

best_features <- shrink(features)
```
