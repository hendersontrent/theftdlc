# Fit classifiers using time-series features using a resample-based approach and get a fast understanding of performance

Fit classifiers using time-series features using a resample-based
approach and get a fast understanding of performance

## Usage

``` r
classify(
  data,
  tt_labels = NULL,
  classifier = NULL,
  train_size = 0.75,
  n_resamples = 30,
  by_set = TRUE,
  use_null = FALSE,
  filter_duplicates = FALSE,
  add_all_features = FALSE,
  n_workers = 1,
  seed = 123
)

tsfeature_classifier(
  data,
  tt_labels = NULL,
  classifier = NULL,
  train_size = 0.75,
  n_resamples = 30,
  by_set = TRUE,
  use_null = FALSE,
  filter_duplicates = FALSE,
  add_all_features = FALSE,
  n_workers = 1,
  seed = 123
)
```

## Arguments

- data:

  `feature_calculations` object containing the raw feature matrix
  produced by
  [`theft::calculate_features`](https://hendersontrent.github.io/theft/reference/calculate_features.html)

- tt_labels:

  `data.frame` containing data with two columns: an id column that
  aligns with the id column in `data` and a column for train-test
  designation of each id. Defaults to `NULL`. If `tt_labels = NULL`,
  labels will be created by constructing a new train-test split using
  \`train_size\`

- classifier:

  `function` specifying the classifier to fit. Should be a function with
  2 arguments: `formula` and `data` containing a classifier compatible
  with R's `predict` functionality. Please note that `classify` z-scores
  data prior to modelling using the train set's information so disabling
  default scaling if your function uses it is recommended. Defaults to
  `NULL` which means the following linear SVM is fit:
  `classifier = function(formula, data){mod <- e1071::svm(formula, data = data, kernel = "linear", scale = FALSE, probability = TRUE)}`

- train_size:

  `numeric` denoting the proportion of samples to use in the training
  set. Defaults to `0.75`

- n_resamples:

  `integer` denoting the number of resamples to calculate. Defaults to
  `30`

- by_set:

  `Boolean` specifying whether to compute classifiers for each feature
  set. Defaults to `TRUE`. If `FALSE`, the function will instead find
  the best individually-performing features

- use_null:

  `Boolean` whether to fit null models where class labels are shuffled
  in order to generate a null distribution that can be compared to
  performance on correct class labels. Defaults to `FALSE`

- filter_duplicates:

  `Boolean` denoting whether to filter duplicate features when
  `by_set = FALSE` or not. Only useful for determining top individual
  unique features for a problem. Defaults to `FALSE`

- add_all_features:

  `Boolean` denoting whether to construct a composite set of all
  features to use as a comparison. Only applicable if `by_set = TRUE`.
  Defaults to `FALSE`

- n_workers:

  `integer` denoting the number of parallel processes to use. Likely
  only beneficial for smaller datasets. Defaults to `1` for serial
  processing

- seed:

  `integer` to fix R's random number generator to ensure
  reproducibility. Defaults to `123`

## Value

`list` containing a named `vector` of train-test set sizes, and a
`data.frame` of classification performance results

## Author

Trent Henderson

## Examples

``` r
library(theft)

features <- theft::calculate_features(theft::simData,
  feature_set = "catch22")
#> Running computations for catch22...
#> Warning: There was 1 warning in `dplyr::reframe()`.
#> ℹ In argument: `Rcatch22::catch22_all(values, catch24 = catch24)`.
#> ℹ In group 1: `id = "AR(1)_1"` `process = "AR(1)"`.
#> Caused by warning:
#> ! As of 0.1.14 the feature 'CO_f1ecac' returns a double instead of int
#> This warning is displayed once per session.

classifiers <- classify(features,
  by_set = FALSE,
  n_resamples = 3)
#> Generating resampled data...
#> Fitting model 1/66
#> Fitting model 2/66
#> Fitting model 3/66
#> Fitting model 4/66
#> Fitting model 5/66
#> Fitting model 6/66
#> Fitting model 7/66
#> Fitting model 8/66
#> Fitting model 9/66
#> Fitting model 10/66
#> Fitting model 11/66
#> Fitting model 12/66
#> Fitting model 13/66
#> Fitting model 14/66
#> Fitting model 15/66
#> Fitting model 16/66
#> Fitting model 17/66
#> Fitting model 18/66
#> Fitting model 19/66
#> Fitting model 20/66
#> Fitting model 21/66
#> Fitting model 22/66
#> Fitting model 23/66
#> Fitting model 24/66
#> Fitting model 25/66
#> Fitting model 26/66
#> Fitting model 27/66
#> Fitting model 28/66
#> Fitting model 29/66
#> Fitting model 30/66
#> Fitting model 31/66
#> Fitting model 32/66
#> Fitting model 33/66
#> Fitting model 34/66
#> Fitting model 35/66
#> Fitting model 36/66
#> Fitting model 37/66
#> Fitting model 38/66
#> Fitting model 39/66
#> Fitting model 40/66
#> Fitting model 41/66
#> Fitting model 42/66
#> Fitting model 43/66
#> Fitting model 44/66
#> Fitting model 45/66
#> Fitting model 46/66
#> Fitting model 47/66
#> Fitting model 48/66
#> Fitting model 49/66
#> Fitting model 50/66
#> Fitting model 51/66
#> Fitting model 52/66
#> Fitting model 53/66
#> Fitting model 54/66
#> Fitting model 55/66
#> Fitting model 56/66
#> Fitting model 57/66
#> Fitting model 58/66
#> Fitting model 59/66
#> Fitting model 60/66
#> Fitting model 61/66
#> Fitting model 62/66
#> Fitting model 63/66
#> Fitting model 64/66
#> Fitting model 65/66
#> Fitting model 66/66
```
