# Calculate interval summaries with a measure of central tendency of classification results

Calculate interval summaries with a measure of central tendency of
classification results

## Usage

``` r
interval(
  data,
  metric = c("accuracy", "precision", "recall", "f1"),
  by_set = TRUE,
  type = c("sd", "se", "quantile"),
  interval = NULL,
  model_type = c("main", "null")
)

calculate_interval(
  data,
  metric = c("accuracy", "precision", "recall", "f1"),
  by_set = TRUE,
  type = c("sd", "se", "quantile"),
  interval = NULL,
  model_type = c("main", "null")
)
```

## Arguments

- data:

  `list` object containing the classification outputs produce by
  `tsfeature_classifier`

- metric:

  `character` denoting the classification performance metric to
  calculate intervals for. Can be one of `"accuracy"`, `"precision"`,
  `"recall"`, `"f1"`. Defaults to `"accuracy"`

- by_set:

  `Boolean` specifying whether to compute intervals for each feature
  set. Defaults to `TRUE`. If `FALSE`, the function will instead
  calculate intervals for each feature

- type:

  `character` denoting whether to calculate a +/- SD interval with
  `"sd"`, confidence interval based off the t-distribution with `"se"`,
  or based on a quantile with `"quantile"`. Defaults to `"sd"`

- interval:

  `numeric` scalar denoting the width of the interval to calculate.
  Defaults to `1` if `type = "sd"` to produce a +/- 1 SD interval.
  Defaults to `0.95` if `type = "se"` or `type = "quantile"` for a 95
  per cent interval

- model_type:

  `character` denoting whether to calculate intervals for main models
  with `"main"` or null models with `"null"` if the `use_null` argument
  when using `tsfeature_classifier` was `use_null = TRUE`. Defaults to
  `"main"`

## Value

`interval_calculations` object which is a data frame containing the
results

## Author

Trent Henderson

## Examples

``` r
library(theft)

features <- theft::calculate_features(theft::simData,
  feature_set = NULL,
  features = list("mean" = mean, "sd" = sd))
#> Running computations for user-supplied features...

classifiers <- classify(features,
  by_set = FALSE,
  n_resamples = 3)
#> Generating resampled data...
#> Fitting model 1/6
#> Fitting model 2/6
#> Fitting model 3/6
#> Fitting model 4/6
#> Fitting model 5/6
#> Fitting model 6/6

interval(classifiers,
  by_set = FALSE,
  type = "sd",
  interval = 1)
#>       names     .mean    .lower    .upper
#> 1 User_mean 0.1259259 0.1130959 0.1387559
#> 2   User_sd 0.5629630 0.5501330 0.5757930
```
