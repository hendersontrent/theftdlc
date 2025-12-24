# Conduct statistical testing on time-series feature classification performance to identify top features or compare entire sets

Conduct statistical testing on time-series feature classification
performance to identify top features or compare entire sets

## Usage

``` r
compare_features(
  data,
  metric = c("accuracy", "precision", "recall", "f1"),
  by_set = TRUE,
  hypothesis = c("null", "pairwise"),
  p_adj = c("none", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr"),
  n_workers = 1
)
```

## Arguments

- data:

  `list` object containing the classification outputs produce by
  `tsfeature_classifier`

- metric:

  `character` denoting the classification performance metric to use in
  statistical testing. Can be one of `"accuracy"`, `"precision"`,
  `"recall"`, `"f1"`. Defaults to `"accuracy"`

- by_set:

  `Boolean` specifying whether you want to compare feature sets (if
  `TRUE`) or individual features (if `FALSE`). Defaults to `TRUE` but
  this is contingent on whether you computed by set or not in
  `tsfeature_classifier`

- hypothesis:

  `character` denoting whether p-values should be calculated for each
  feature set or feature (depending on `by_set` argument) individually
  relative to the null if `use_null = TRUE` in `tsfeature_classifier`
  through `"null"`, or whether pairwise comparisons between each set or
  feature should be conducted on main model fits only through
  `"pairwise"`. Defaults to `"null"`

- p_adj:

  `character` denoting the adjustment made to p-values for multiple
  comparisons. Should be a valid argument to
  [`stats::p.adjust`](https://rdrr.io/r/stats/p.adjust.html). Defaults
  to `"none"` for no adjustment. `"holm"` is recommended as a starting
  point for adjustments

- n_workers:

  `integer` denoting the number of parallel processes to use. Defaults
  to `1` for serial processing

## Value

`data.frame` containing the results

## References

Henderson, T., Bryant, A. G., and Fulcher, B. D. Never a Dull Moment:
Distributional Properties as a Baseline for Time-Series Classification.
27th Pacific-Asia Conference on Knowledge Discovery and Data Mining,
(2023).

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

compare_features(classifiers,
                 by_set = FALSE,
                 hypothesis = "pairwise")
#> Calculating comparison 1/1
#>             hypothesis   names_a names_b   metric names_a_mean names_b_mean
#> 1 User_mean != User_sd User_mean User_sd accuracy    0.1259259     0.562963
#>   t_statistic     p.value
#> 1      -14.75 0.004564931
```
