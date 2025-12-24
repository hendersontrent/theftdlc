# Perform cluster analysis of time series using their feature vectors

Perform cluster analysis of time series using their feature vectors

## Usage

``` r
cluster(
  data,
  norm_method = c("zScore", "Sigmoid", "RobustSigmoid", "MinMax", "MaxAbs"),
  unit_int = FALSE,
  clust_method = c("kmeans", "hclust", "mclust"),
  k = 2,
  features = NULL,
  na_removal = c("feature", "sample"),
  seed = 123,
  ...
)
```

## Arguments

- data:

  `feature_calculations` object containing the raw feature matrix
  produced by
  [`theft::calculate_features`](https://hendersontrent.github.io/theft/reference/calculate_features.html)

- norm_method:

  `character` denoting the rescaling/normalising method to apply. Can be
  one of `"zScore"`, `"Sigmoid"`, `"RobustSigmoid"`, `"MinMax"`, or
  `"MaxAbs"`. Defaults to `"zScore"`

- unit_int:

  `Boolean` whether to rescale into unit interval `[0,1]` after applying
  normalisation method. Defaults to `FALSE`

- clust_method:

  `character` specifying the clustering algorithm to use. Can be one of
  `"kmeans"` for k-means clustering, `"hclust"` for hierarchical
  clustering, or `"mclust"` for Gaussian mixture model clustering.
  Defaults to `"kMeans"`

- k:

  `integer` denoting the number of clusters to extract. Defaults to `2`

- features:

  `character` vector denoting the names of time-series features to use
  in the clustering algorithm. Defaults to `NULL` for no feature
  filtering and usage of the entire feature matrix

- na_removal:

  `character` defining the way to deal with NAs produced during feature
  calculation. Can be one of `"feature"` or `"sample"`. `"feature"`
  removes all features that produced any NAs in any sample, keeping the
  number of samples the same. `"sample"` omits all samples that produced
  at least one NA. Defaults to `"feature"`

- seed:

  `integer` to fix R's random number generator to ensure
  reproducibility. Defaults to `123`

- ...:

  arguments to be passed to
  [`stats::kmeans`](https://rdrr.io/r/stats/kmeans.html) or
  [`stats::hclust`](https://rdrr.io/r/stats/hclust.html), or
  [`mclust::Mclust`](https://mclust-org.github.io/mclust/reference/Mclust.html)
  depending on selection in `clust_method`

## Value

object of class `feature_cluster` containing the clustering algorithm
and a tidy version of clusters joined to the input dataset ready for
further analysis

## Author

Trent Henderson

## Examples

``` r
library(theft)

features <- theft::calculate_features(theft::simData,
  feature_set = "catch22")
#> Running computations for catch22...

clusts <- cluster(features,
  k = 6)
```
