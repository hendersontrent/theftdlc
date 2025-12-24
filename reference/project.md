# Project a feature matrix into a two-dimensional representation using PCA, MDS, t-SNE, or UMAP ready for plotting

Project a feature matrix into a two-dimensional representation using
PCA, MDS, t-SNE, or UMAP ready for plotting

## Usage

``` r
project(
  data,
  norm_method = c("zScore", "Sigmoid", "RobustSigmoid", "MinMax", "MaxAbs"),
  unit_int = FALSE,
  low_dim_method = c("PCA", "tSNE", "ClassicalMDS", "KruskalMDS", "SammonMDS", "UMAP"),
  na_removal = c("feature", "sample"),
  seed = 123,
  ...
)

reduce_dims(
  data,
  norm_method = c("zScore", "Sigmoid", "RobustSigmoid", "MinMax", "MaxAbs"),
  unit_int = FALSE,
  low_dim_method = c("PCA", "tSNE", "ClassicalMDS", "KruskalMDS", "SammonMDS", "UMAP"),
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

- low_dim_method:

  `character` specifying the low dimensional embedding method to use.
  Can be one of `"PCA"`, `"tSNE"`, `"ClassicalMDS"`, `"KruskalMDS"`,
  `"SammonMDS"`, or `"UMAP"`. Defaults to `"PCA"`

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
  [`stats::prcomp`](https://rdrr.io/r/stats/prcomp.html) or
  [`Rtsne::Rtsne`](https://rdrr.io/pkg/Rtsne/man/Rtsne.html),
  [`stats::cmdscale`](https://rdrr.io/r/stats/cmdscale.html),
  [`MASS::isoMDS`](https://rdrr.io/pkg/MASS/man/isoMDS.html),
  [`MASS::sammon`](https://rdrr.io/pkg/MASS/man/sammon.html), or
  [`umap::umap`](https://rdrr.io/pkg/umap/man/umap.html) depending on
  selection in `low_dim_method`

## Value

object of class `feature_projection` which is a named list containing
the `feature_calculations` data supplied to the function, the wide
matrix of filtered data, a tidy `data.frame` of the projected 2-D data,
and the model fit object

## Author

Trent Henderson

## Examples

``` r
# \donttest{

library(theft)

features <- theft::calculate_features(theft::simData,
  feature_set = "catch22")
#> Running computations for catch22...

pca <- project(features,
  norm_method = "zScore",
  low_dim_method = "PCA")
# }
```
