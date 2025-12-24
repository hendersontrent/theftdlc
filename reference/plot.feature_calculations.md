# Produce a plot for a feature_calculations object

Produce a plot for a feature_calculations object

## Usage

``` r
# S3 method for class 'feature_calculations'
plot(
  x,
  type = c("matrix", "cor", "violin", "box", "quality"),
  norm_method = c("zScore", "Sigmoid", "RobustSigmoid", "MinMax", "MaxAbs"),
  unit_int = FALSE,
  clust_method = c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty",
    "median", "centroid"),
  cor_method = c("pearson", "spearman"),
  feature_names = NULL,
  ...
)
```

## Arguments

- x:

  `feature_calculations` object containing the raw feature matrix
  produced by
  [`theft::calculate_features`](https://hendersontrent.github.io/theft/reference/calculate_features.html)

- type:

  `character` specifying the type of plot to draw. Can be one of
  `"matrix"`, `"cor"`, `"violin"`, `"box"`, or `"quality"`. Defaults to
  `"matrix"`

- norm_method:

  `character` specifying a rescaling/normalising method to apply if
  `type = "matrix"` or if `type = "cor"`. Can be one of `"zScore"`,
  `"Sigmoid"`, `"RobustSigmoid"`, `"MinMax"`, or `"MaxAbs"`. Defaults to
  `"zScore"`

- unit_int:

  `Boolean` whether to rescale into unit interval `[0,1]` after applying
  normalisation method. Defaults to `FALSE`

- clust_method:

  `character` specifying the hierarchical clustering method to use if
  `type = "matrix"` or if `type = "cor"`. Defaults to `"average"`

- cor_method:

  `character` specifying the correlation method to use if
  `type = "cor"`. Defaults to `"pearson"`

- feature_names:

  `character` vector denoting the name of the features to plot if
  `type = "violin"`. Defaults to `NULL`

- ...:

  Arguments to be passed to
  [`ggplot2::geom_bar`](https://ggplot2.tidyverse.org/reference/geom_bar.html)
  if `type = "quality"`,
  [`ggplot2::geom_raster`](https://ggplot2.tidyverse.org/reference/geom_tile.html)
  if `type = "matrix"`,
  [`ggplot2::geom_raster`](https://ggplot2.tidyverse.org/reference/geom_tile.html)
  if `type = "cor"`, or
  [`ggplot2::geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)
  if `type = "violin"`

## Value

object of class `ggplot` that contains the graphic

## Author

Trent Henderson
