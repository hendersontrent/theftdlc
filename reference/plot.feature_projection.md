# Produce a plot for a feature_projection object

Produce a plot for a feature_projection object

## Usage

``` r
# S3 method for class 'feature_projection'
plot(x, show_covariance = TRUE, ...)
```

## Arguments

- x:

  `feature_projection` object containing the two-dimensional embedding
  calculated by `project`

- show_covariance:

  `Boolean` specifying whether covariance ellipses should be shown on
  the plot. Defaults to `TRUE`

- ...:

  Arguments to be passed to methods

## Value

object of class `ggplot` that contains the graphic

## Author

Trent Henderson
