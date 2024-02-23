
# thefttools <img src="man/figures/logo.png" align="right" width="120" />

Tools for Analysing and Interpreting Time Series Features

## Installation

*Coming to CRAN soon!*

You can install the development version of `thefttools` from GitHub
using the following:

``` r
devtools::install_github("hendersontrent/thefttools")
```

## General purpose

The [`theft`](https://hendersontrent.github.io/theft/) package for R
facilitates user-friendly access to a structured analytical workflow for
the extraction of time-series features from six different feature sets
(or a set of user-supplied features): `"catch22"`, `"feasts"`, `"Kats"`,
`"tsfeatures"`, `"tsfresh"`, and `"TSFEL"`.

`thefttools` extends this feature-based ecosystem by providing a suite
of functions for analysing, interpreting, and visualising time-series
features calculated using `theft`. Functionality including data quality
assessments and normalisation methods, low dimensional projections
(linear and nonlinear), data matrix and feature distribution
visualisations, time-series classification machine learning procedures,
statistical hypothesis testing, and various other statistical and
graphical tools.
