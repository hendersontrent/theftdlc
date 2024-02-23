#'
#' @docType package
#' @aliases thefttools-package
#' @name thefttools
#' @title Tools for Analysing and Interpreting Time Series Features
#'
#' @description Tools for Analysing and Interpreting Time Series Features
#'
#' @importFrom rlang .data
#' @importFrom stats IQR cor cor.test dist hclust median prcomp reorder sd var p.adjust quantile qt na.omit cmdscale
#' @importFrom scales rescale
#' @importFrom tidyr gather unnest_wider pivot_longer pivot_wider drop_na crossing
#' @importFrom broom augment tidy
#' @import tibble
#' @import dplyr
#' @import ggplot2
#' @importFrom normaliseR normalise
#' @importFrom e1071 svm
#' @importFrom purrr map map_dfr
#' @importFrom janitor clean_names
#' @importFrom MASS isoMDS sammon
#' @importFrom umap umap
#' @importFrom correctR resampled_ttest
NULL
