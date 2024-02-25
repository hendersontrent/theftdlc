#' Perform cluster analysis of time series using their feature vectors
#'
#' @importFrom stats kmeans hclust
#' @importFrom rlang .data
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom dplyr %>% select group_by mutate ungroup filter
#' @importFrom tidyr drop_na pivot_wider
#' @importFrom normaliseR normalise
#' @importFrom mclust Mclust
#' @param data \code{feature_calculations} object containing the raw feature matrix produced by \code{theft::calculate_features}
#' @param norm_method \code{character} denoting the rescaling/normalising method to apply. Can be one of \code{"zScore"}, \code{"Sigmoid"}, \code{"RobustSigmoid"}, \code{"MinMax"}, or \code{"MaxAbs"}. Defaults to \code{"zScore"}
#' @param unit_int \code{Boolean} whether to rescale into unit interval \code{[0,1]} after applying normalisation method. Defaults to \code{FALSE}
#' @param clust_method \code{character} specifying the clustering algorithm to use. Can be one of \code{"kmeans"} for k-means clustering, \code{"hclust"} for hierarchical clustering, or \code{"mclust"} for Gaussian mixture model clustering. Defaults to \code{"kMeans"}
#' @param seed \code{integer} to fix R's random number generator to ensure reproducibility. Defaults to \code{123}
#' @param ... arguments to be passed to \code{stats::kmeans} or \code{stats::hclust}, or \code{mclust::Mclust} depending on selection in \code{clust_method}
#' @return object of class \code{feature_cluster}
#' @author Trent Henderson
#' @export
#' @examples
#' \donttest{
#'
#' library(theft)
#'
#' features <- theft::calculate_features(theft::simData,
#'   group_var = "process",
#'   feature_set = "catch22")
#'
#' clusts <- cluster(features,
#'   norm_method = "zScore",
#'   low_dim_method = "PCA")
#' }
#'

cluster <- function(data, norm_method = c("zScore", "Sigmoid", "RobustSigmoid", "MinMax"), unit_int = FALSE,
                    clust_method = c("kmeans", "hclust", "mclust"),
                    seed = 123, ...){

  stopifnot(inherits(data, "feature_calculations") == TRUE)
  norm_method <- match.arg(norm_method)
  clust_method <- match.arg(clust_method)

  #------------------- Normalise data -------------------

  normed <- data %>%
    dplyr::select(c(.data$id, .data$names, .data$values, .data$feature_set)) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$names) %>%
    dplyr::mutate(values = normaliseR::normalise(.data$values, norm_method = norm_method, unit_int = unit_int)) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na() %>%
    dplyr::mutate(names = paste0(.data$feature_set, "_", .data$names)) %>% # Catches errors when using all features across sets (i.e., there's duplicates)
    dplyr::select(-c(.data$feature_set))

  #------------------- Perform clustering ----------------------

  # Produce matrix

  wide_data <- normed %>%
    tidyr::pivot_wider(id_cols = "id", names_from = "names", values_from = "values") %>%
    tibble::column_to_rownames(var = "id") %>%
    tidyr::drop_na()

  set.seed(123)

  if(clust_method == "kmeans"){
    x
  } else if(clust_method == "hclust"){
    x
  } else{
    x
  }
}
