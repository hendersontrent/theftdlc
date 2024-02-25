#' Perform cluster analysis of time series using their feature vectors
#'
#' @importFrom stats kmeans hclust
#' @importFrom rlang .data
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom dplyr %>% select group_by mutate ungroup filter inner_join
#' @importFrom tidyr drop_na pivot_wider pivot_longer
#' @importFrom normaliseR normalise
#' @importFrom mclust Mclust
#' @param data \code{feature_calculations} object containing the raw feature matrix produced by \code{theft::calculate_features}
#' @param norm_method \code{character} denoting the rescaling/normalising method to apply. Can be one of \code{"zScore"}, \code{"Sigmoid"}, \code{"RobustSigmoid"}, \code{"MinMax"}, or \code{"MaxAbs"}. Defaults to \code{"zScore"}
#' @param unit_int \code{Boolean} whether to rescale into unit interval \code{[0,1]} after applying normalisation method. Defaults to \code{FALSE}
#' @param clust_method \code{character} specifying the clustering algorithm to use. Can be one of \code{"kmeans"} for k-means clustering, \code{"hclust"} for hierarchical clustering, or \code{"mclust"} for Gaussian mixture model clustering. Defaults to \code{"kMeans"}
#' @param features \code{character} vector denoting the names of time-series features to use in the clustering algorithm. Defaults to \code{NULL} for no feature filtering and usage of the entire feature matrix
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
                    clust_method = c("kmeans", "hclust", "mclust"), features = NULL, seed = 123, ...){

  stopifnot(inherits(data, "feature_calculations") == TRUE)
  norm_method <- match.arg(norm_method)

  if(length(clust_method) > 1){
    k <- 3
    k_flag <- TRUE
    message("Using 'kmeans' with k = 3 by default.")
  }

  clust_method <- match.arg(clust_method)

  #------------------- Filter data -------------------

  if(!is.null(features)){

    # Check feature names are valid

    stopifnot(is.character(features))

    # Perform filtering

    filtered <- data %>%
      dplyr::filter(.data$names %in% features)

  } else{
    filtered <- data
  }

  #------------------- Normalise data -------------------

  normed <- filtered %>%
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

    # Fit k-means algorithm

    if(k_flag){
      clusts <- stats::kmeans(wide_data, k, ...)
    } else{
      clusts <- stats::kmeans(wide_data, ...)
    }

    # Extract results into a tidy format

    clust_info <- data.frame(id = rownames(wide_data), cluster = clusts$cluster)

    clust_tidy <- filtered %>%
      dplyr::inner_join(clust_info, by = c("id" = "id"))

    stopifnot(nrow(filtered) == nrow(clust_info)) # Check we didn't lose any data

  } else if(clust_method == "hclust"){

    # Fit hierarchical clustering algorithm

    clusts <- stats::hclust(wide_data, ...)

    # Extract results into a tidy format

    clust_info <- data.frame(id = rownames(wide_data), cluster = clusts$cluster)

    clust_tidy <- filtered %>%
      dplyr::inner_join(clust_info, by = c("id" = "id"))

    stopifnot(nrow(filtered) == nrow(clust_info)) # Check we didn't lose any data

  } else{
    x
  }
}
