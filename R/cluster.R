#' Perform cluster analysis of time series using their feature vectors
#'
#' @importFrom stats kmeans hclust dist cutree
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
#' @param k \code{integer} denoting the number of clusters to extract. Defaults to \code{2}
#' @param features \code{character} vector denoting the names of time-series features to use in the clustering algorithm. Defaults to \code{NULL} for no feature filtering and usage of the entire feature matrix
#' @param na_removal \code{character} defining the way to deal with NAs produced during feature calculation. Can be one of \code{"feature"} or \code{"sample"}. \code{"feature"} removes all features that produced any NAs in any sample, keeping the number of samples the same. \code{"sample"} omits all samples that produced at least one NA. Defaults to \code{"feature"}
#' @param seed \code{integer} to fix R's random number generator to ensure reproducibility. Defaults to \code{123}
#' @param ... arguments to be passed to \code{stats::kmeans} or \code{stats::hclust}, or \code{mclust::Mclust} depending on selection in \code{clust_method}
#' @return object of class \code{feature_cluster} containing the clustering algorithm and a tidy version of clusters joined to the input dataset ready for further analysis
#' @author Trent Henderson
#' @export
#' @examples
#'
#' library(theft)
#'
#' features <- theft::calculate_features(theft::simData,
#'   feature_set = "catch22")
#'
#' clusts <- cluster(features,
#'   k = 6)
#'

cluster <- function(data, norm_method = c("zScore", "Sigmoid", "RobustSigmoid", "MinMax", "MaxAbs"), unit_int = FALSE,
                    clust_method = c("kmeans", "hclust", "mclust"), k = 2, features = NULL,
                    na_removal = c("feature","sample"), seed = 123, ...){

  stopifnot(inherits(data, "feature_calculations") == TRUE)
  norm_method <- match.arg(norm_method)
  clust_method <- match.arg(clust_method)
  na_removal <- match.arg(na_removal)

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
    tibble::column_to_rownames(var = "id")

  # Filter data

  if(na_removal == "feature"){
    wide_data <- wide_data %>%
      dplyr::select(where(~!any(is.na(.))))
  } else{
    wide_data <- wide_data %>%
      tidyr::drop_na()
  }

  # Report omitted features/samples

  n_features <- length(unique(normed$names))
  n_samples <- length(unique(normed$id))
  n_features_after <- ncol(wide_data)
  n_samples_after <- nrow(wide_data)
  n_features_omitted <- n_features - n_features_after
  n_samples_omitted <- n_samples - n_samples_after
  if (n_features_omitted > 0) {message(paste(n_features_omitted, "features omitted due to NAs", sep = " "))}
  if (n_samples_omitted > 0) {message(paste(n_samples_omitted, "samples omitted due to NAs", sep = " "))}
  set.seed(seed)

  if(clust_method == "kmeans"){

    # Fit k-means algorithm

    clusts <- stats::kmeans(wide_data, k, ...)

    # Extract results into a tidy format

    clust_info <- data.frame(id = rownames(wide_data), cluster = clusts$cluster)

    clust_tidy <- filtered %>%
      dplyr::inner_join(clust_info, by = c("id" = "id"))

    stopifnot(nrow(wide_data) == nrow(clust_info)) # Check we didn't lose any data

  } else if(clust_method == "hclust"){

    # Fit hierarchical clustering algorithm

    clusts <- stats::hclust(stats::dist(wide_data), ...)

    clust_info <- as.data.frame(stats::cutree(clusts, k = k)) %>%
      dplyr::rename(cluster = 1) %>%
      tibble::rownames_to_column(var = "id")

    # Extract results into a tidy format

    clust_tidy <- filtered %>%
      dplyr::inner_join(clust_info, by = c("id" = "id"))

    stopifnot(nrow(wide_data) == nrow(clust_info)) # Check we didn't lose any data

  } else{

    # Fit finite mixture model

    clusts <- mclust::Mclust(wide_data, G = k, ...)

    # Extract results into a tidy format

    clust_info <- data.frame(id = rownames(wide_data), cluster = clusts$classification)

    clust_tidy <- filtered %>%
      dplyr::inner_join(clust_info, by = c("id" = "id"))

    stopifnot(nrow(wide_data) == nrow(clust_info)) # Check we didn't lose any data
  }

  cluster_storage <- list(clust_tidy, clusts)
  names(cluster_storage) <- c("Data", "ModelFit")
  cluster_storage <- structure(cluster_storage, class = c("feature_clusters", "list"))
  return(cluster_storage)
}
