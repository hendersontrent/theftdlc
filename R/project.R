#' Project a feature matrix into a two-dimensional representation using PCA, MDS, t-SNE, or UMAP ready for plotting
#'
#' @importFrom stats cmdscale prcomp dist
#' @importFrom rlang .data
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom dplyr %>% select group_by mutate ungroup filter
#' @importFrom tidyr drop_na pivot_wider
#' @importFrom normaliseR normalise
#' @importFrom broom augment tidy
#' @importFrom Rtsne Rtsne
#' @importFrom MASS isoMDS sammon
#' @importFrom umap umap
#' @param data \code{feature_calculations} object containing the raw feature matrix produced by \code{theft::calculate_features}
#' @param norm_method \code{character} denoting the rescaling/normalising method to apply. Can be one of \code{"zScore"}, \code{"Sigmoid"}, \code{"RobustSigmoid"}, \code{"MinMax"}, or \code{"MaxAbs"}. Defaults to \code{"zScore"}
#' @param unit_int \code{Boolean} whether to rescale into unit interval \code{[0,1]} after applying normalisation method. Defaults to \code{FALSE}
#' @param low_dim_method \code{character} specifying the low dimensional embedding method to use. Can be one of \code{"PCA"}, \code{"tSNE"}, \code{"ClassicalMDS"}, \code{"KruskalMDS"}, \code{"SammonMDS"}, or \code{"UMAP"}. Defaults to \code{"PCA"}
#' @param na_removal \code{character} defining the way to deal with NAs produced during feature calculation. Can be one of \code{"feature"} or \code{"sample"}. \code{"feature"} removes all features that produced any NAs in any sample, keeping the number of samples the same. \code{"sample"} omits all samples that produced at least one NA. Defaults to \code{"feature"}
#' @param seed \code{integer} to fix R's random number generator to ensure reproducibility. Defaults to \code{123}
#' @param ... arguments to be passed to \code{stats::prcomp} or \code{Rtsne::Rtsne}, \code{stats::cmdscale}, \code{MASS::isoMDS}, \code{MASS::sammon}, or \code{umap::umap} depending on selection in \code{low_dim_method}
#' @return object of class \code{feature_project} which is a named list containing the \code{feature_calculations} data supplied to the function, the wide matrix of filtered data, a tidy \code{data.frame} of the projected 2-D data, and the model fit object
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
#' pca <- project(features,
#'   norm_method = "zScore",
#'   low_dim_method = "PCA")
#' }
#'

project <- function(data, norm_method = c("zScore", "Sigmoid", "RobustSigmoid", "MinMax"), unit_int = FALSE,
                    low_dim_method = c("PCA", "tSNE", "ClassicalMDS", "KruskalMDS", "SammonMDS", "UMAP"),
                    na_removal = c("feature","sample"), seed = 123, ...){

  stopifnot(inherits(data, "feature_calculations") == TRUE)
  norm_method <- match.arg(norm_method)
  low_dim_method <- match.arg(low_dim_method)
  na_removal <- match.arg(na_removal)

  if(low_dim_method == "t-SNE"){
    low_dim_method <- "tSNE" # Old version from {theft}
  }

  if(norm_method == "z-score"){
    norm_method <- "zScore" # Old version from {theft}
  }

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

  #------------------- Perform dimension reduction ----------------------

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

  if(low_dim_method == "PCA"){

    fits <- wide_data %>%
      stats::prcomp(center = FALSE, scale. = FALSE, ...)

    projected <- fits

  } else if(low_dim_method == "tSNE") {

    # t-SNE calculation

    fits <- Rtsne::Rtsne(as.matrix(wide_data), dims = 2,
                         check_duplicates = FALSE, ...)

    # Retrieve 2-dimensional embedding and add in unique IDs

    id_ref <- wide_data %>%
      tibble::rownames_to_column(var = "id") %>%
      dplyr::select(c(.data$id))

    projected <- data.frame(.fitted1 = fits$Y[,1],
                            .fitted2 = fits$Y[,2]) %>%
      dplyr::mutate(id = id_ref$id)

  } else if(low_dim_method == "ClassicalMDS"){

    fits <- wide_data %>%
      stats::dist() %>%
      stats::cmdscale(k = 2, ...)

    projected <- fits %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "id") %>%
      dplyr::rename(.fitted1 = 2,
                    .fitted2 = 3)

  } else if(low_dim_method == "KruskalMDS"){

    fits <- wide_data %>%
      stats::dist() %>%
      MASS::isoMDS(k = 2, ...)

    projected <- fits$points %>%
      tibble::as_tibble()

    id_ref <- wide_data %>%
      tibble::rownames_to_column(var = "id") %>%
      dplyr::select(c(.data$id))

    fits$id <- id_ref$id

  } else if(low_dim_method == "SammonMDS"){

    fits <- wide_data %>%
      stats::dist() %>%
      MASS::sammon(k = 2, ...)

    projected <- fits$points %>%
      tibble::as_tibble()

    id_ref <- wide_data %>%
      tibble::rownames_to_column(var = "id") %>%
      dplyr::select(c(.data$id))

    fits$id <- id_ref$id

  } else{

    fits <- umap::umap(wide_data, n_components = 2, ...)

    projected <- fits$layout %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "id") %>%
      dplyr::rename(.fitted1 = 2,
                    .fitted2 = 3)
  }

  low_dim <- list(data, wide_data, projected, fits)
  names(low_dim) <- c("Data", "ModelData", "ProjectedData", "ModelFit")
  low_dim <- structure(low_dim, class = "feature_projection")
  return(low_dim)
}

# Previous version

#' @rdname project
#' @export
reduce_dims <- project
