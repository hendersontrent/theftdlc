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
#' @param low_dim_method \code{character} specifying the low dimensional embedding method to use. Defaults to \code{"PCA"}
#' @param seed \code{integer} to fix R's random number generator to ensure reproducibility. Defaults to \code{123}
#' @param ... arguments to be passed to \code{stats::prcomp} or \code{Rtsne::Rtsne}, \code{stats::cmdscale}, \code{MASS::isoMDS}, \code{MASS::sammon}, or \code{umap::umap} depending on selection in \code{low_dim_method}
#' @return object of class \code{low_dimension}
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
#' classifiers <- tsfeature_classifier(features,
#'   by_set = FALSE)
#'
#' pca <- reduce_dims(features,
#'   norm_method = "zScore",
#'   low_dim_method = "PCA")
#' }
#'

reduce_dims <- function(data, norm_method = c("zScore", "Sigmoid", "RobustSigmoid", "MinMax"), unit_int = FALSE,
                              low_dim_method = c("PCA", "tSNE", "ClassicalMDS", "KruskalMDS", "SammonMDS", "UMAP"),
                              seed = 123, ...){

  stopifnot(inherits(data, "feature_calculations") == TRUE)
  norm_method <- match.arg(norm_method)
  low_dim_method <- match.arg(low_dim_method)

  if(low_dim_method == "t-SNE"){
    low_dim_method <- "tSNE" # Old version from {theft}
  }

  if(norm_method == "z-score"){
    norm_method <- "zScore" # Old version from {theft}
  }

  #------------- Normalise data -------------------

  normed <- data[[1]] %>%
    dplyr::select(c(.data$id, .data$names, .data$values, .data$feature_set)) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$names) %>%
    dplyr::mutate(values = normaliseR::normalise(.data$values, norm_method = norm_method, unit_int = unit_int)) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na() %>%
    dplyr::mutate(names = paste0(.data$feature_set, "_", .data$names)) %>% # Catches errors when using all features across sets (i.e., there's duplicates)
    dplyr::select(-c(.data$feature_set))

  #------------- Perform low dim ----------------------

  # Produce matrix

  wide_data <- normed %>%
    tidyr::pivot_wider(id_cols = "id", names_from = "names", values_from = "values") %>%
    tibble::column_to_rownames(var = "id") %>%
    tidyr::drop_na()

  set.seed(seed)

  if(low_dim_method == "PCA"){

    fits <- wide_data %>%
      stats::prcomp(center = FALSE, scale. = FALSE, ...)

    # Retrieve eigenvalues and tidy up variance explained for plotting

    eigens <- fits %>%
      broom::tidy(matrix = "eigenvalues") %>%
      dplyr::filter(.data$PC %in% c(1, 2)) %>% # Filter to just the 2 going in the plot
      dplyr::select(c(.data$PC, .data$percent)) %>%
      dplyr::mutate(percent = round(.data$percent * 100), digits = 1)

    eigen_pc1 <- eigens %>%
      dplyr::filter(.data$PC == 1)

    eigen_pc2 <- eigens %>%
      dplyr::filter(.data$PC == 2)

    eigen_pc1 <- paste0(eigen_pc1$percent,"%")
    eigen_pc2 <- paste0(eigen_pc2$percent,"%")

  } else if(low_dim_method == "tSNE") {

    # t-SNE calculation

    fits <- Rtsne::Rtsne(as.matrix(wide_data), dims = 2,
                         check_duplicates = FALSE, ...)

    # Retrieve 2-dimensional embedding and add in unique IDs

    id_ref <- wide_data %>%
      tibble::rownames_to_column(var = "id") %>%
      dplyr::select(c(.data$id))

    fits <- data.frame(.fitted1 = fits$Y[,1],
                       .fitted2 = fits$Y[,2]) %>%
      dplyr::mutate(id = id_ref$id)

  } else if(low_dim_method == "ClassicalMDS"){

    fits <- wide_data %>%
      stats::dist() %>%
      stats::cmdscale(k = 2, ...) %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "id") %>%
      dplyr::rename(.fitted1 = 2,
                    .fitted2 = 3)

  } else if(low_dim_method == "KruskalMDS"){

    fits <- wide_data %>%
      stats::dist() %>%
      MASS::isoMDS(k = 2, ...)

    fits <- fits$points %>%
      tibble::as_tibble()

    id_ref <- wide_data %>%
      tibble::rownames_to_column(var = "id") %>%
      dplyr::select(c(.data$id))

    fits$id <- id_ref$id

  } else if(low_dim_method == "SammonMDS"){

    fits <- wide_data %>%
      stats::dist() %>%
      MASS::sammon(k = 2, ...)

    fits <- fits$points %>%
      tibble::as_tibble()

    id_ref <- wide_data %>%
      tibble::rownames_to_column(var = "id") %>%
      dplyr::select(c(.data$id))

    fits$id <- id_ref$id

  } else{

    fits <- umap::umap(wide_data, n_components = 2, ...)$layout %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "id") %>%
      dplyr::rename(.fitted1 = 2,
                    .fitted2 = 3)
  }

  low_dim <- list(data[[1]], wide_data, fits)
  low_dim <- structure(low_dim, class = "low_dimension")
  return(low_dim)
}
