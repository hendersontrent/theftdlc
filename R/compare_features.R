#' Conduct statistical testing on time-series feature classification performance to identify top features or compare entire sets
#'
#' @importFrom stats p.adjust
#' @import dplyr
#' @importFrom tidyr crossing
#' @importFrom purrr map_dfr
#' @importFrom future plan multisession
#' @importFrom furrr future_map_dfr
#' @param data \code{list} object containing the classification outputs produce by \code{tsfeature_classifier}
#' @param metric \code{character} denoting the classification performance metric to use in statistical testing. Can be one of \code{"accuracy"}, \code{"precision"}, \code{"recall"}, \code{"f1"}. Defaults to \code{"accuracy"}
#' @param by_set \code{Boolean} specifying whether you want to compare feature sets (if \code{TRUE}) or individual features (if \code{FALSE}). Defaults to \code{TRUE} but this is contingent on whether you computed by set or not in \code{tsfeature_classifier}
#' @param hypothesis \code{character} denoting whether p-values should be calculated for each feature set or feature (depending on \code{by_set} argument) individually relative to the null if \code{use_null = TRUE} in \code{tsfeature_classifier} through \code{"null"}, or whether pairwise comparisons between each set or feature should be conducted on main model fits only through \code{"pairwise"}. Defaults to \code{"null"}
#' @param p_adj \code{character} denoting the adjustment made to p-values for multiple comparisons. Should be a valid argument to \code{stats::p.adjust}. Defaults to \code{"none"} for no adjustment. \code{"holm"} is recommended as a starting point for adjustments
#' @param n_workers \code{integer} denoting the number of parallel processes to use. Defaults to \code{1} for serial processing
#' @references Henderson, T., Bryant, A. G., and Fulcher, B. D. Never a Dull Moment: Distributional Properties as a Baseline for Time-Series Classification. 27th Pacific-Asia Conference on Knowledge Discovery and Data Mining, (2023).
#' @return \code{data.frame} containing the results
#' @author Trent Henderson
#' @export
#' @examples
#'
#' library(theft)
#'
#' features <- theft::calculate_features(theft::simData,
#'   feature_set = NULL,
#'   features = list("mean" = mean, "sd" = sd))
#'
#' classifiers <- classify(features,
#'                         by_set = FALSE,
#'                         n_resamples = 3)
#'
#' compare_features(classifiers,
#'                  by_set = FALSE,
#'                  hypothesis = "pairwise")
#'

compare_features <- function(data, metric = c("accuracy", "precision", "recall", "f1"),
                             by_set = TRUE, hypothesis = c("null", "pairwise"),
                             p_adj = c("none", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr"),
                             n_workers = 1){

  p_adj <- match.arg(p_adj)
  '%ni%' <- Negate('%in%')

  # Check metric

  metric <- match.arg(metric)

  if(metric %ni% c("accuracy", "precision", "recall", "f1")){
    stop("metric must be one of: 'accuracy', 'precision', 'recall' or 'f1'.")
  }

  # Check hypothesis

  hypothesis <- match.arg(hypothesis)

  if(hypothesis %ni% c("null", "pairwise")){
    stop("hypothesis must be one of: 'null' or 'pairwise'. If 'null', input data requires null estimates which can be obtained through setting 'use_null = TRUE' in tsfeature_classifier")
  }

  # Check workers

  stopifnot(n_workers >= 1)

  if(n_workers > 1){
    is_parallel <- TRUE
  } else{
    is_parallel <- FALSE
  }

  #--------------- Statistical testing ----------------

  if(by_set){

    if(hypothesis == "null"){

      # Develop iterator

      iters <- data.frame(feature_set = unique(data$ClassificationResults$feature_set))

      # Produce final output

      if(is_parallel){

        future::plan(future::multisession, workers = n_workers)

        results <- 1:nrow(iters) %>%
          furrr::future_map_dfr(~ stat_test(data$ClassificationResults, iters, .x, by_set = by_set,
                                            hypothesis = hypothesis, metric = metric, train_test_sizes = data$TrainTestSizes,
                                            n_resamples = max(data$ClassificationResults$resample)))
      } else{
        results <- 1:nrow(iters) %>%
          purrr::map_dfr(~ stat_test(data$ClassificationResults, iters, .x, by_set = by_set,
                                     hypothesis = hypothesis, metric = metric, train_test_sizes = data$TrainTestSizes,
                                     n_resamples = max(data$ClassificationResults$resample)))
      }

    } else{

      # Develop iterator

      iters <- tidyr::crossing(unique(data$ClassificationResults$feature_set), unique(data$ClassificationResults$feature_set)) %>%
        dplyr::rename(method_a = 1, method_b = 2) %>%
        dplyr::filter(.data$method_a != .data$method_b)

      iters$sorted_combination <- apply(iters, 1, function(row) paste(sort(row), collapse = "-"))

      iters <- iters %>%
        dplyr::distinct(.data$sorted_combination, .keep_all = TRUE) %>%
        dplyr::select(-c(.data$sorted_combination))

      # Produce final output

      if(is_parallel){

        future::plan(future::multisession, workers = n_workers)

        results <- 1:nrow(iters) %>%
          furrr::future_map_dfr(~ stat_test(data$ClassificationResults, iters, .x, by_set = by_set,
                                            hypothesis = hypothesis, metric = metric, train_test_sizes = data$TrainTestSizes,
                                            n_resamples = max(data$ClassificationResults$resample)))
      } else{
        results <- 1:nrow(iters) %>%
          purrr::map_dfr(~ stat_test(data$ClassificationResults, iters, .x, by_set = by_set,
                                     hypothesis = hypothesis, metric = metric, train_test_sizes = data$TrainTestSizes,
                                     n_resamples = max(data$ClassificationResults$resample)))
      }
    }

  } else{

    if(hypothesis == "null"){

      # Develop iterator

      iters <- data.frame(names = unique(data$ClassificationResults$names))

      # Produce final output

      if(is_parallel){

        future::plan(future::multisession, workers = n_workers)

        results <- 1:nrow(iters) %>%
          furrr::future_map_dfr(~ stat_test(data$ClassificationResults, iters, .x, by_set = by_set,
                                           hypothesis = hypothesis, metric = metric, train_test_sizes = data$TrainTestSizes,
                                           n_resamples = max(data$ClassificationResults$resample)))

      } else{
        results <- 1:nrow(iters) %>%
          purrr::map_dfr(~ stat_test(data$ClassificationResults, iters, .x, by_set = by_set,
                                     hypothesis = hypothesis, metric = metric, train_test_sizes = data$TrainTestSizes,
                                     n_resamples = max(data$ClassificationResults$resample)))
      }

    } else{

      # Develop iterator

      iters <- tidyr::crossing(unique(data$ClassificationResults$names), unique(data$ClassificationResults$names)) %>%
        dplyr::rename(names_a = 1, names_b = 2) %>%
        dplyr::filter(.data$names_a != .data$names_b)

      iters$sorted_combination <- apply(iters, 1, function(row) paste(sort(row), collapse = "-"))

      iters <- iters %>%
        dplyr::distinct(.data$sorted_combination, .keep_all = TRUE) %>%
        dplyr::select(-c(.data$sorted_combination))

      # Produce final output

      if(is_parallel){

        future::plan(future::multisession, workers = n_workers)

        results <- 1:nrow(iters) %>%
          furrr::future_map_dfr(~ stat_test(data$ClassificationResults, iters, .x, by_set = by_set,
                                             hypothesis = hypothesis, metric = metric, train_test_sizes = data$TrainTestSizes,
                                             n_resamples = max(data$ClassificationResults$resample)))
      } else{
        results <- 1:nrow(iters) %>%
          purrr::map_dfr(~ stat_test(data$ClassificationResults, iters, .x, by_set = by_set,
                                     hypothesis = hypothesis, metric = metric, train_test_sizes = data$TrainTestSizes,
                                     n_resamples = max(data$ClassificationResults$resample)))
      }
    }
  }

  if(p_adj != "none"){
    results$p_value_adj <- stats::p.adjust(results$p.value, method = p_adj)
  }

  return(results)
}
