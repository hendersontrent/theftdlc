#' Helper function for converting to title case
#'
#' @param x \code{character} vector
#' @return \code{character} vector
#' @author Trent Henderson
#'

make_title <- function(x){
  s <- strsplit(x, " ")[[1]]
  s <- paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
  return(s)
}

#' Calculate interval summaries with a measure of central tendency of classification results
#'
#' @importFrom stats sd quantile median qt
#' @importFrom rlang sym .data
#' @import dplyr
#' @param data \code{list} object containing the classification outputs produce by \code{tsfeature_classifier}
#' @param metric \code{character} denoting the classification performance metric to calculate intervals for. Can be one of \code{"accuracy"}, \code{"precision"}, \code{"recall"}, \code{"f1"}. Defaults to \code{"accuracy"}
#' @param by_set \code{Boolean} specifying whether to compute intervals for each feature set. Defaults to \code{TRUE}. If \code{FALSE}, the function will instead calculate intervals for each feature
#' @param type \code{character} denoting whether to calculate a +/- SD interval with \code{"sd"}, confidence interval based off the t-distribution with \code{"se"}, or based on a quantile with \code{"quantile"}. Defaults to \code{"sd"}
#' @param interval \code{numeric} scalar denoting the width of the interval to calculate. Defaults to \code{1} if \code{type = "sd"} to produce a +/- 1 SD interval. Defaults to \code{0.95} if \code{type = "se"} or \code{type = "quantile"} for a 95 per cent interval
#' @param model_type \code{character} denoting whether to calculate intervals for main models with \code{"main"} or null models with \code{"null"} if the \code{use_null} argument when using \code{tsfeature_classifier} was \code{use_null = TRUE}. Defaults to \code{"main"}
#' @return \code{interval_calculations} object which is a data frame containing the results
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
#'   by_set = FALSE,
#'   n_resamples = 3)
#'
#' interval(classifiers,
#'   by_set = FALSE,
#'   type = "sd",
#'   interval = 1)
#'

interval <- function(data, metric = c("accuracy", "precision", "recall", "f1"),
                     by_set = TRUE, type = c("sd", "se", "quantile"),
                     interval = NULL, model_type = c("main", "null")){

  type <- match.arg(type)
  metric <- match.arg(metric)
  model_type <- match.arg(model_type)
  '%ni%' <- Negate('%in%')

  if(type == "qt"){
    type <- "se" # Catches old argument specification for t-distribution
  }

  # Check args

  if(type %ni% c("sd", "se", "quantile")){
    stop("type must be one of 'sd', 'se', or 'quantile'.")
  }

  if(metric %ni% c("accuracy", "precision", "recall", "f1")){
    stop("metric must be one of: 'accuracy', 'precision', 'recall' or 'f1'.")
  }

  # Set defaults

  if(is.null(interval)){
    if(type == "sd"){
      interval <- 1
    } else{
      interval <- 0.95
    }
  }

  # Check valid widths

  if(interval <= 0){
    stop("interval must be > 0. 1 is recommended to start with if type = 'sd' or 0.95 for type = 'quantile'.")
  }

  if(type %in% c("se", "quantile") && interval >= 1){
    stop("for type 'se' or 'quantile', interval must be 0 < interval < 1. For example, interval = 0.95 produces a 95% CI.")
  }

  # Set up strings to make code easier

  if(metric == "accuracy"){
    mymetric <- "accuracy"
  } else if(metric == "precision"){
    mymetric <- "mean_precision"
  } else if(metric == "recall"){
    mymetric <- "mean_recall"
  } else{
    mymetric <- "mean_f1_score"
  }

  if(by_set){
    grouper <- "feature_set"
  } else{
    grouper <- "names"
  }

  model_type_str <- tolower(model_type)
  model_type_str <- make_title(model_type_str)

  # Produce dataframe to be summarised

  intervals <- data$ClassificationResults %>%
    dplyr::select(c(dplyr::all_of(grouper), "model_type", dplyr::all_of(mymetric))) %>%
    dplyr::rename("values" = 3)

  # Calculate intervals

  if(type == "sd"){

    intervals <- intervals %>%
      dplyr::filter(model_type == model_type_str) %>%
      dplyr::reframe(.mean = mean(.data$values),
                       .lower = .data$.mean - (interval * stats::sd(.data$values)),
                       .upper = .data$.mean + (interval * stats::sd(.data$values)),
                     .by = !!rlang::sym(grouper))

  } else if(type == "se"){

    n_samps <- max(data$ClassificationResults$resample)

    intervals <- intervals %>%
      dplyr::filter(model_type == model_type_str) %>%
      dplyr::reframe(.mean = mean(.data$values),
                     interval_width = stats::qt(interval, (n_samps - 1)) * stats::sd(.data$values) / sqrt(n_samps),
                     .lower = .data$.mean - .data$interval_width,
                     .upper = .data$.mean + .data$interval_width,
                     .by = !!rlang::sym(grouper)) %>%
      dplyr::select(-c(.data$interval_width))

  } else{

    # Find bounds

    upper_bound <- (interval + 1) / 2
    lower_bound <- upper_bound - interval

    # Calculate results

    intervals <- intervals %>%
      dplyr::filter(model_type == model_type_str) %>%
      dplyr::reframe(.median = stats::median(.data$values),
                     .lower = stats::quantile(.data$values, prob = lower_bound),
                     .upper = stats::quantile(.data$values, prob = upper_bound),
                     .by = !!rlang::sym(grouper))
  }

  intervals <- structure(intervals, class = c("interval_calculations", "data.frame"))
  return(intervals)
}

# Previous version

#' @rdname interval
#' @export
calculate_interval <- interval
