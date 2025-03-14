#' Use a cross validated penalized maximum likelihood generalized linear model to perform feature selection
#'
#' @importFrom stats coef
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom dplyr %>% select mutate inner_join distinct filter
#' @importFrom tidyr pivot_wider
#' @importFrom glmnet cv.glmnet
#'
#' @param data \code{feature_calculations} object containing the raw feature matrix produced by \code{theft::calculate_features}
#' @param plot \code{Boolean} whether to draw the misclassification error lambda plot for a \code{cv.glmnet} object. Defaults to \code{FALSE}
#' @param ... arguments to be passed to \code{glmnet::cv.glmnet}
#' @return \code{feature_calculations} object containing a data frame of the reduced feature set
#' @author Trent Henderson
#' @export
#' @examples
#'
#' library(theft)
#'
#' features <- theft::calculate_features(theft::simData,
#'   group_var = "process",
#'   feature_set = "catch22")
#'
#' best_features <- shrink(features)
#'

shrink <- function(data, plot = FALSE, ...){

  stopifnot(inherits(data, "feature_calculations") == TRUE)
  '%ni%' <- Negate('%in%')

  if("group" %ni% colnames(data)){
    stop("data must include a group column to perform feature selection.")
  }

  #--------------- Fit model -------------

  # Check number of classes

  num_classes <- length(unique(data[, c("group")]))

  if(num_classes < 2){
    stop("Data must have >= 2 groups.")
  }

  if(num_classes == 2){
    the_family <- "binomial"
  }

  if(num_classes > 2){
    the_family <- "multinomial"
  }

  # Widen model matrix and pull out response (class) vector

  X <- data %>%
    dplyr::mutate(group = as.factor(as.character(.data$group)),
                  names = paste0(.data$feature_set, "_", .data$names)) %>%
    dplyr::select(c(.data$id, .data$group, .data$names, .data$values)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
    tibble::column_to_rownames(var = "id")

  y <- X %>%
    dplyr::select("group") %>%
    dplyr::pull("group")

  X <- X %>%
    dplyr::select(-c("group"))

  X <- as.matrix(X)
  y <- as.vector(y)

  # Fit model

  mod <- glmnet::cv.glmnet(X, y, family = the_family, type.measure = "class", ...)

  if(plot){
    plot(mod)
  }

  # Pull out coefficients

  coefs <- stats::coef(mod)

  #--------------- Filter to important features -------------

  if(the_family == "binomial"){

    # Find features that do not have a coefficient of 0

    coefs_df <- data.frame(names = rownames(coefs), values = coefs[, 1])
    rownames(coefs_df) <- NULL

    coefs_df <- coefs_df %>%
      dplyr::filter(names != "(Intercept)") %>%
      dplyr::filter(values != 0) %>%
      dplyr::mutate(feature_set = gsub("_.*", "\\1", names),
                    names = sub("^[^_]*_", "", names)) %>%
      dplyr::select(c(names, feature_set)) %>%
      dplyr::distinct()

  } else{

    # Find features which do not have a coefficient of 0 across all levels of the factor

    coefs_df <- vector(mode = "list", length = length(names(coefs)))

    for(i in 1:length(names(coefs))){
      coefs_1 <- data.frame(names = rownames(coefs[[1]]), values = coefs[[1]][, 1])
      rownames(coefs_1) <- NULL
      coefs_1$group <- names(coefs)[i]
      coefs_df[[i]] <- coefs_1
    }

    coefs_df <- do.call("rbind", coefs_df) %>%
      dplyr::filter(names != "(Intercept)") %>%
      tidyr::pivot_wider(id_cols = "names", names_from = "group", values_from = "values") %>%
      tibble::column_to_rownames(var = "names")

    coefs_df$sums <- rowSums(coefs_df)

    coefs_df <- coefs_df %>%
      dplyr::filter(sums != 0) %>%
      dplyr::select(-c(sums)) %>%
      tibble::rownames_to_column(var = "names") %>%
      dplyr::mutate(feature_set = gsub("_.*", "\\1", names),
                    names = sub("^[^_]*_", "", names)) %>%
      dplyr::select(c(names, feature_set)) %>%
      dplyr::distinct()
  }

  # Perform final filtering of initial feature data

  tmp_all_features <- data %>%
    dplyr::inner_join(coefs_df, by = c("names" = "names", "feature_set" = "feature_set"))

  return(tmp_all_features)
}
