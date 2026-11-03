#' Remove duplicate features that exist in multiple feature sets and retain a reproducible random selection of one of them
#'
#' @importFrom stats na.omit
#' @import dplyr
#'
#' @param data \code{feature_calculations} object containing the raw feature matrix produced by \code{calculate_features}
#' @param preference deprecated. Do not use
#' @param seed \code{integer} denoting a fix for R's pseudo-random number generator to ensure selections are reproducible. Defaults to \code{123}
#' @return \code{feature_calculations} object containing filtered feature data
#' @author Trent Henderson
#' @export
#'

filter_duplicates <- function(data, preference = NULL, seed = 123){

  if(sum(c("catch22", "feasts", "tsfeatures", "Kats") %in% unique(data$feature_set)) < 2){
    message("Only one set of 'catch22', 'feasts', 'tsfeatures', or 'Kats' with potential duplicates is in your feature data. Exiting and returning original input data.")
    return(data)
  } else{

    # Set up dictionary of duplicates and their concordance of names

    dictionary <- data.frame(feasts_name = c("n_crossing_points", "diff1_acf1", "diff1_acf10", "diff2_acf1", "diff2_acf10", "spectral_entropy",
                                             "trend_strength", NA, NA, "coef_hurst", "stl_e_acf1", "stl_e_acf10", "acf1",
                                             "stat_arch_lm", "shift_kl_max", "shift_kl_index", "diff1_pacf5", "diff2_pacf5",
                                             "pacf5", "kpss_stat", NA),
                             tsfeatures_name = c("crossing_points", "diff1_acf1", "diff1_acf10", "diff2_acf1", "diff2_acf10", "entropy",
                                                 "trend", "firstmin_ac", "firstzero_ac", "hurst", "e_acf1", "e_acf10", "x_acf1",
                                                 "ARCH.LM", "max_kl_shift", "time_kl_shift", "diff1x_pacf5", "diff2x_pacf5",
                                                 "x_pacf5", "unitroot_kpss", "fluctanal_prop_r1"),
                             Kats_name = c("crossing_points", "diff1y_acf1", NA, "diff2y_acf1", NA, NA,
                                           NA, "firstmin_ac", "firstzero_ac", NA, NA, NA, "y_acf1",
                                           NA, NA, NA, NA, NA,
                                           NA, NA, NA),
                             catch22_name = c(NA, NA, NA, NA, NA, NA, NA, "CO_FirstMin_ac", NA, NA, NA, NA, NA, NA, NA,
                                              NA, NA, NA, NA, NA, "SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1"))

    rownames(dictionary) <- c("crossing_points", "diff1_acf1", "diff1_acf10", "diff2_acf1", "diff2_acf10", "entropy",
                              "trend", "firstmin_ac", "firstzero_ac", "hurst", "e_acf1", "e_acf10", "x_acf1",
                              "ARCH.LM", "shift_kl_max", "time_kl_shift", "diff1_pacf5", "diff2x_pacf5",
                              "x_pacf5", "kpss_stat", "SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1")

    # For each group of duplicates in the dictionary, find which (set, name) pairs are in the data.
    # If two or more are present, retain a random one of those present and drop the rest.
    # Every other feature is passed through untouched

    sets_present <- intersect(c("catch22", "feasts", "tsfeatures", "Kats"), unique(data$feature_set))

    set.seed(seed)
    to_drop <- list()

    for(i in 1:nrow(dictionary)){

      dup_names <- unlist(dictionary[i, paste0(sets_present, "_name")])
      names(dup_names) <- sets_present
      dup_names <- dup_names[!is.na(dup_names)]
      dup_names <- dup_names[mapply(function(set, name){any(data$feature_set == set & data$names == name)},
                                    names(dup_names), dup_names)]

      if(length(dup_names) > 1){
        keeper <- sample(seq_along(dup_names), size = 1)

        to_drop[[length(to_drop) + 1]] <- data.frame(feature_set = names(dup_names)[-keeper],
                                                     names = unname(dup_names[-keeper]))
      }
    }

    if(length(to_drop) > 0){
      to_drop <- do.call("rbind", to_drop)
      filtered_feats <- data %>%
        dplyr::anti_join(to_drop, by = c("feature_set" = "feature_set", "names" = "names"))
    } else{
      filtered_feats <- data
    }

    # Return final object

    filtered_feats <- structure(filtered_feats, class = c("feature_calculations", "data.frame"))
    return(filtered_feats)
  }
}
