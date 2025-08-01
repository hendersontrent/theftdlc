#------------------------- feature_calculations object methods -------------------------

#' Produce a plot for a feature_calculations object
#'
#' @importFrom rlang .data
#' @import dplyr
#' @import ggplot2
#' @import tibble
#' @importFrom tidyr pivot_wider pivot_longer drop_na
#' @importFrom reshape2 melt
#' @importFrom stats hclust dist cor
#' @importFrom normaliseR normalise
#' @param x \code{feature_calculations} object containing the raw feature matrix produced by \code{theft::calculate_features}
#' @param type \code{character} specifying the type of plot to draw. Can be one of \code{"matrix"}, \code{"cor"}, \code{"violin"}, \code{"box"}, or \code{"quality"}. Defaults to \code{"matrix"}
#' @param norm_method \code{character} specifying a rescaling/normalising method to apply if \code{type = "matrix"} or if \code{type = "cor"}. Can be one of \code{"zScore"}, \code{"Sigmoid"}, \code{"RobustSigmoid"}, \code{"MinMax"}, or \code{"MaxAbs"}. Defaults to \code{"zScore"}
#' @param unit_int \code{Boolean} whether to rescale into unit interval \code{[0,1]} after applying normalisation method. Defaults to \code{FALSE}
#' @param clust_method \code{character} specifying the hierarchical clustering method to use if \code{type = "matrix"} or if \code{type = "cor"}. Defaults to \code{"average"}
#' @param cor_method \code{character} specifying the correlation method to use if \code{type = "cor"}. Defaults to \code{"pearson"}
#' @param feature_names \code{character} vector denoting the name of the features to plot if \code{type = "violin"}. Defaults to \code{NULL}
#' @param ... Arguments to be passed to \code{ggplot2::geom_bar} if \code{type = "quality"}, \code{ggplot2::geom_raster} if \code{type = "matrix"}, \code{ggplot2::geom_raster} if \code{type = "cor"}, or \code{ggplot2::geom_point} if \code{type = "violin"}
#' @return object of class \code{ggplot} that contains the graphic
#' @author Trent Henderson
#' @export
#'

plot.feature_calculations <- function(x, type = c("matrix", "cor", "violin", "box", "quality"),
                                      norm_method = c("zScore", "Sigmoid", "RobustSigmoid", "MinMax", "MaxAbs"),
                                      unit_int = FALSE,
                                      clust_method = c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty", "median", "centroid"),
                                      cor_method = c("pearson", "spearman"), feature_names = NULL, ...){

  stopifnot(inherits(x, "feature_calculations") == TRUE)
  type <- match.arg(type)
  norm_method <- match.arg(norm_method)
  clust_method <- match.arg(clust_method)
  cor_method <- match.arg(cor_method)

  if(type == "quality"){

    #--------------- Calculate proportions ------------

    tmp <- x %>%
      dplyr::mutate(quality = dplyr::case_when(
        is.na(.data$values)                               ~ "NaN",
        is.nan(.data$values)                              ~ "NaN",
        is.infinite(.data$values)                         ~ "-Inf or Inf",
        is.numeric(.data$values) & !is.na(.data$values) &
          !is.na(.data$values) & !is.nan(.data$values)    ~ "Good")) %>%
      dplyr::reframe(counter = dplyr::n(), .by = c(.data$names, .data$quality)) %>%
      dplyr::group_by(.data$names) %>%
      dplyr::mutate(props = .data$counter / sum(.data$counter)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(quality = factor(.data$quality, levels = c("-Inf or Inf", "NaN", "Good")))

    # Calculate order of 'good' quality feature vectors to visually steer plot

    ordering <- tmp %>%
      dplyr::filter(.data$quality == "Good") %>%
      dplyr::mutate(ranker = dplyr::dense_rank(.data$props)) %>%
      dplyr::select(c(.data$names, .data$ranker))

    # Join back in

    tmp <- tmp %>%
      dplyr::left_join(ordering, by = c("names" = "names"))

    #--------------- Draw plot ------------------------

    # Define a nice colour palette consistent with RColorBrewer in other functions

    my_palette <- c("-Inf or Inf" = "#7570B3",
                    "NaN" = "#D95F02",
                    "Good" = "#1B9E77")

    # Plot

    p <- tmp %>%
      ggplot2::ggplot(ggplot2::aes(x = stats::reorder(.data$names, -.data$ranker), y = .data$props)) +
      ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = .data$quality), ...) +
      ggplot2::labs(title = "Data quality for computed features",
                    x = "Feature",
                    y = "Proportion of Outputs",
                    fill = "Data Type") +
      ggplot2::scale_y_continuous(limits = c(0,1),
                                  breaks = seq(from = 0, to = 1, by = 0.1)) +
      ggplot2::scale_fill_manual(values = my_palette) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     legend.position = "bottom")

    if(length(unique(tmp$names)) > 22){
      p <- p +
        ggplot2::theme(axis.text.x = ggplot2::element_blank())
    } else{
      p <- p +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }
  } else if(type == "matrix"){

    #------------- Apply normalisation -------------

    data_id <- x %>%
      dplyr::select(c(.data$id, .data$names, .data$values, .data$feature_set)) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(names = paste0(.data$feature_set, "_", .data$names)) %>% # Catches errors when using all features across sets (i.e., there's duplicates)
      dplyr::select(-c(.data$feature_set)) %>%
      dplyr::group_by(.data$names) %>%
      dplyr::mutate(values = normaliseR::normalise(.data$values, norm_method = norm_method, unit_int = unit_int)) %>%
      dplyr::ungroup()

    #------------- Hierarchical clustering ----------

    dat <- data_id %>%
      tidyr::pivot_wider(id_cols = "id", names_from = "names", values_from = "values") %>%
      tibble::column_to_rownames(var = "id")

    # Remove any columns with >50% NAs to prevent masses of rows getting dropped due to poor features

    dat_filtered <- dat[, which(colMeans(!is.na(dat)) > 0.5)]

    # Drop any remaining rows with NAs

    dat_filtered <- dat_filtered %>%
      tidyr::drop_na()

    row.order <- stats::hclust(stats::dist(dat_filtered, method = "euclidean"), method = clust_method)$order # Hierarchical cluster on rows
    col.order <- stats::hclust(stats::dist(t(dat_filtered), method = "euclidean"), method = clust_method)$order # Hierarchical cluster on columns
    dat_new <- dat_filtered[row.order, col.order] # Re-order matrix by cluster outputs

    cluster_out <- reshape2::melt(as.matrix(dat_new)) %>% # Turn into dataframe
      dplyr::rename(id = .data$Var1,
                    names = .data$Var2)

    #------------- Draw graphic ---------------------

    # Define a nice colour palette consistent with RColorBrewer in other functions

    mypalette <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")

    p <- cluster_out %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$names, y = .data$id, fill = .data$value))  +
      ggplot2::geom_raster(...) +
      ggplot2::scale_fill_stepsn(n.breaks = 6, colours = rev(mypalette),
                                 show.limits = TRUE) +
      ggplot2::labs(title = "Data matrix",
                    x = "Feature",
                    y = "Time series") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(fill = "Scaled value")

    if(length(unique(cluster_out$names)) <= 22){
      p <- p +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
                       axis.ticks.y = ggplot2::element_blank())
    } else {
      p <- p +
        ggplot2::theme(axis.text = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank())
    }

    p <- p +
      ggplot2::theme(legend.position = "bottom")

  } else if(type == "cor") {

    #------------- Clean up structure --------------

    data_id <- x %>%
      dplyr::select(c(.data$id, .data$names, .data$values, .data$feature_set)) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(names = paste0(.data$feature_set, "_", .data$names)) %>% # Catches errors when using all features across sets (i.e., there's duplicates)
      dplyr::select(-c(.data$feature_set))

    #------------- Data reshaping -------------------

    features <- unique(data_id$names)

    ids_to_keep <- data_id %>%
      dplyr::group_by(.data$id) %>%
      dplyr::summarise(counter = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$counter == length(features))

    ids_to_keep <- ids_to_keep$id

    cor_dat <- data_id %>%
      dplyr::filter(.data$id %in% ids_to_keep) %>%
      tidyr::pivot_wider(id_cols = "id", names_from = "names", values_from = "values") %>%
      dplyr::select(-c(.data$id))

    #--------- Correlation ----------

    # Calculate correlations and take absolute

    result <- abs(stats::cor(cor_dat, method = cor_method))

    #--------- Clustering -----------

    # Wrangle into tidy format

    melted <- reshape2::melt(result)

    # Perform clustering

    row.order <- stats::hclust(stats::dist(result, method = "euclidean"), method = clust_method)$order # Hierarchical cluster on rows
    col.order <- stats::hclust(stats::dist(t(result), method = "euclidean"), method = clust_method)$order # Hierarchical cluster on columns
    dat_new <- result[row.order, col.order] # Re-order matrix by cluster outputs
    cluster_out <- reshape2::melt(as.matrix(dat_new)) # Turn into dataframe

    #--------- Graphic --------------

    # Define a nice colour palette consistent with RColorBrewer in other functions

    mypalette <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")

    p <- cluster_out %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$Var1, y = .data$Var2)) +
      ggplot2::geom_raster(ggplot2::aes(fill = .data$value), ...) +
      ggplot2::labs(title = "Pairwise correlation matrix",
                    x = "Feature",
                    y = "Feature",
                    fill = "Absolute correlation coefficient") +
      ggplot2::scale_fill_stepsn(n.breaks = 6, colours = rev(mypalette),
                                 show.limits = TRUE) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     legend.position = "bottom")

    if(ncol(cor_dat) <= 20){
      p <- p +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    } else {
      p <- p +
        ggplot2::theme(axis.text = ggplot2::element_blank())
    }
  } else if(type == "violin"){

    if(is.null(feature_names)){
      stop("feature_names argument must not be NULL if drawing a violin plot. Please enter a vector of valid feature names.")
    } else{
      p <- x %>%
        dplyr::filter(names %in% feature_names) %>%
        dplyr::mutate(names = paste0(.data$feature_set, "_", .data$names))

      if("group" %in% colnames(p)){
        p <- p %>%
          ggplot2::ggplot(ggplot2::aes(x = .data$group, y = .data$values, colour = .data$group))
      } else{
        p <- p %>%
          dplyr::mutate(group = "Data") %>%
          ggplot2::ggplot(ggplot2::aes(x = .data$group, y = .data$values))
      }

      p <- p +
        ggplot2::geom_violin() +
        ggplot2::geom_point(position = ggplot2::position_jitter(w = 0.05), ...) +
        ggplot2::labs(x = "Class",
                      y = "Feature value") +
        ggplot2::scale_colour_brewer(palette = "Dark2") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none",
                       axis.text.x = ggplot2::element_text(angle = 90)) +
        ggplot2::facet_wrap(~ names, scales = "free_y")
    }
  }

  else{

    if(is.null(feature_names)){
      stop("feature_names argument must not be NULL if drawing a boxplot Please enter a vector of valid feature names.")
    } else{
      p <- x %>%
        dplyr::filter(names %in% feature_names) %>%
        dplyr::mutate(names = paste0(.data$feature_set, "_", .data$names))

      if("group" %in% colnames(p)){
        p <- p %>%
          ggplot2::ggplot(ggplot2::aes(x = .data$group, y = .data$values, colour = .data$group))
      } else{
        p <- p %>%
          dplyr::mutate(group = "Data") %>%
          ggplot2::ggplot(ggplot2::aes(x = .data$group, y = .data$values))
      }

      p <- p +
        ggplot2::geom_boxplot(...) +
        ggplot2::labs(x = "Class",
                      y = "Feature value") +
        ggplot2::scale_colour_brewer(palette = "Dark2") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none",
                       axis.text.x = ggplot2::element_text(angle = 90)) +
        ggplot2::facet_wrap(~ names, scales = "free_y")
    }
  }

  return(p)
}

#------------------------- low_dimension object methods ------------------------

#' Produce a plot for a feature_projection object
#'
#' @importFrom rlang .data
#' @import dplyr
#' @import ggplot2
#' @import tibble
#' @importFrom tidyr drop_na
#' @importFrom broom augment tidy
#' @param x \code{feature_projection} object containing the two-dimensional embedding calculated by \code{project}
#' @param show_covariance \code{Boolean} specifying whether covariance ellipses should be shown on the plot. Defaults to \code{TRUE}
#' @param ... Arguments to be passed to methods
#' @return object of class \code{ggplot} that contains the graphic
#' @author Trent Henderson
#' @export
#'

plot.feature_projection <- function(x, show_covariance = TRUE, ...){

  stopifnot(inherits(x, "feature_projection") == TRUE)

  if(inherits(x[[4]], "prcomp") == TRUE){

    # Retrieve eigenvalues and tidy up variance explained for plotting

    eigens <- x[[4]] %>%
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

    fits <- x[[3]] %>%
      broom::augment(x[[2]]) %>%
      dplyr::rename(id = 1) %>%
      dplyr::mutate(id = as.factor(.data$id)) %>%
      dplyr::rename(.fitted1 = .data$.fittedPC1,
                    .fitted2 = .data$.fittedPC2)

    if("group" %in% colnames(x[[1]])){

      data_id <- as.data.frame(lapply(x[[1]], unlist)) # Catch weird cases where it's a list...

      groups <- data_id %>%
        dplyr::rename(group_id = .data$group) %>%
        dplyr::reframe(counter = dplyr::n(),
                       .by = c(.data$id, .data$group_id)) %>%
        dplyr::select(-c(.data$counter)) %>%
        dplyr::mutate(id = as.factor(.data$id))

      fits <- fits %>%
        dplyr::inner_join(groups, by = c("id" = "id"))

      # Draw plot

      p <- fits %>%
        dplyr::mutate(group_id = as.factor(.data$group_id)) %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$.fitted1, y = .data$.fitted2))

      if(show_covariance){
        p <- p +
          ggplot2::stat_ellipse(ggplot2::aes(x = .data$.fitted1, y = .data$.fitted2, fill = .data$group_id), geom = "polygon", alpha = 0.2) +
          ggplot2::guides(fill = "none") +
          ggplot2::scale_fill_brewer(palette = "Dark2")
      }

      if(nrow(fits) > 200){
        p <- p +
          ggplot2::geom_point(size = 1.5, ggplot2::aes(colour = .data$group_id))
      } else{
        p <- p +
          ggplot2::geom_point(size = 2.25, ggplot2::aes(colour = .data$group_id))
      }

      p <- p +
        ggplot2::labs(title = "Low dimensional projection of time series",
                      x = paste0("PC 1"," (", eigen_pc1, ")"),
                      y = paste0("PC 2"," (", eigen_pc2, ")"),
                      colour = NULL) +
        ggplot2::scale_colour_brewer(palette = "Dark2") +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       legend.position = "bottom")
    } else{

      p <- fits %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$.fitted1, y = .data$.fitted2))

      if(nrow(fits) > 200){
        p <- p +
          ggplot2::geom_point(size = 1.5, colour = "black")
      } else{
        p <- p +
          ggplot2::geom_point(size = 2, colour = "black")
      }

      p <- p +
        ggplot2::labs(title = "Low dimensional projection of time series",
                      x = paste0("PC 1"," (", eigen_pc1, ")"),
                      y = paste0("PC 2"," (", eigen_pc2, ")")) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    }
  } else{

    # Retrieve 2-dimensional embedding and add in unique IDs

    id_ref <- x[[2]] %>%
      tibble::rownames_to_column(var = "id") %>%
      dplyr::select(c(.data$id))

    fits <- data.frame(.fitted1 = x[[3]]$.fitted1,
                       .fitted2 = x[[3]]$.fitted2) %>%
      dplyr::mutate(id = id_ref$id)

    fits <- fits %>%
      dplyr::mutate(id = as.factor(.data$id))

    if("group" %in% colnames(x[[1]])){

      data_id <- as.data.frame(lapply(x[[1]], unlist)) # Catch weird cases where it's a list...

      groups <- data_id %>%
        dplyr::rename(group_id = .data$group) %>%
        dplyr::reframe(counter = dplyr::n(),
                         .by = c(.data$id, .data$group_id)) %>%
        dplyr::select(-c(.data$counter)) %>%
        dplyr::mutate(id = as.factor(.data$id))

      fits <- fits %>%
        dplyr::inner_join(groups, by = c("id" = "id"))

      # Draw plot

      p <- fits %>%
        dplyr::mutate(group_id = as.factor(.data$group_id)) %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$.fitted1, y = .data$.fitted2))

      if(show_covariance){
        p <- p +
          ggplot2::stat_ellipse(ggplot2::aes(x = .data$.fitted1, y = .data$.fitted2, fill = .data$group_id), geom = "polygon", alpha = 0.2) +
          ggplot2::guides(fill = "none") +
          ggplot2::scale_fill_brewer(palette = "Dark2")
      }

      if(nrow(fits) > 200){
        p <- p +
          ggplot2::geom_point(size = 1.5, ggplot2::aes(colour = .data$group_id))
      } else{
        p <- p +
          ggplot2::geom_point(size = 2.25, ggplot2::aes(colour = .data$group_id))
      }

      p <- p +
        ggplot2::labs(title = "Low dimensional projection of time series",
                      x = "Dimension 1",
                      y = "Dimension 2",
                      colour = NULL) +
        ggplot2::scale_colour_brewer(palette = "Dark2") +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       legend.position = "bottom")

    } else{

      p <- fits %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$.fitted1, y = .data$.fitted2))

      if(nrow(fits) > 200){
        p <- p +
          ggplot2::geom_point(size = 1.5, colour = "black")
      } else{
        p <- p +
          ggplot2::geom_point(size = 2, colour = "black")
      }

      p <- p +
        ggplot2::labs(title = "Low dimensional projection of time series",
                      x = "Dimension 1",
                      y = "Dimension 2") +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    }
  }

  return(p)
}

#------------------------- interval_calculations object methods -------------------------

#' Produce a plot for a interval_calculations object
#'
#' @importFrom rlang .data
#' @import dplyr
#' @importFrom ggplot2 ggplot aes geom_errorbar geom_point labs scale_colour_brewer theme_bw theme element_blank
#' @param x \code{interval_calculations} object containing the summary calculated by \code{interval}
#' @param ... Arguments to be passed to methods
#' @return object of class \code{ggplot} that contains the graphic
#' @author Trent Henderson
#' @export
#'

plot.interval_calculations <- function(x, ...){

  stopifnot(inherits(x, "interval_calculations") == TRUE)

  p <- x %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(.data$feature_set, -.data$.mean), y = .data$.mean,
                                 colour = .data$feature_set)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$.lower, ymax = .data$.upper)) +
    ggplot2::geom_point(size = 5) +
    ggplot2::labs(x = "Feature set",
                  y = "Classification accuracy") +
    ggplot2::scale_colour_brewer(palette = "Dark2") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.minor = ggplot2::element_blank())

  print(p)
}
