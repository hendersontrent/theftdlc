% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project.R
\name{project}
\alias{project}
\alias{reduce_dims}
\title{Project a feature matrix into a two-dimensional representation using PCA, MDS, t-SNE, or UMAP ready for plotting}
\usage{
project(
  data,
  norm_method = c("zScore", "Sigmoid", "RobustSigmoid", "MinMax", "MaxAbs"),
  unit_int = FALSE,
  low_dim_method = c("PCA", "tSNE", "ClassicalMDS", "KruskalMDS", "SammonMDS", "UMAP"),
  na_removal = c("feature", "sample"),
  seed = 123,
  ...
)

reduce_dims(
  data,
  norm_method = c("zScore", "Sigmoid", "RobustSigmoid", "MinMax", "MaxAbs"),
  unit_int = FALSE,
  low_dim_method = c("PCA", "tSNE", "ClassicalMDS", "KruskalMDS", "SammonMDS", "UMAP"),
  na_removal = c("feature", "sample"),
  seed = 123,
  ...
)
}
\arguments{
\item{data}{\code{feature_calculations} object containing the raw feature matrix produced by \code{theft::calculate_features}}

\item{norm_method}{\code{character} denoting the rescaling/normalising method to apply. Can be one of \code{"zScore"}, \code{"Sigmoid"}, \code{"RobustSigmoid"}, \code{"MinMax"}, or \code{"MaxAbs"}. Defaults to \code{"zScore"}}

\item{unit_int}{\code{Boolean} whether to rescale into unit interval \code{[0,1]} after applying normalisation method. Defaults to \code{FALSE}}

\item{low_dim_method}{\code{character} specifying the low dimensional embedding method to use. Can be one of \code{"PCA"}, \code{"tSNE"}, \code{"ClassicalMDS"}, \code{"KruskalMDS"}, \code{"SammonMDS"}, or \code{"UMAP"}. Defaults to \code{"PCA"}}

\item{na_removal}{\code{character} defining the way to deal with NAs produced during feature calculation. Can be one of \code{"feature"} or \code{"sample"}. \code{"feature"} removes all features that produced any NAs in any sample, keeping the number of samples the same. \code{"sample"} omits all samples that produced at least one NA. Defaults to \code{"feature"}}

\item{seed}{\code{integer} to fix R's random number generator to ensure reproducibility. Defaults to \code{123}}

\item{...}{arguments to be passed to \code{stats::prcomp} or \code{Rtsne::Rtsne}, \code{stats::cmdscale}, \code{MASS::isoMDS}, \code{MASS::sammon}, or \code{umap::umap} depending on selection in \code{low_dim_method}}
}
\value{
object of class \code{feature_projection} which is a named list containing the \code{feature_calculations} data supplied to the function, the wide matrix of filtered data, a tidy \code{data.frame} of the projected 2-D data, and the model fit object
}
\description{
Project a feature matrix into a two-dimensional representation using PCA, MDS, t-SNE, or UMAP ready for plotting
}
\examples{
\donttest{

library(theft)

features <- theft::calculate_features(theft::simData,
  feature_set = "catch22")

pca <- project(features,
  norm_method = "zScore",
  low_dim_method = "PCA")
}

}
\author{
Trent Henderson
}
