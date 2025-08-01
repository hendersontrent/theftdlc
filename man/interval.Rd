% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval.R
\name{interval}
\alias{interval}
\alias{calculate_interval}
\title{Calculate interval summaries with a measure of central tendency of classification results}
\usage{
interval(
  data,
  metric = c("accuracy", "precision", "recall", "f1"),
  by_set = TRUE,
  type = c("sd", "se", "quantile"),
  interval = NULL,
  model_type = c("main", "null")
)

calculate_interval(
  data,
  metric = c("accuracy", "precision", "recall", "f1"),
  by_set = TRUE,
  type = c("sd", "se", "quantile"),
  interval = NULL,
  model_type = c("main", "null")
)
}
\arguments{
\item{data}{\code{list} object containing the classification outputs produce by \code{tsfeature_classifier}}

\item{metric}{\code{character} denoting the classification performance metric to calculate intervals for. Can be one of \code{"accuracy"}, \code{"precision"}, \code{"recall"}, \code{"f1"}. Defaults to \code{"accuracy"}}

\item{by_set}{\code{Boolean} specifying whether to compute intervals for each feature set. Defaults to \code{TRUE}. If \code{FALSE}, the function will instead calculate intervals for each feature}

\item{type}{\code{character} denoting whether to calculate a +/- SD interval with \code{"sd"}, confidence interval based off the t-distribution with \code{"se"}, or based on a quantile with \code{"quantile"}. Defaults to \code{"sd"}}

\item{interval}{\code{numeric} scalar denoting the width of the interval to calculate. Defaults to \code{1} if \code{type = "sd"} to produce a +/- 1 SD interval. Defaults to \code{0.95} if \code{type = "se"} or \code{type = "quantile"} for a 95 per cent interval}

\item{model_type}{\code{character} denoting whether to calculate intervals for main models with \code{"main"} or null models with \code{"null"} if the \code{use_null} argument when using \code{tsfeature_classifier} was \code{use_null = TRUE}. Defaults to \code{"main"}}
}
\value{
\code{interval_calculations} object which is a data frame containing the results
}
\description{
Calculate interval summaries with a measure of central tendency of classification results
}
\examples{

library(theft)

features <- theft::calculate_features(theft::simData,
  feature_set = NULL,
  features = list("mean" = mean, "sd" = sd))

classifiers <- classify(features,
  by_set = FALSE,
  n_resamples = 3)

interval(classifiers,
  by_set = FALSE,
  type = "sd",
  interval = 1)

}
\author{
Trent Henderson
}
