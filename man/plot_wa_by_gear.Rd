% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures.R
\name{plot_wa_by_gear}
\alias{plot_wa_by_gear}
\title{Plot weight-at-age time series from a data frames as extracted from csv files
created by dataSummaries}
\usage{
plot_wa_by_gear(
  df,
  xlim = c(1000, 3000),
  ylim = c(0, NA),
  n_roll = 5,
  major = TRUE,
  translate = FALSE
)
}
\arguments{
\item{df}{a data frame}

\item{xlim}{Limits for the years shown on the plot}

\item{ylim}{limits for the weights shown on the plot}

\item{n_roll}{Number of years to calculate the rolling mean (window)}

\item{major}{Logical. Major SAR or not.}

\item{translate}{Logical. If TRUE, translate to French}
}
\value{
A ggplot object
}
\description{
Plot weight-at-age time series from a data frames as extracted from csv files
created by dataSummaries
}
