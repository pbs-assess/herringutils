% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures.R
\name{plot_pa}
\alias{plot_pa}
\title{Plot proportions-at-age time series from a data frames as extracted from iscam data (dat) files}
\usage{
plot_pa(
  df,
  age_plus = 10,
  conf = 0.9,
  xlim = c(1000, 3000),
  ylim = c(0, NA),
  size_range = c(0.5, 2.5),
  translate = FALSE
)
}
\arguments{
\item{df}{a data frame as constructed by [get_pa()]}

\item{age_plus}{age plus group}

\item{conf}{confidence value for the envelope}

\item{xlim}{limits for the years shown on the plot}

\item{ylim}{limits for the ages shown on the plot}

\item{size_range}{vector of min and max for range of sizes of points}

\item{translate}{Logical. If TRUE, translate to French}
}
\value{
A ggplot object
}
\description{
Plot proportions-at-age time series from a data frames as extracted from iscam data (dat) files
}
