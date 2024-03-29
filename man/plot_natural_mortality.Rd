% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures.R
\name{plot_natural_mortality}
\alias{plot_natural_mortality}
\title{Plot natural mortality mcmc median and credibility interval}
\usage{
plot_natural_mortality(
  model,
  line_size = 0.75,
  ribbon_alpha = 0.5,
  prod_yrs = 1990:1999,
  show_prod_yrs = TRUE,
  xlim = NA,
  y_max = NA,
  show_x_axis = TRUE,
  show_y_axis = TRUE,
  x_axis_label_size = 8,
  x_axis_tick_label_size = 8,
  y_axis_label_size = 8,
  y_axis_tick_label_size = 8,
  x_axis_label_newline_length = 50,
  y_axis_label_newline_length = 40,
  x_axis_position = "bottom",
  y_axis_position = "left",
  annot = "b",
  translate = FALSE
)
}
\arguments{
\item{model}{an iscam model}

\item{line_size}{thickness of the median line}

\item{ribbon_alpha}{transparency of the credibility interval ribbon}

\item{prod_yrs}{Numeric vector. Productive period to calculate the USR.
Default 1990:1999.}

\item{show_prod_yrs}{Logical. Show vertical band for productive period.
Default TRUE.}

\item{xlim}{x-limits for the plot. Implemented with [ggplot2::coord_cartesian()]}

\item{show_x_axis}{see [modify_axes_labels()]}

\item{show_y_axis}{see [modify_axes_labels()]}

\item{x_axis_label_size}{see [modify_axes_labels()]}

\item{x_axis_tick_label_size}{see [modify_axes_labels()]}

\item{y_axis_label_size}{see [modify_axes_labels()]}

\item{y_axis_tick_label_size}{see [modify_axes_labels()]}

\item{x_axis_label_newline_length}{see [newline_format()]}

\item{y_axis_label_newline_length}{see [newline_format()]}

\item{annot}{a character to place in parentheses in the top left of the plot.
If NA, nothing will appear}

\item{translate}{Logical. If TRUE, translate to french}
}
\value{
A ggplot object
}
\description{
Plot natural mortality mcmc median and credibility interval
}
