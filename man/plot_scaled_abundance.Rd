% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures.R
\name{plot_scaled_abundance}
\alias{plot_scaled_abundance}
\title{-----------------------------------------------------------------------------------------------
Plot the median SSB as a line, with points which are survey index scaled by catchability value
for the survey}
\usage{
plot_scaled_abundance(
  df,
  model,
  gear,
  new_surv_yr = NA,
  point_size = 1,
  line_size = 0.75,
  prod_yrs = 1990:1999,
  show_prod_yrs = TRUE,
  xlim = NA,
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
  annot = "a",
  show_legend = FALSE,
  translate = FALSE
)
}
\arguments{
\item{df}{Data frame of the survey estimates, as constructed by [get_surv_ind()]}

\item{model}{an iscam model object}

\item{gear}{a gear data frame containing `gear`, `gearname`, and `qind` columns}

\item{new_surv_yr}{the year when the survey changed from surface to dive}

\item{point_size}{size for points}

\item{line_size}{thickness of line}

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

\item{show_legend}{Logical}

\item{translate}{Logical. If TRUE, translate to french}
}
\value{
A ggplot object
}
\description{
-----------------------------------------------------------------------------------------------
Plot the median SSB as a line, with points which are survey index scaled by catchability value
for the survey
}
