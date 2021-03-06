% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{spawn_index_by_area_table}
\alias{spawn_index_by_area_table}
\title{Table for spawn index by area}
\usage{
spawn_index_by_area_table(tab, cap = "", first_yr, translate = FALSE, ...)
}
\arguments{
\item{tab}{data.frame as read in by [readr::read_csv()]}

\item{cap}{caption for table}

\item{first_yr}{Earliest year to show in the table}

\item{translate}{Logical. Translate to french if TRUE}

\item{...}{arguments passed to [csas_table()]}
}
\value{
a [csasdown::csas_table()]
}
\description{
Table for spawn index by area
}
