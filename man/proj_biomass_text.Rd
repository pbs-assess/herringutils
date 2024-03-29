% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text.R
\name{proj_biomass_text}
\alias{proj_biomass_text}
\title{Text for "Application of MPs.." section.}
\usage{
proj_biomass_text(SAR, next_yr, p_lrp = 0.3, translate)
}
\arguments{
\item{SAR}{Character. SAR name (e.g., "HG").}

\item{next_yr}{Numeric. Usually `assess_yr + 1`.}

\item{p_lrp}{Numeric. Proportion of SB_0 that is the LRP (default is 0.3).}

\item{translate}{Logical. Translate to french if TRUE.}
}
\value{
Text.
}
\description{
Text for "Application of MPs.." section.
}
\note{
Requires variables from the `get_vars()` function named for the SAR
  (i.e., `hg_vars <- get_vars("HG", french = french)`).
}
