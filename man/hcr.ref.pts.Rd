% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hcr.R
\name{hcr.ref.pts}
\alias{hcr.ref.pts}
\title{Calculate TAC and TAC-based harvest rate based on biomass reference points and reference harvest rate}
\usage{
hcr.ref.pts(bt, ref.hr, lrp, usr, catch.cap = 0, bo = 1, num.end.yrs = 1)
}
\arguments{
\item{bt}{biomass vector for years}

\item{ref.hr}{reference harvest rate}

\item{lrp}{limit reference point}

\item{usr}{upper stock reference point}

\item{catch.cap}{catch cap to use if calculated catch limit is higher}

\item{bo}{initial biomass used in reletive biomass calculation}

\item{num.end.yrs}{mean of these last N years at the end of the biomass vector to use}
}
\value{
a vector of length 2 made up of the catch limit and corresponding target harvest rate
}
\description{
Calculate TAC and TAC-based harvest rate based on biomass reference points and reference harvest rate
}
