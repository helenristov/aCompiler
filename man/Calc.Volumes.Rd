% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Calc.Volumes.R
\name{Calc.Volumes}
\alias{Calc.Volumes}
\title{Creates a time-series of the traded volumes for a given time period.}
\usage{
Calc.Volumes(data, contract, lookBackPeriod = 1)
}
\arguments{
\item{data}{MDR or TRS data.}

\item{contract}{The contract that you are pulling data for.}

\item{lookBackPeriod}{A list containing the lookback units and size for a rolling volume calculation specified in secs,hours,day,weeks,months}
}
\description{
Creates a time-series of the traded volumes for a given time period.
}
\author{
Helen Ristov and Cameron Sawyer
}
