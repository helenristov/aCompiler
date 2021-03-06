% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalcVar.R
\name{CalcVar}
\alias{CalcVar}
\title{Calculates Variances for Different Contract Types Based on an Outright Covariance Matrix}
\usage{
CalcVar(OR_Cov, Types, date = last(GetWeekDays(Sys.Date() - 8, Sys.Date() -
  1)))
}
\arguments{
\item{OR_Cov}{Outright covariance matrix to be used in the variance calculations}

\item{Types}{Types of combinations to be calculated from the outright covariance matrix.}

\item{date}{optional parameter in case of ICS var calcs.  Need a date to determine the TLS beta to use.}
}
\description{
Calculates Variances for Pre-Selected Contract Types
}
\details{
CalcVar can calculate variances from an outright covariance matrix for outrights, calendar spreads, butterflies, double butterflies, packs, pack spreads and bundles.
}
\author{
Nicholas Dregne and Helena Ristov
}
