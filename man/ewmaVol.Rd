% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ewmaVol.R
\name{ewmaVol}
\alias{ewmaVol}
\title{Calculates the Exponential Weighted Moving Average volatilty}
\usage{
ewmaVol(prices, dPeriod, lambda, UseLog = TRUE, .combine = FALSE,
  skipDiff = FALSE)
}
\arguments{
\item{prices}{A price series on which you want to compute the EWMA volatility measure}

\item{UseLog}{A boolean to determine if we should take the log of the price series. The series must all be positive to set UseLog = TRUE otherwise set to FALSE to use the differences method.}

\item{lamda}{The exponential decay factor to be used as weights}
}
\description{
Calculates the Exponential Weighted Moving Average volatilty
}
\author{
Helena Ristov
}
