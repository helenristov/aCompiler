% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getKalmanFitted.R
\name{getKalmanFitted}
\alias{getKalmanFitted}
\title{Computes the Kalman Filter model values using the DLM Polynomial Fit}
\usage{
getKalmanFitted(data, order.model = 2, dV = "vol", window = 0,
  fit.type = "full", err.sd.lvl = NULL)
}
\arguments{
\item{data}{A price series}

\item{order.model}{The order of the polynomial for the dlmModPoly approximation and smoothing}

\item{dV}{The observation variance to be used in the polynomial fit. The default value is set to "vol" which compute the variance of the price series. Other values can be inputed to}

\item{window}{The window for anchored or rolling fit mechanisms. The default is set to 0 since fit.type is defaulted to full.}

\item{fit.type}{The different ways we can obtain the fitted values. The accepted type are full, anchored, and rolling. "full" parameterizes the model on the entire data range. Anchored sets a fixed window and every forward estimate uses all data from the start of the data vector. Rolling computes the next value from a rolling window for model parameterization.}

\item{err.sd.lvl}{The error threshold at which we generate the buy and sell signals for the error correction model}
}
\value{
list of values are returned
\item{dlm.fitted}{A vector of fitted model values}
}
\description{
Computes the Kalman Filter model values using the DLM Polynomial Fit
}
\author{
Helena Ristov
}
