
% Generated by roxygen2 (4.0.1): do not edit by hand

% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/IncrCapture.R

\name{IncrCapture}
\alias{IncrCapture}
\title{Incr Capture}
\usage{
IncrCapture(TickCount, slippage = 0)
}
\arguments{
\item{TickCount}{A tick count series.}

\item{slippage}{Optional factor that penalizes the assumption of perfect execution by a percentage of the increment}
}
\description{
Internal Function That Calculates a Incr Profit Given a Specific Incr Size
}
\details{
This calculation is very crude as it assumes all increments can be held forever and there is no puke point.
}
\author{
Nicholas Dregne
}

