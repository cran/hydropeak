% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/events.R
\name{change_points}
\alias{change_points}
\title{Change Points}
\usage{
change_points(x, rownames = FALSE, omit.constant = TRUE, omit.na = TRUE)
}
\arguments{
\item{x}{Data frame (time series) of stage measurements. Data frame must contain
a date-time column (\code{Time}) and a flow rate column (\code{Q}) and must
be in a compatible format. (see \code{\link[=flow]{flow()}}).}

\item{rownames}{A logical. If \code{FALSE} (default) indices are used to
determine change points, otherwise row names. \code{rownames = TRUE} is
important for data with different IDs.}

\item{omit.constant}{A logical. If \code{FALSE} (default) it does not return
events with constant measurements. Otherwise these events are included.}

\item{omit.na}{A logical. If \code{FALSE} (default) it does not return missing
value events. Otherwise these events are included.}
}
\value{
A data frame which contains change point indices in the input data frame.
    These are the indices where an event starts and ends, and a column which
    indicates the event (-1 = decreasing (DC), 1 = increasing (IC)).
}
\description{
Calculates flow (Q) differences of consecutive time steps and
    discriminates between time steps with increasing (IC: \eqn{Q_{ts1} < Q_{ts2}})
    and decreasing (DC: \eqn{Q_{ts1} > Q_{ts2}}) flow. The differences are
    transformed to 1 or -1 if the number is positive or negative, respectively.
    Then the lengths of runs of equal values are computed to determine the
    change point indices in the input data frame. \code{NA} in Q values is
    replaced with 0 and treated as an equal trend.
}
\keyword{internal}
