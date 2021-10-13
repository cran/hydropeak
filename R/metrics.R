#' Event Based Fluctuation Flow Parameters
#' @description Given an event with equal flow trend, all metrics implemented in the package \pkg{hydropeak} are computed and combined to a data frame.
#' It includes also an identifier of the gauging station provided by the user and the starting time of the event.
#'
#' @param x Data frame (time series) from an event with equal flow trend. The data frame must contain a date-time column (\code{Time}) and a flow rate column (\code{Q}) and must be in a compatible format
#' (see \code{\link[=flow]{flow()}}).
#' @param id Character string which refers to the identifier of the gauging station (in Austria: HZBCODE).
#' @param event_type Numeric value which indicates the event type. By using \code{\link[=get_events]{get_events()}}, this is internally computed. If metrics for a single event
#' have to be computed individually, the event type has to be provided as an argument or can be computed with \code{\link[=event_type]{event_type()}}.
#'
#' @return A data frame including all computed metrics, IDs of gauging stations, event type (see \code{\link[=event_type]{event_type()}} for an overview of possible event types), and starting time of an event \code{x}.
#' Included metrics are \code{\link[=amp]{amp()}}, \code{\link[=mafr]{mafr()}}, \code{\link[=mefr]{mefr()}}, \code{\link[=dur]{dur()}}, \code{\link[=fr]{fr()}}.
#' @export
#'
#' @examples
#' data(Q)
#' # decreasing event:
#' Q4 <- flow(Q[3:4, ])
#' all_metrics(Q4, id = Q$ID[1], event_type = 4)
#' all_metrics(Q4, id = Q$ID[1], event_type = event_type(Q4))
#'
#' # increasing event:
#' Q2 <- flow(Q[486:487, ])
#' all_metrics(Q2, id = Q$ID[1], event_type = 2)
#' all_metrics(Q2, id = Q$ID[1], event_type = event_type(Q2))
#'
#' # constant event (at beginning or after NA event):
#' Q0 <- flow(Q[1:3, ])
#' all_metrics(Q0, id = Q$ID[1], event_type = 0)
#' all_metrics(Q0, id = Q$ID[1], event_type = event_type(Q0))
all_metrics <- function(x, id, event_type) {
  stopifnot(!missing(id))

  event <- event_type

  data.frame(ID = id,
             EVENT_TYPE = event,
             Time = x$Time[1],
             AMP = amp(x),
             MAFR = mafr(x),
             MEFR = mefr(x),
             DUR = dur(x),
             FR = fr(x, event_type = event))
}


#' AMP - Amplitude (Metric 1)
#' @description The amplitude (AMP, unit: \eqn{m^3/s}) of an event is defined as the difference between the flow maximum (\eqn{Q_{max}}{Q_max}) and the flow minimum (\eqn{Q_{min}}{Q_min}).
#' Given an event with equal flow trend, the amplitude is computed and returned.
#'
#' @param x Data frame (time series) from an event with equal flow trend. The data frame must contain a date-time column (\code{Time}) and a flow rate column (\code{Q}) and must be in a compatible format
#' (see \code{\link[=flow]{flow()}}).
#'
#' @return Returns a positive numeric value which is the difference of \code{max(x$Q)} and \code{min(x$Q)} of an event. If a data frame containing \code{NA} flow rates (\code{Q}) is given, \code{NA} is returned.
#' @export
#'
#' @examples
#' data(Q)
#' Q <- flow(Q[3:4, ])
#' amp(Q)
amp <- function(x) {
  validate_df(x)

  if (any(is.na(x$Q))) {
    return(NA_real_)
  }

  return(abs(x$Q[nrow(x)] - x$Q[1]))
}

#' MAFR - Maximum Flow Fluctuation Rate (Metric 2)
#' @description The maximum flow fluctuation rate (MAFR, unit: \eqn{m^3/s}) represents the highest absolute flow change of two consecutive time steps within an event.
#' Given an event with equal flow trend, the maximum flow fluctuation rate is computed and returned.
#'
#' @param x Data frame (time series) from an event with equal flow trend. The data frame must contain a date-time column (\code{Time}) and a flow rate column (\code{Q}) and must be in a compatible format
#' (see \code{\link[=flow]{flow()}}).
#'
#' @return Returns a numeric value which is the maximum (absolute) flow fluctuation rate. If a data frame containing \code{NA} flow rates (\code{Q}) is given, \code{NA} is returned.
#' @export
#'
#' @examples
#' data(Q)
#' Q <- flow(Q[3:4, ])
#' mafr(Q)
mafr <- function(x) {
  validate_df(x)

  if (any(is.na(x$Q))) {
    return(NA_real_)
  }

  max(abs(diff(x$Q)))
}

#' MEFR - Mean Flow Fluctuation Rate (Metric 3)
#' @description The mean flow fluctuation rate (MEFR, unit: \eqn{m^3/s^2}) is calculated by the event amplitude divided by the number of time steps (duration) within an event.
#' Given an event with equal flow trend, amplitude and duration are computed. From these metrics the mean flow fluctuation rate is calculated and returned.
#'
#' @param x Data frame (time series) from an event with equal flow trend. The data frame must contain a date-time column (\code{Time}) and a flow rate column (\code{Q}) and must be in a compatible format.
#' (see \code{\link[=flow]{flow()}}).
#' @return Returns a numeric value which is the mean flow fluctuation rate computed by the event amplitude \code{\link[=amp]{amp()}} divided by the number of time steps \code{\link[=dur]{dur()}}. If a data frame containing \code{NA} flow rates (\code{Q}) is given, \code{NA} is returned.
#' @export
#'
#' @examples
#' data(Q)
#' Q <- flow(Q[3:4, ])
#' mefr(Q)
mefr <- function(x) {
  validate_df(x)

  if (any(is.na(x$Q))) {
    return(NA_real_)
  }

  amp <- amp(x)
  dur <- dur(x)

  amp/dur
}

#' DUR - Duration (Metric 4)
#' @description The duration of an event is specified as the number of consecutive time steps with equal flow trend.

#'
#' @param x Data frame (time series) from an event with equal flow trend. The data frame must contain a date-time column (\code{Time}) and a flow rate column (\code{Q}) and must be in a compatible format.
#' (see \code{\link[=flow]{flow()}}).
#' @return Returns an integer value which is the number of consecutive time steps.
#' @export
#'
#' @examples
#' data(Q)
#' Q <- flow(Q[3:4, ])
#' dur(Q)
dur <- function(x) {
  validate_df(x)

  nrow(x)-1L
}

#' FR - Flow Ratio (Metric 5)
#' @description The metric flow ratio (FR) is defined as the flow maximum divided by the flow minimum, \eqn{\frac{Q_{max}}{Q_{min}}}.
#' Given an event with equal flow trend, the flow ratio is computed and returned.
#'
#' @param x Data frame (time series) from an event with equal flow trend. The data frame must contain a date-time column (\code{Time}) and a flow rate column (\code{Q}) and must be in a compatible format.
#' (see \code{\link[=flow]{flow()}}).
#' @param event_type Numeric value which specifies the event type. See \code{\link[=get_events]{get_events()}} for an overview of the event types.
#'
#' @return Returns a numeric value which is the flow ratio computed by \code{max(x$Q)} divided by \code{min(x$Q)}. If a data frame containing \code{NA} flow rates (\code{Q}) is given, \code{NA} is returned.
#' @export
#'
#' @examples
#' data(Q)
#' Q <- flow(Q[3:4, ])
#' fr(Q, event_type(Q))
fr <- function(x, event_type) {
  validate_df(x)

  if (missing(event_type)) stop("event_type is missing")

  if (event_type == 2) { # IC
    return(x$Q[nrow(x)] / x$Q[1])

  } else if (event_type == 4) { # DC
    return(x$Q[1] / x$Q[nrow(x)])

  } else if (event_type == 0 | event_type == 1 | event_type == 3) { # constant event (0, 1, 3)
    return(1)
  } else if (event_type == 5) {  # NA event (5)
    return(NA_real_)
  } else {
    stop("Invalid event type.")
  }
}


#' Validate Data
#' @description Checks if input data are valid and produces error messages with \code{\link[base::stopifnot]{base::stopifnot()}}.
#' @param x Data frame (flow() object, see \code{\link[=flow]{flow()}}).
#' @return NULL if checked statements are TRUE.
#' @noRd
#' @keywords internal
validate_df <- function(x) {
  stopifnot(!missing(x))
  stopifnot(is.data.frame(x))
  stopifnot(inherits(x, "flow"))
}
