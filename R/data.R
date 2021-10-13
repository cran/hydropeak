#' Flow Fluctuations Q
#'
#' A dataset containing sub-daily flow fluctuations of five consecutive days from two different gauging stations.
#' One time step is 15 minutes.
#'
#' @format A data frame with 960 rows and 3 variables:
#' \describe{
#'   \item{ID}{Character string which refers to the identifier of the gauging station (in Austria: HZBCODE)}
#'   \item{Time}{Character string with date-time information of stage measurements which needs to be converted to a compatible format (see \code{\link[=flow]{flow()}})}
#'   \item{Q}{Flow, stage measurements in \eqn{m^3/s}}
#' }
"Q"

#' Events
#'
#' A complementary dataset to the flow fluctuation dataset \link{Q}. It contains the events and metrics such as computed by \code{\link[=get_events]{get_events()}}.
#'
#' @format A data frame with 165 rows and 8 variables:
#' \describe{
#'   \item{ID}{Character string which refers to the identifier of the gauging station (in Austria: HZBCODE)}
#'   \item{EVENT_TYPE}{Event types are defined as follows:
#'     \itemize{
#'      \item 0: Constant event after NA event or constant event as first event in time series
#'      \item 1: Constant event after DC
#'      \item 2: Increasing event (IC)
#'      \item 3: Constant event after IC
#'      \item 4: Decreasing event (DC)
#'      \item 5: NA event}}
#'   \item{Time}{Date-time of event starting point}
#'   \item{AMP}{Amplitude (\code{\link[=amp]{amp()}})}
#'   \item{MAFR}{Maximum flow fluctuation rate (\code{\link[=mafr]{mafr()}})}
#'   \item{MEFR}{Mean flow fluctuation rate (\code{\link[=mefr]{mefr()}})}
#'   \item{DUR}{Duration (\code{\link[=dur]{dur()}})}
#'   \item{FR}{Flow ratio (\code{\link[=fr]{fr()}})}
#' }
"Events"
