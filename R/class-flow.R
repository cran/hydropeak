#' 'flow' S3 Class for Flow Rate Time Series
#' @description The function \code{flow()} creates a flow rate time series object
#'     which is formatted to be compatible with the functions in the \pkg{hydropeak}
#'     package.
#' @param x Data frame which contains at least a column with an ID of the gauging
#'     station, a column with date-time values in character representation and a
#'     column with flow rates
#' @param format Character string giving the date-time format of the date-time
#'     column in the input data frame  (default: \code{dd.mm.YYYY HH:MM})
#' @param tz Character string specifying the time zone to be used for the conversion
#'     (default: \code{Etc/GMT-1}).
#' @param cols Integer vector specifying column indices in the input data frame
#'     which contain gauging station ID, date-time and flow rate to be renamed.
#'     The default indices are 1 (ID), 2 (date-time) and 3 (flow rate, Q).
#' @param steplength Numeric value which specifies the distance between (equispaced)
#'     time steps in minutes. (default: \code{15}, which refers to 15 minutes).
#'     Non-equispaced time steps are not supported and missing time steps are
#'     imputed if argument \code{full} is set to \code{TRUE} (default), Q values
#'     are assumed to be \code{NA}.
#' @param full A logical. If \code{TRUE} (default) imputes missing time step values
#'     so the time series is complete. Imputed Q values are set to NA. It should
#'     only be set to \code{FALSE}, if it is known, that the time series is complete.
#' @return  Returns a \code{flow} object which inherits from data frame (time series).
#'     It contains at least a gauging station ID column (\code{ID}) converted to
#'     character values, a date-time column (\code{Time}) converted to class "POSIXct"
#'     (after conversion using \code{\link[base:strptime]{base::strptime()}})
#'     and a flow rate column (\code{Q}), which is converted to numeric values.
#'     The \code{flow()} object ensures that input flow fluctuation time series data
#'     can be processed with the functions in the \pkg{hydropeak} package. Therefore,
#'     it is mandatory to provide the correct indices (see argument \code{cols}) and
#'     the correct date-time format (see argument \code{format}) of the input data frame.
#' @export
#'
#' @examples
#' data(Q)
#' Q <- flow(Q)
flow <-
  function(x,
           format = "%d.%m.%Y %H:%M",
           tz = "Etc/GMT-1",
           cols = c(1, 2, 3),
           steplength = 15,
           full = TRUE) {
    validate_flow(new_flow(
      x,
      format = format,
      cols = cols,
      steplength = steplength,
      full = full
    ))
  }


#' Constructor for 'flow' class
#'
#' @keywords internal
#' @noRd
new_flow <-
  function(x = data.frame(),
           format = "%d.%m.%Y %H:%M",
           tz = "Etc/GMT-1",
           cols = c(1, 2, 3),
           steplength = 15,
           full = TRUE) {
    stopifnot(is.data.frame(x))

    names(x)[cols] <- c("ID", "Time", "Q")
    x$Time <- as.POSIXct(strptime(x$Time, format = format, tz = tz))
    x$Q <- suppressWarnings(as.numeric(x$Q))
    x$ID <- as.character(x$ID)

    # make time steps consecutive
    if (full) {
      x <-
        impute_flow(x,
                    format = format,
                    tz = tz,
                    steplength = steplength)
    }
    attributes(x)$format <- format
    attributes(x)$cols <- cols
    attributes(x)$steplength <- steplength

    class(x) <- c("flow", "data.frame")

    row.names(x) <- NULL # resets row names

    x
  }

#' Validate Input Data
#' @export
#' @rdname flow
validate_flow <- function(x) {
  stopifnot(all(c("ID", "Time", "Q") %in% colnames(x))) # valid column names
  stopifnot(inherits(x$Time, "POSIXt")) # Time must be a date-time object

  # check for multiple Time/ID combinations
  stopifnot(!any(duplicated(x[, c("ID", "Time")])))

  x
}

#' Impute Missing Time Steps
#'
#' @keywords internal
#' @noRd
impute_flow <-
  function(x,
           steplength = 15,
           format = "%d.%m.%Y %H:%M",
           tz = "Etc/GMT-1") {
    x_split <- split(x, ~ ID)
    steplength <- as.difftime(steplength, units = "mins")

    for (i in seq_along(x_split)) {
      miss <-
        which(!diff(x_split[[i]]$Time) == steplength)  # detect non-equispaced time steps

      if (length(miss) > 0) {
        warning("ID ", x_split[[i]]$ID[1], ": Missing time steps imputed with NA")
        df <-
          data.frame(
            ID = c(x_split[[i]]$ID, rep(x_split[[i]]$ID[1], times = length(miss))),
            Time = c(x_split[[i]]$Time, rep(x_split[[i]]$Time[1], times = length(miss))),
            Q = c(x_split[[i]]$Q, rep(NA, times = length(miss)))
          )

        for (j in seq_along(miss)) {
          df[(nrow(x_split[[i]]) + j), "Time"] <-
            x_split[[i]][miss[j], ]$Time + steplength
        }

        # sort by increasing date
        x_split[[i]] <- df[order(df$Time, decreasing = FALSE), ]
      }
    }
    # return unlisted x_split
    return(do.call("rbind", x_split))
  }
