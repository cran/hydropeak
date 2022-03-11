#' Flow Fluctuation Events and Metrics
#' @description Given a data frame (time series) of stage measurements, all
#'     increase (IC) and decrease (DC) events are determined and all metrics
#'     implemented in the package \pkg{hydropeak}
#'     (see \code{\link[=all_metrics]{all_metrics()}}) are computed and combined
#'     to a data frame. Optionally, NA events and constant events can be included.
#'     NA events occur due to missing stage measurement values. The beginning
#'     of NA events refers to the last measurement with a non-missing Q value.
#'     Constant events are events where the Q values stay constant over time.
#'     An event is uniquely identifiable through the combination of the event starting
#'     time (\code{Time}) and the gauging station identifier (\code{ID}).
#'
#' @param x  Data frame (time series) of stage measurements which contains at
#'     least a column with ID of the gauging station (default: column index 1)
#'     column with date-time values (default: columns index 2) in character
#'     representation and a column with flow rates (default: column index 3).
#'     If the column indices differ from \code{c(1, 2, 3)}, they have to be
#'     specified as \code{...} argument in the format \code{c(i, j, k)}.
#' @param mc.cores Number of cores to use with
#'     \code{\link[parallel:mclapply]{parallel::mclapply()}}. On Windows, this
#'     will be set to 1.
#' @param omit.constant A logical. If \code{FALSE} (default) it does not return
#'     events with constant measurements. Otherwise these events are included.
#' @param omit.na A logical. If \code{FALSE} (default) it does not return missing
#'     value events. Otherwise these events are included.
#' @param ... Arguments to be passed to \code{\link[=flow]{flow()}} to specify
#'     the date-time format in the input data (default: \code{dd.mm.YYYY HH:MM}),
#'     the time zone used for the conversion (default: \code{Etc/GMT-1}) and the
#'     column indices in the input data, which contain date-time values and flow
#'     rate values. The default indices are 1 (ID), 2 (date-time) and 3 (flow rate,
#'     Q), i.e. \code{cols = c(1, 2, 3)}.
#' @return A data frame which contains for every event in a given time series all
#'     metrics (\code{\link[=all_metrics]{all_metrics()}}), gauging station ID,
#'     event type, and starting time of an event.
#'     Included metrics are \code{\link[=amp]{amp()}}, \code{\link[=mafr]{mafr()}},
#'     \code{\link[=mefr]{mefr()}}, \code{\link[=dur]{dur()}},
#'     \code{\link[=ratio]{ratio()}}. These metrics are only computed for
#'     increasing (IC) and decreasing (DC) events. For all other events the
#'     values are set to 0 except for flow ratio that is set to 1.
#'     Event types are defined as follows:
#'     \itemize{
#'       \item 0: Constant event after NA event or constant event as first event in time series
#'       \item 1: Constant event after DC
#'       \item 2: Increasing event (IC)
#'       \item 3: Constant event after IC
#'       \item 4: Decreasing event (DC)
#'       \item 5: NA event
#' }
#' @export
#'
#' @examples
#' # Data with multiple events and different stations
#' data(Q)
#' get_events(Q)
#'
#' # including constant events
#' get_events(Q, omit.constant = FALSE)
get_events <-
  function(x,
           mc.cores = 2L,
           omit.constant = TRUE,
           omit.na = TRUE,
           ...) {
    x <- flow(x, ...)

    if (.Platform$OS.type == "windows" & mc.cores > 1L) {
      mc.cores <- 1L
    }

    change <- do.call("rbind", lapply(split(x, x$ID),
                                      function(y)
                                        change_points(
                                          y,
                                          rownames = TRUE,
                                          omit.constant = omit.constant,
                                          omit.na = omit.na
                                        )))
    change <- as.list(as.data.frame(t(change)))

    # get events based on change points if data.table is not installed
    if (!requireNamespace("data.table", quietly = TRUE)) {
      df <-
        do.call("rbind",
                parallel::mclapply(change, function(y)
                  all_metrics(x[y[1]:y[2], ],
                              id = x$ID[y[1]],
                              event_type = y[3]),
                  mc.cores = mc.cores))
      df <-
        do.call("rbind", lapply(change, function(y)
          all_metrics(
            x[y[1]:y[2], ],
            id = x$ID[y[1]],
            event_type = y[3]
          )))
      # set row names
      attr(df, "row.names") <- as.character(seq_len(nrow(df)))


      # get events based on change points with data.table (significantly faster for large data)
    } else {
      df <-
        data.table::setDF(data.table::rbindlist(
          parallel::mclapply(change, function(y)
            all_metrics(x[y[1]:y[2], ],
                        id = x$ID[y[1]],
                        event_type = y[3]),
            mc.cores = mc.cores)
        ))
      # set row names
      attr(df, "row.names") <- as.character(row.names(df))

    }


    # return data frame
    df
  }



#' Flow Fluctuation Events and Metrics from Input File
#' @description Given a file path it reads a data frame (time series) of stage
#'     measurements and calls \code{\link[=get_events]{get_events()}}.
#'     The resulting events can be optionally written to a single file or to
#'     separate files for each gauging station ID (\code{ID}) and \code{Event_Type}.
#'     Files which produce errors return \code{NULL}.
#'
#' @param Q_file A character string containing the name of the file which the data
#'     are to be read from with \code{\link[utils:read.csv]{utils::read.csv()}}.
#' @param inputsep Field separator character string for input data.
#' @param inputdec Character string for decimal points in input data.
#' @param save A logical. If \code{FALSE} (default) events (results from
#'     \code{\link[=get_events]{get_events()}}) are not written to file(s),
#'     otherwise events are written to \code{outdir}.
#' @param split A logical. If \code{TRUE} (default) output files are separated by
#'     their gauging station ID (\code{ID}) and by \code{Event_Type}, otherwise
#'     all events are written to a single file.
#' @param outdir A character string naming a directory where the output file(s)
#'     should be written to.
#' @param mc.cores Number of cores to use with
#'     \code{\link[parallel:mclapply]{parallel::mclapply()}}. On Windows, this
#'     will be set to 1.
#' @param return A logical. If \code{TRUE} (default) it returns the resulting
#'     data frame or list of data frames. Otherwise it returns \code{NULL}.
#' @param ... Arguments to be passed to \code{\link[=get_events]{get_events()}}
#'     and further to \code{\link[=flow]{flow()}}.
#' @return A data frame which contains for every increase or decrease event in a
#'     given time series all metrics (\code{\link[=all_metrics]{all_metrics()}}),
#'     gauging station ID, event type, and starting time of an event.
#'     Included metrics are \code{\link[=amp]{amp()}}, \code{\link[=mafr]{mafr()}},
#'     \code{\link[=mefr]{mefr()}}, \code{\link[=dur]{dur()}}, \code{\link[=ratio]{ratio()}}.
#'     The returned data frame is not split. Returns \code{NULL}, if argument
#'     \code{return} is set to \code{FALSE}.
#'
#' @export
#'
#' @examples
#' Q_file <- system.file("extdata", "Q.csv", package = "hydropeak")
#' # save to tempdir()
#' events <- get_events_file(Q_file, inputsep = ",", inputdec = ".",
#' save = TRUE, split = TRUE, return = TRUE)
get_events_file <-
  function(Q_file,
           inputsep = ";",
           inputdec = ".",
           save = FALSE,
           split = TRUE,
           outdir = file.path(tempdir(), "Events"),
           mc.cores = 2L,
           return = TRUE,
           ...) {
    x <- utils::read.csv(Q_file, sep = inputsep, dec = inputdec)

    if (.Platform$OS.type == "windows" & mc.cores > 1L) {
      mc.cores <- 1L
    }

    # get events from Q_file, pass arguments for flow()
    # tryCatch returns NULL if error occurs (file cannot be converted to flow,...)
    # lapply needs NULL to ignore such files
    # if an error occurs: get_events_file() returns NULL and saves nothing to output directory
    events <-
      tryCatch(
        get_events(x, mc.cores = mc.cores, ...),
        error = function(e)
          NULL
      )

    # exit function if events is not valid
    if (is.null(events)) {
      return(NULL)
    }

    if (save) {
      if (split) {
        events_split <-
          split(events, list(events$ID, events$EVENT_TYPE), sep = "_")

        # get all possible id/event_type combinations
        index <- names(events_split)

        # remove id/event combinations where no events exist
        index <-
          unlist(parallel::mclapply(index, function(x)
            if (nrow(events_split[[x]]) > 0) {
              x
            },
            mc.cores = mc.cores))


        if (!dir.exists(outdir)) {
          dir.create(outdir)
        }

        parallel::mclapply(index,
                           function(x)
                             utils::write.csv(
                               x = events_split[[x]],
                               file.path(
                                 outdir,
                                 paste(
                                   x,
                                   "_",
                                   as.Date(events_split[[x]][1, "Time"]),
                                   "_",
                                   as.Date(events_split[[x]][nrow(events_split[[x]]), "Time"]),
                                   ".csv",
                                   sep = ""
                                 )
                               ),
                               row.names = FALSE
                             ),
                           mc.cores = mc.cores)

      } else {
        # all events from input file in one output file
        utils::write.csv(x = events, file = file.path(
          outdir,
          paste(
            "Events",
            as.Date(events[1, "Time"]),
            as.Date(events[nrow(events),
                           "Time"]),
            ".csv",
            sep = "_"
          )
        ))
      }

    }
    # return all events or nothing
    if (return) {
      events
    }
  }


#' Flow Fluctuation Events and Metrics from Input Directory
#' @description Given a directory path it calls
#'     \code{\link[=get_events]{get_events_file()}} for each file in the directory,
#'     recursively. The resulting events are split into separate files for each
#'     gauging station ID (\code{ID}) and \code{Event_Type} and are written to
#'     the given output directory.
#'
#' @param Q_dir A character string containing the path name where the input data
#'     are located.
#' @param inputsep Field separator character string for input data.
#' @param inputdec Character string for decimal points in input data.
#' @param outdir A character string naming a directory where the output file(s)
#'     should be written to.
#' @param mc.cores Number of cores to use with
#'     \code{\link[parallel:mclapply]{parallel::mclapply()}}. On Windows, this
#'     will be set to 1.
#' @param ... Arguments to be passed to \code{\link[=get_events_file]{get_events_file()}}
#'     and further to  \code{\link[=get_events]{get_events()}}  and \code{\link[=flow]{flow()}}.
#' @return No return value, called for side effects.
#'
#' @export
#'
#' @examples
#' Q_dir <- "./inst/extdata"
#' get_events_dir(Q_dir, inputsep = ",", inputdec = ".")
get_events_dir <- function(Q_dir,
                           inputsep = ";",
                           inputdec = ".",
                           outdir = file.path(tempdir(), "Events"),
                           mc.cores = 2L,
                           ...) {
  file_list <- list.files(Q_dir, recursive = TRUE, full.names = TRUE)

  if (.Platform$OS.type == "windows" & mc.cores > 1L) {
    mc.cores <- 1L
  }

  parallel::mclapply(file_list, function(x)
    get_events_file(
      x,
      inputsep = inputsep,
      inputdec = inputdec,
      save = TRUE,
      split = TRUE,
      outdir = outdir,
      mc.cores = mc.cores,
      return = FALSE,
      ...
    ),
    mc.cores = mc.cores)
}


#' Change Points
#' @description Calculates flow (Q) differences of consecutive time steps and
#'     discriminates between time steps with increasing (IC: \eqn{Q_{ts1} < Q_{ts2}})
#'     and decreasing (DC: \eqn{Q_{ts1} > Q_{ts2}}) flow. The differences are
#'     transformed to 1 or -1 if the number is positive or negative, respectively.
#'     Then the lengths of runs of equal values are computed to determine the
#'     change point indices in the input data frame. \code{NA} in Q values is
#'     replaced with 0 and treated as an equal trend.
#'
#' @param x Data frame (time series) of stage measurements. Data frame must contain
#'     a date-time column (\code{Time}) and a flow rate column (\code{Q}) and must
#'     be in a compatible format. (see \code{\link[=flow]{flow()}}).
#' @param rownames A logical. If \code{FALSE} (default) indices are used to
#'     determine change points, otherwise row names. \code{rownames = TRUE} is
#'     important for data with different IDs.
#' @param omit.constant A logical. If \code{FALSE} (default) it does not return
#'     events with constant measurements. Otherwise these events are included.
#' @param omit.na A logical. If \code{FALSE} (default) it does not return missing
#' value events. Otherwise these events are included.
#' @return A data frame which contains change point indices in the input data frame.
#'     These are the indices where an event starts and ends, and a column which
#'     indicates the event (-1 = decreasing (DC), 1 = increasing (IC)).
#' @keywords internal
change_points <-
  function(x,
           rownames = FALSE,
           omit.constant = TRUE,
           omit.na = TRUE) {
    validate_df(x)

    # differences of consecutive time steps
    change <- diff(x$Q, lag = 1)

    # transform differences to -1 = DC, 1 = IC, 0 = equal
    change <- sign(change)

    # NA values to 0 (no change, treated as equal trend)
    change[is.na(change)] <- 5L

    # start value depends on first difference
    if (!is.na(change[1]) & change[1] == 1) {
      change <- c(1, change)
    } else if (!is.na(change[1]) & change[1] == -1) {
      change <- c(-1, change)
    } else if (!is.na(change[1]) & change[1] == 0) {
      change <- c(0, change)
    } else {
      # NA
      change <- c(5, change)
    }

    # lengths and values of equal values in a vector (length of event)
    rle_x <- rle(change)

    # indices of event start and end
    end <- cumsum(rle_x$lengths)
    start <- c(1, utils::head(end, -1))

    df <- data.frame(start, end, event = rle_x$values)

    df$event <- event_type_internal(df$event)

    if (omit.constant) {
      # remove constant events
      df <- df[df$event != 1 & df$event != 3 & df$event != 0, ]
    }

    if (omit.na) {
      df <- df[df$event != 5, ] # remove constant events
    }

    # pick row names instead of indices (important, if multiple IDs (HZBCODEs) in input data)
    if (rownames) {
      df$start <- as.integer(rownames(x)[df$start])
      df$end <- as.integer(rownames(x)[df$end])
    }

    df
  }


#' Event Type
#' @description Given a data frame (time series) of equal flow (Q) trend, it is
#'     determined whether the flow is increasing or decreasing, constant or if
#'     missing values occur. It returns a numeric value which indicates the event
#'     type. As the event type is already determined when the change points are
#'     computed, this function is mainly used for demonstration purpose or if
#'     metrics should be computed individually.
#'
#' @param x Data frame (time series) from an event with equal flow trend. Data
#'     frame must contain a date-time column (\code{Time}) and a flow rate column
#'     (\code{Q}) and must be in a compatible format. (see \code{\link[=flow]{flow()}}).
#'
#' @return Returns an numeric value which indicates the event type.
#' Event types are defined as follows:
#' \itemize{
#'   \item 0: Constant event after NA event or constant event as first event in time series
#'   \item 1: Constant event after DC
#'   \item 2: Increasing event (IC)
#'   \item 3: Constant event after IC
#'   \item 4: Decreasing event (DC)
#'   \item 5: NA event
#' }
#' @export
#'
#' @examples
#' data(Q)
#' # decreasing event
#' Q4 <- flow(Q[3:4, ])
#' event_type(Q4)
#'
#' # increasing event
#' Q2 <- flow(Q[486:487, ])
#' event_type(Q2)
#'
event_type <- function(x) {
  validate_df(x)

  if (any(is.na(x$Q))) {
    # NA event
    return(5)
  } else if (all(diff(x$Q) > 0)) {
    # IC event
    return(2)
  } else if (all(diff(x$Q) < 0)) {
    # DC event
    return(4)
  } else if (all(diff(x$Q) == 0)) {
    # constant event (no prior knowledge of previous events, therefore 0)
    return(0)
  }
  NA_real_ # invalid event sequence
}

#' Event Type - Internal
#' @description Given a numeric value or a numeric vector, the event type(s) is/are
#'     determined. \code{\link[=change_points]{change_points()}} computes the event
#'     type and assigns either -1 for decreasing events, 1 for increasing events,
#'     5 for NA events, or 0 for constant events. This function returns the correct
#'     labels (integer) for the given event type.
#'
#' @param x Numeric value or numeric vector that indicates the event type.
#' @return Returns a numeric value which indicates the event type.
#' Event types are defined as follows:
#' \itemize{
#'   \item 0: Constant event after NA event or constant event as first event in time series
#'   \item 1: Constant event after DC
#'   \item 2: Increasing event (IC)
#'   \item 3: Constant event after IC
#'   \item 4: Decreasing event (DC)
#'   \item 5: NA event
#' }
#' @keywords internal
#' @noRd
event_type_internal <- function(x) {
  if (length(x) <= 0) {
    return(NA_real_)
  }

  get_event_type <- function(x) {
    if (x == 1) {
      # IC
      2
    } else if (x == -1) {
      # DC
      4
    } else if (x == 5) {
      5
    }
  }

  if (length(x) == 1) {
    if (x == 0) {
      return(0)
    } else {
      return(get_event_type(x))
    }
  }

  # empty vector with predefined length
  types <- vector(mode = "numeric", length = length(x))

  # first element of x
  if (x[1] == 0) {
    # starting with constant event
    types[1] <- 0
  } else {
    types[1] <-  get_event_type(x[1])
  }


  # get event type for all elements of x
  for (i in 2:length(x)) {
    if (x[i] == 0 & x[i - 1] == -1) {
      # constant after DC
      types[i] = 1
    } else if (x[i] == 0 & x[i - 1] == 1) {
      # constant after IC
      types[i] = 3
    } else if (x[i] == 0 & x[i - 1] == 5) {
      # constant after NA event
      types[i] = 0
    } else {
      types[i] = get_event_type(x[i])
    }

  }
  types
}
