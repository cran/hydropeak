## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library("hydropeak")

## -----------------------------------------------------------------------------
dim(Q)
head(Q)

## -----------------------------------------------------------------------------
dim(Events)
head(Events)

## -----------------------------------------------------------------------------
result <- get_events(Q, omit.constant = FALSE, omit.na = FALSE)
head(result)

all.equal(Events, result)

## -----------------------------------------------------------------------------
Q_file <- system.file("extdata", "Q.csv", package = "hydropeak")
outdir <- file.path(tempdir(), "Events1")

events <- get_events_file(Q_file, inputsep = ",", inputdec = ".", 
                          save = TRUE, split = TRUE, return = TRUE,
                          outdir = outdir)
head(events)

## -----------------------------------------------------------------------------
Q_dir <- system.file("extdata", package = "hydropeak")
outdir <- file.path(tempdir(), "Events2")

get_events_dir(Q_dir, inputsep = ",", inputdec = ".", outdir = outdir)
list.files(outdir)

## -----------------------------------------------------------------------------
Q_event <- Q[3:4, ]
Q_event # decreasing event by 0.001 m^3/s within 15 minutes

## -----------------------------------------------------------------------------
get_events(Q_event)

## -----------------------------------------------------------------------------
Q_event <- flow(Q_event)
Q_event

## -----------------------------------------------------------------------------
amp(Q_event)

## -----------------------------------------------------------------------------
mafr(Q_event)

## -----------------------------------------------------------------------------
mefr(Q_event)

## -----------------------------------------------------------------------------
dur(Q_event)

## -----------------------------------------------------------------------------
ratio(Q_event, event_type = event_type(Q_event))

