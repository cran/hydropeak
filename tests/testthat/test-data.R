test_that("Q from inst/extdata equals Q from data/ ",{
  Q_file <- system.file("extdata", "Q.csv", package = "hydropeak")
  Q_file <- read.csv(Q_file)
  Q_file$ID <- as.character(Q_file$ID)

  data(Q)
  expect_equal(Q_file, Q)
})

test_that("Events from inst/extdata equals Events from data/ ",{
  Events_file <- system.file("extdata", "Events.csv", package = "hydropeak")
  Events_file <- read.csv(Events_file)
  Events_file$ID <- as.character(Events_file$ID)

  tz <- "Etc/GMT-1"
  Events_file$Time <- as.POSIXct(Events_file$Time, tz = tz)

  # coerce row names to character
  attr(Events_file, "row.names") <- as.character(attr(Events_file, "row.names"))

  data(Events)
  expect_equal(Events_file, Events)
})
