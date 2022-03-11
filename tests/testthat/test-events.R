data("Q", package = "hydropeak")
data("Events", package = "hydropeak") # expected data (original event output from SQL)

test_that("metrics are computed correctly (start with DC event)", {
  testEvents <-
    Events[Events$EVENT_TYPE == 2 |
             Events$EVENT_TYPE == 4,] # only IC and DC
  testEvents <- testEvents[90,]

  testQ <- Q[628:634,]

  expect_equal(get_events(testQ), testEvents, ignore_attr = TRUE)
})


test_that("metrics are computed correctly (gauging station ID change)", {
  testEvents <-
    Events[Events$EVENT_TYPE == 2 |
             Events$EVENT_TYPE == 4,] # only IC and DC
  testEvents <- testEvents[50:51,]

  testQ <- Q[477:485,]

  expect_equal(get_events(testQ), testEvents, ignore_attr = TRUE)
})


test_that("change points are computed correctly", {
  testQ <- Q[530:539,]
  testQ <- flow(testQ)

  result <- data.frame(
    start = c(2, 4, 6, 8),
    end = c(4, 6, 8, 9),
    event = c(4, 2, 4, 2)
  )
  # attr(result, "row.names") <- 2L:5L

  expect_equal(change_points(testQ), result, ignore_attr = TRUE)
})

test_that("change points are computed correctly including constant event", {
  testQ <- Q[530:539,]
  testQ <- flow(testQ)

  result <- data.frame(
    start = c(1, 2, 4, 6, 8, 9),
    end = c(2, 4, 6, 8, 9, 10),
    event = c(0, 4, 2, 4, 2, 3)
  )
  #attr(result, "row.names") <- 1L:6L

  expect_equal(change_points(testQ, omit.constant = FALSE),
               result,
               ignore_attr = TRUE)
})

test_that("change points are computed correctly including constant and na event",
          {
            testQ <- Q[1:14,]
            testQ <- flow(testQ)

            result <- data.frame(
              start = c(1, 3, 4, 7, 12, 13),
              end = c(3, 4, 7, 12, 13, 14),
              event = c(0, 4, 1, 5, 0, 4)
            )

            expect_equal(change_points(testQ, omit.constant = FALSE, omit.na = FALSE),
                         result,
                         ignore_attr = TRUE)
          })


test_that("assigned DC event", {
  testQ <- Q[3:4,]
  testQ <- flow(testQ)

  expect_equal(event_type(testQ), 4)
})


test_that("assigned IC event", {
  testQ <- Q[486:487,]
  testQ <- flow(testQ)

  expect_equal(event_type(testQ), 2)
})


test_that("assigned constant event 0", {
  testQ <- Q[1:3,]
  testQ <- flow(testQ)

  expect_equal(event_type(testQ), 0)
})

test_that("assigned NA event 5", {
  testQ <- Q[7:12,]
  testQ <- flow(testQ)

  expect_equal(event_type(testQ), 5)
})


test_that("output type of get_events() is a list (data.frame)", {
  expect_type(get_events(Q[1:10,]), "list")
})


test_that("column names of output of get_events() equal specific names", {
  expect_named(get_events(Q[1:10,]),
               c(
                 "ID",
                 "EVENT_TYPE",
                 "Time",
                 "AMP",
                 "MAFR",
                 "MEFR",
                 "DUR",
                 "RATIO"
               ))
})


test_that("no duplicates in time series", {
  testEvents <- get_events(Q)
  expect_false(any(duplicated(testEvents[, c("ID", "Time")])))
})
