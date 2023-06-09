test_that("schedule validate empty", {
  x <- crew_schedule()
  expect_silent(x$validate())
})

test_that("schedule validate full", {
  x <- crew_schedule()
  x$start()
  x$head <- "id"
  x$until <- nanonext::mclock()
  expect_silent(x$validate())
})

test_that("schedule start", {
  x <- crew_schedule()
  expect_null(x$pushed)
  expect_null(x$collected)
  x$start()
  expect_true(is.environment(x$pushed))
  expect_true(is.environment(x$collected))
})

test_that("schedule push", {
  x <- crew_schedule()
  x$start()
  expect_equal(length(x$pushed), 0L)
  expect_equal(length(x$collected), 0L)
  expect_null(x$head)
  expect_null(x$until)
  x$push(task = crew_null, id = "1")
  expect_equal(length(x$pushed), 1L)
  expect_equal(length(x$collected), 0L)
  expect_null(x$head)
  expect_null(x$until)
  expect_true(is_crew_null(x$pushed[["1"]]))
})

test_that("schedule collect without throttling", {
  x <- crew_schedule()
  x$start()
  x$push(task = crew_null, id = "1")
  x$collect(throttle = FALSE)
  expect_equal(length(x$pushed), 0L)
  expect_equal(length(x$collected), 1L)
  expect_true(is_crew_null(x$collected[["1"]]))
  expect_null(x$until)
  expect_equal(x$head, "1")
})
