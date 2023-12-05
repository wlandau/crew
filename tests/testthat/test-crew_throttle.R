test_that("crew_throttle validation", {
  expect_silent(crew_throttle()$validate())
  expect_silent(crew_throttle(seconds_interval = 5)$validate())
  expect_crew_error(crew_throttle(seconds_interval = -1))
})

test_that("crew_throttle poll() and reset()", {
  x <- crew_throttle(seconds_interval = 300)
  expect_true(x$poll())
  expect_false(x$poll())
  expect_silent(x$reset())
  expect_true(x$poll())
})

test_that("crew_throttle active bindings", {
  x <- crew_throttle(seconds_interval = 300)
  expect_equal(x$seconds_interval, 300)
  expect_null(x$polled)
  expect_true(x$poll())
  expect_true(is.numeric(x$polled))
})
