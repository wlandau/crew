crew_test("crew_throttle validation", {
  expect_silent(crew_throttle()$validate())
  expect_silent(crew_throttle(seconds_max = 5)$validate())
  expect_crew_error(crew_throttle(seconds_max = -1))
})

crew_test("crew_throttle active bindings", {
  x <- crew_throttle(
    seconds_max = 300,
    seconds_min = 0.1,
    seconds_start = 10,
    base = 2
  )
  expect_equal(x$seconds_max, 300)
  expect_equal(x$seconds_min, 0.1)
  expect_equal(x$seconds_start, 10)
  expect_equal(x$base, 2)
  expect_equal(x$seconds_interval, 10)
  expect_false(is.finite(x$polled))
  expect_true(x$poll())
  expect_true(is.numeric(x$polled))
})

crew_test("crew_throttle poll() and reset()", {
  x <- crew_throttle(
    seconds_max = 300,
    seconds_min = 0.1,
    seconds_start = 10,
    base = 2
  )
  expect_true(x$poll())
  expect_false(x$poll())
  expect_silent(x$reset())
  expect_equal(x$seconds_interval, 10)
  expect_true(x$poll())
})

crew_test("crew_throttle accelerate()", {
  x <- crew_throttle(
    seconds_max = 25,
    seconds_min = 3,
    seconds_start = 8,
    base = 2
  )
  expect_equal(x$seconds_interval, 8)
  x$accelerate()
  expect_equal(x$seconds_interval, 4)
  for (index in seq_len(10L)) {
    x$accelerate()
    expect_equal(x$seconds_interval, 3)
  }
  x$reset()
  expect_equal(x$seconds_interval, 8)
})

crew_test("crew_throttle decelerate()", {
  x <- crew_throttle(
    seconds_max = 25,
    seconds_min = 1,
    seconds_start = 10,
    base = 2
  )
  expect_equal(x$seconds_interval, 10)
  x$decelerate()
  expect_equal(x$seconds_interval, 20)
  for (index in seq_len(10L)) {
    x$decelerate()
    expect_equal(x$seconds_interval, 25)
  }
  x$reset()
  expect_equal(x$seconds_interval, 10)
})

crew_test("crew_throttle update()", {
  x <- crew_throttle(
    seconds_max = 25,
    seconds_min = 1,
    seconds_start = 10,
    base = 2
  )
  expect_equal(x$seconds_interval, 10)
  expect_false(is.finite(x$polled))
  x$update(activity = FALSE)
  expect_equal(x$seconds_interval, 20)
  expect_false(is.finite(x$polled))
  expect_true(x$poll())
  expect_true(is.finite(x$polled))
  x$update(activity = FALSE)
  expect_equal(x$seconds_interval, 25)
  expect_true(is.finite(x$polled))
  x$update(activity = TRUE)
  expect_equal(x$seconds_interval, 10)
  expect_false(is.finite(x$polled))
})
