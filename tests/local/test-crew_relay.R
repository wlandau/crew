crew_test("relay wait() timeout condition", {
  throttle <- crew_throttle(
    seconds_max = 2,
    seconds_min = 2,
    seconds_start = 2,
    base = 1.1
  )
  x <- crew_relay(throttle = throttle)
  cv <- nanonext::cv()
  x$set_from(cv)
  x$start()
  time <- system.time(out <- x$wait())
  expect_false(out)
  expect_gt(time["elapsed"], 1)
  expect_equal(nanonext::cv_value(x$condition), 0L)
  expect_equal(nanonext::cv_value(cv), 0L)
})
