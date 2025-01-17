crew_test("throttle$poll() returns the right values at the right times", {
  x <- crew_throttle(seconds_max = 4, seconds_min = 4, seconds_start = 4)
  throttled <- 0L
  start <- proc.time()["elapsed"]
  while (proc.time()["elapsed"] - start < 10) {
    throttled <- throttled + x$poll()
  }
  expect_equal(throttled, 3L)
})
