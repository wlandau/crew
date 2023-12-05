crew_test("throttling happens at the right times", {
  x <- crew_throttle(seconds_interval = 4)
  throttled <- 0L
  start <- proc.time()["elapsed"]
  while (proc.time()["elapsed"] - start < 10) {
    throttled <- throttled + x$poll()
  }
  expect_equal(throttled, 3L)
})
