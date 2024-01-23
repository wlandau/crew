test_that("backlogged crashed workers relaunch", {
  controller <- crew_controller_local(workers = 2L, seconds_idle = Inf)
  on.exit(controller$terminate())
  controller$start()
  controller$push(TRUE)
  controller$push(Sys.sleep(10))
  controller$scale(throttle = FALSE)
  for (handle in controller$launcher$workers$handle) {
    crew_retry(
      ~handle$is_alive(),
      seconds_interval = 0.01,
      seconds_timeout = 60
    )
  }
  controller$wait(mode = "one")
  for (handle in controller$launcher$workers$handle) {
    handle$kill()
  }
  for (handle in controller$launcher$workers$handle) {
    crew_retry(
      ~!handle$is_alive(),
      seconds_interval = 0.01,
      seconds_timeout = 60
    )
  }
  expect_true(controller$wait(seconds_timeout = 20L))
})
