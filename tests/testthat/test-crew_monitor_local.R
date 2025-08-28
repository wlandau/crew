crew_test("detect dispatchers", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_monitor_local()
  pids <- x$dispatchers()
  expect_true(is.integer(pids))
  mirai::daemons(n = 1L, dispatcher = TRUE)
  pids <- x$dispatchers()
  expect_true(is.integer(pids))
  expect_gt(length(pids), 0L)
  on.exit({
    mirai::daemons(n = 0L)
    gc()
    crew_test_sleep()
  })
})

crew_test("detect daemons", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_monitor_local()
  expect_true(is.integer(x$daemons()))
  first <- length(x$daemons())
  mirai::daemons(n = 1L)
  on.exit({
    mirai::daemons(n = 0L)
    gc()
    crew_test_sleep()
  })
  crew::crew_retry(~ length(x$daemons()) > first, seconds_interval = 0.01)
  expect_true(is.integer(x$daemons()))
  expect_true(length(x$daemons()) > first)
})

crew_test("detect workers", {
  skip_on_cran()
  skip_on_covr() # corrupt RDS files on covr
  skip_on_os("windows")
  x <- crew_monitor_local()
  expect_true(is.integer(x$workers()))
  controller <- crew_controller_local()
  first <- length(x$workers())
  on.exit({
    controller$terminate()
    rm(controller)
    gc()
    crew_test_sleep()
  })
  controller$start()
  controller$launch(n = 1L)
  crew::crew_retry(~ length(x$workers()) > first, seconds_interval = 0.01)
  expect_true(is.integer(x$workers()))
  expect_true(length(x$workers()) > first)
})

crew_test("terminate a process", {
  skip_on_cran()
  skip_on_covr() # corrupt RDS files on covr
  skip_on_os("windows")
  monitor <- crew_monitor_local()
  controller <- crew_controller_local()
  on.exit({
    controller$terminate()
    rm(controller)
    gc()
    crew_test_sleep()
  })
  controller$start()
  controller$launch()
  handle <- unlist(controller$launcher$launches$handle)[[1L]]
  crew_retry(
    ~ handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 30
  )
  expect_true(handle$is_alive())
  monitor$terminate(handle$get_pid())
  crew_retry(
    ~ !handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 30
  )
  expect_false(handle$is_alive())
})
