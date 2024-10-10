crew_test("monitor dispatchers", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_monitor_local()
  expect_true(is.integer(x$dispatchers()))
  mirai::daemons(n = 1L, dispatcher = "process")
  on.exit({
    mirai::daemons(n = 0L)
    gc()
    crew_test_sleep()
  })
  pid <- as.integer(nextget("pid"))
  crew_retry(
    ~inherits(ps::ps_handle(pid = pid), "ps_handle"),
    seconds_interval = 0.01
  )
  handle <- ps::ps_handle(pid = pid)
  expect_true(ps::ps_is_running(handle))
  expect_true(pid %in% x$dispatchers())
  expect_silent(x$terminate(pid))
  crew::crew_retry(~!ps::ps_is_running(handle), seconds_interval = 0.01)
  expect_true(is.integer(x$dispatchers()))
  expect_false(ps::ps_is_running(handle))
})

crew_test("monitor daemons", {
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
  crew::crew_retry(~length(x$daemons()) > first, seconds_interval = 0.01)
  expect_true(is.integer(x$daemons()))
  expect_true(length(x$daemons()) > first)
})

crew_test("monitor workers", {
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
  crew::crew_retry(~length(x$workers()) > first, seconds_interval = 0.01)
  expect_true(is.integer(x$workers()))
  expect_true(length(x$workers()) > first)
})
