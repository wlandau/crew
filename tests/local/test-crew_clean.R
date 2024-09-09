crew_test("crew_clean()", {
  skip_on_cran()
  x <- crew_controller_local(
    workers = 1L,
    seconds_idle = 360
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  handle_dispatcher <- x$client$dispatcher
  expect_true(ps::ps_is_running(handle_dispatcher))
  x$launch(n = 1L)
  handle_worker <- x$launcher$workers$handle[[1]]
  crew_retry(
    ~handle_worker$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 15,
  )
  expect_message(crew_clean())
  crew_retry(
    ~!ps::ps_is_running(handle_dispatcher),
    seconds_interval = 0.1,
    seconds_timeout = 15,
  )
  crew_retry(
    ~!handle_worker$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 15,
  )
  expect_message(crew_clean())
})
