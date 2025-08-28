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
  x$launch(n = 1L)
  handle_worker <- unlist(x$launcher$launches$handle)[[1]]
  crew_retry(
    ~ handle_worker$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 15,
  )
  expect_true(handle_worker$is_alive())
  suppressWarnings(crew_clean())
  crew_retry(
    ~ !handle_worker$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 15,
  )
  suppressWarnings(crew_clean())
  expect_false(handle_worker$is_alive())
})
