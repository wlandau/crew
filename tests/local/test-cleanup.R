crew_test("workers signal themselves to exit when connection breaks", {
  x <- crew_controller_local(workers = 1L)
  x$start()
  x$push(
    nanonext::until(nanonext::cv(), 600 * 1e3),
    scale = TRUE,
    throttle = FALSE
  )
  worker <- unlist(x$launcher$launches$handle)[[1L]]
  crew_retry(
    ~ worker$is_alive(),
    seconds_interval = 0.5,
    seconds_timeout = 60
  )
  crew_retry(
    ~ x$client$status()["connections"] > 0L,
    seconds_interval = 0.5,
    seconds_timeout = 60
  )
  expect_true(worker$is_alive())
  mirai::daemons(n = 0L, .compute = x$client$profile)
  crew_retry(
    ~ !worker$is_alive(),
    seconds_interval = 0.5,
    seconds_timeout = 60
  )
  expect_false(worker$is_alive())
})
