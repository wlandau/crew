crew_test("workers signal themselves to exit when connection breaks", {
  x <- crew_controller_local(workers = 1L)
  x$start()
  x$push(nanonext::until(nanonext::cv(), 600 * 1e3))
  Sys.sleep(2)
  x$scale()
  Sys.sleep(2)
  dispatcher <- ps::ps_handle(pid = x$client$dispatcher)
  ps::ps_kill(dispatcher)
  crew_retry(
    ~!ps::ps_is_running(dispatcher),
    seconds_interval = 0.5,
    seconds_timeout = 60
  )
  Sys.sleep(5)
  worker <- x$launcher$workers$handle[[1L]]
  on.exit(worker$kill())
  expect_false(worker$is_alive())
})
