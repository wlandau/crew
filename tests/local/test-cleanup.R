crew_test("workers signal themselves to exit when connection breaks", {
  skip_if_not_installed("mirai", minimum_version = "0.11.3.9000")
  skip_if_not_installed("nanonext", minimum_version = "0.11.0.9000")
  x <- crew_controller_local(workers = 1L)
  x$start()
  x$push(nanonext::until(nanonext::cv(), 600 * 1e3))
  Sys.sleep(2)
  x$scale()
  Sys.sleep(2)
  dispatcher <- ps::ps_handle(pid = x$client$dispatcher)
  crew_terminate_process(x$client$dispatcher)
  crew_retry(
    ~!ps::ps_is_running(dispatcher),
    seconds_interval = 0.5,
    seconds_timeout = 60
  )
  Sys.sleep(5)
  worker <- x$launcher$workers$handle[[1L]]
  on.exit(worker$signal(signal = crew_terminate_signal()))
  expect_false(worker$is_alive())
})
