crew_test("async validate when not started", {
  expect_silent(crew_async(workers = NULL)$validate())
  expect_silent(crew_async(workers = 57L)$validate())
})

crew_test("async validate when started", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_async(workers = 1L)
  on.exit(x$terminate())
  x$start()
  expect_silent(x$validate())
  x$terminate()
  x$validate()
})

crew_test("async object task with NULL workers", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_async(workers = NULL)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  expect_null(x$workers)
  expect_null(x$instance)
  x$start()
  expect_null(x$workers)
  expect_null(x$instance)
  out <- x$eval(
    command = list(pid = ps::ps_pid(), x = x),
    data = list(x = "value"),
    packages = "rlang"
  )
  expect_equal(out$data$x, "value")
  expect_equal(out$data$pid, ps::ps_pid())
})

crew_test("async task with 1 process", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_async(workers = 1L)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  expect_equal(x$workers, 1L)
  expect_null(x$instance)
  x$start()
  expect_equal(x$workers, 1L)
  expect_true(nzchar(x$instance))
  out <- x$eval(
    command = list(pid = ps::ps_pid(), x = x),
    data = list(x = "value"),
    packages = "rlang"
  )
  crew_retry(
    fun = ~!nanonext::unresolved(out),
    seconds_interval = 0.01,
    seconds_timeout = 10
  )
  expect_equal(out$data$x, "value")
  expect_true(is.numeric(out$data$pid))
  expect_false(any(out$data$pid == ps::ps_pid()))
})
