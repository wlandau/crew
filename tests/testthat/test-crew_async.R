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
  x <- crew_async(workers = NULL)
  on.exit(x$terminate())
  x$start()
  out <- x$eval(
    command = c(ps::ps_pid(), x),
    args = list(x = "value"),
    packages = "rlang"
  )
  expect_equal(out$data, c(ps::ps_pid(), "value"))
})

crew_test("async task with 1 process", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_async(workers = 1L)
  on.exit(x$terminate())
  x$start()
  expect_equal(x$errors(), 0L)
  out <- x$eval(
    command = list(pid = ps::ps_pid(), x = x),
    args = list(x = "value"),
    packages = "rlang"
  )
  crew_retry(
    fun = ~!nanonext::unresolved(out),
    seconds_interval = 0.01,
    seconds_timeout = 30
  )
  expect_true(is.numeric(out$data$pid))
  expect_false(any(out$data$pid == ps::ps_pid()))
  expect_equal(out$data$x, "value")
  expect_equal(x$errors(), 0L)
})

crew_test("async object error handling utils", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_async(workers = 1L)
  on.exit(x$terminate())
  x$start()
  expect_error(
    crew_eval_async(
      command = quote(stop("error message")),
      args = list(),
      url = x$url
    )
  )
  crew_retry(
    fun = ~all(x$errors(), 1L),
    seconds_interval = 0.01,
    seconds_timeout = 5
  )
})

crew_test("async object errors in task", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_async(workers = 1L)
  on.exit(x$terminate())
  x$start()
  expect_equal(x$errors(), 0L)
  task1 <- x$eval(
    command = stop("error message"),
    args = list(x = "value"),
    packages = "rlang"
  )
  crew_retry(
    fun = ~!nanonext::unresolved(task1),
    seconds_interval = 0.01,
    seconds_timeout = 30
  )
  crew_retry(
    fun = ~all(x$errors(), 1L),
    seconds_interval = 0.01,
    seconds_timeout = 5
  )
  task2 <- x$eval(
    command = "good answer",
    args = list(x = "value"),
    packages = "this package does not exist"
  )
  crew_retry(
    fun = ~!nanonext::unresolved(task2),
    seconds_interval = 0.01,
    seconds_timeout = 30
  )
  crew_retry(
    fun = ~all(x$errors(), 2L),
    seconds_interval = 0.01,
    seconds_timeout = 5
  )
  x$reset()
  expect_equal(x$errors(), 0L)
})
