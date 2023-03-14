crew_test("crew_controller_group()", {
  skip_on_cran()
  crew_session_start()
  a <- crew_controller_callr(name = "a", seconds_idle = 360)
  b <- crew_controller_callr(
    name = "b",
    tasks_max = 1L,
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  on.exit({
    x$terminate()
    crew_session_terminate()
  })
  expect_silent(x$validate())
  expect_false(x$controllers[[1]]$router$listening())
  expect_false(x$controllers[[2]]$router$listening())
  x$start()
  expect_null(x$pop())
  x$push(command = ps::ps_pid(), name = "task_pid", controller = "b")
  x$wait(timeout = 5)
  out <- x$pop()
  expect_equal(out$name, "task_pid")
  expect_equal(out$command, "ps::ps_pid()")
  expect_equal(
    out$result[[1]],
    x$controllers[[2]]$launcher$workers$handle[[1]]$get_pid()
  )
  expect_true(is.numeric(out$seconds))
  expect_false(anyNA(out$seconds))
  expect_true(out$seconds >= 0)
  expect_true(anyNA(out$error))
  expect_true(anyNA(out$traceback))
  expect_true(anyNA(out$warnings))
  x$terminate()
  for (index in seq_len(2)) {
    crew_wait(
      ~!x$controllers[[index]]$router$listening(),
      timeout = 5,
      wait = 0.1
    )
    crew_wait(
      ~(length(x$controllers[[index]]$launcher$active()) == 0L),
      timeout = 5,
      wait = 0.1
    )
  }
})

crew_test("crew_controller_group() collect", {
  skip_on_cran()
  crew_session_start()
  a <- crew_controller_callr(name = "a")
  b <- crew_controller_callr(
    name = "b",
    tasks_max = 1L,
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  on.exit({
    x$terminate()
    crew_session_terminate()
  })
  expect_silent(x$validate())
  expect_false(x$controllers[[1]]$router$listening())
  expect_false(x$controllers[[2]]$router$listening())
  x$start()
  expect_null(x$pop())
  x$push(command = ps::ps_pid(), name = "task_pid")
  crew_wait(
    fun = ~{
      x$collect()
      length(x$controllers[[1]]$results) > 0L
    }
  )
  out <- x$pop(collect = FALSE, controllers = "a")
  expect_equal(
    out$result[[1]],
    x$controllers[[1]]$launcher$workers$handle[[1]]$get_pid()
  )
  expect_false(is.null(out))
})

crew_test("crew_controller_group() launch method", {
  skip_on_cran()
  crew_session_start()
  a <- crew_controller_callr(name = "a", seconds_idle = 360)
  b <- crew_controller_callr(
    name = "b",
    tasks_max = 1L,
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  on.exit({
    x$terminate()
    crew_session_terminate()
  })
  x$start()
  for (index in seq_len(2)) {
    crew_wait(
      ~(length(x$controllers[[index]]$launcher$active()) == 0L),
      timeout = 5,
      wait = 0.1
    )
  }
  expect_silent(x$launch(n = 1L))
  for (index in seq_len(2)) {
    crew_wait(
      ~(length(x$controllers[[index]]$launcher$active()) == 1L),
      timeout = 5,
      wait = 0.1
    )
  }
  x$terminate()
  for (index in seq_len(2)) {
    crew_wait(
      ~(length(x$controllers[[index]]$launcher$active()) == 0L),
      timeout = 5,
      wait = 0.1
    )
  }
})

crew_test("crew_controller_group() scale method", {
  skip_on_cran()
  crew_session_start()
  a <- crew_controller_callr(
    name = "a",
    auto_scale = "single",
    seconds_idle = 360
  )
  x <- crew_controller_group(a)
  on.exit({
    x$terminate()
    crew_session_terminate()
  })
  x$start()
  expect_equal(a$launcher$active(), character(0L))
  a$queue <- list("task")
  expect_silent(x$scale())
  crew_wait(
    fun = ~identical(length(a$launcher$active()), 1L),
    timeout = 5,
    wait = 0.1
  )
  x$terminate()
  crew_wait(
    fun = ~identical(length(a$launcher$active()), 0L),
    timeout = 5,
    wait = 0.1
  )
})
