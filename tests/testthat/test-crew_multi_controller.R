crew_test("crew_multi_controller()", {
  skip_on_cran()
  a <- crew_mirai_controller_callr(name = "a", idle_time = 360)
  b <- crew_mirai_controller_callr(
    name = "b",
    max_tasks = 1L,
    idle_time = 360
  )
  x <- crew_multi_controller(a, b)
  on.exit(x$terminate())
  expect_silent(x$validate())
  expect_false(x$controllers[[1]]$router$is_connected())
  expect_false(x$controllers[[2]]$router$is_connected())
  x$connect()
  expect_null(x$pop())
  x$push(command = ps::ps_pid(), name = "task_pid", controller = "b")
  x$wait(timeout = 5)
  out <- x$pop()
  expect_equal(out$name, "task_pid")
  expect_equal(out$command, "ps::ps_pid()")
  expect_equal(
    out$result[[1]],
    x$controllers[[2]]$launcher$workers[[1]]$get_pid()
  )
  expect_true(is.numeric(out$seconds))
  expect_false(anyNA(out$seconds))
  expect_true(out$seconds >= 0)
  expect_true(anyNA(out$error))
  expect_true(anyNA(out$traceback))
  expect_true(anyNA(out$warnings))
  x$terminate()
  for (index in seq_len(2)) {
    expect_false(x$controllers[[index]]$router$is_connected())
    crew_wait(
      ~(x$controllers[[index]]$launcher$running() == 0L),
      timeout = 5,
      wait = 0.1
    )
  }
})

crew_test("crew_multi_controller() collect", {
  skip_on_cran()
  a <- crew_mirai_controller_callr(name = "a")
  b <- crew_mirai_controller_callr(
    name = "b",
    max_tasks = 1L,
    idle_time = 360
  )
  x <- crew_multi_controller(a, b)
  on.exit(x$terminate())
  expect_silent(x$validate())
  expect_false(x$controllers[[1]]$router$is_connected())
  expect_false(x$controllers[[2]]$router$is_connected())
  x$connect()
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
    x$controllers[[1]]$launcher$workers[[1]]$get_pid()
  )
  expect_false(is.null(out))
})

crew_test("crew_multi_controller() launch method", {
  skip_on_cran()
  a <- crew_mirai_controller_callr(name = "a", idle_time = 360)
  b <- crew_mirai_controller_callr(
    name = "b",
    max_tasks = 1L,
    idle_time = 360
  )
  x <- crew_multi_controller(a, b)
  on.exit(x$terminate())
  x$connect()
  for (index in seq_len(2)) {
    crew_wait(
      ~(x$controllers[[index]]$launcher$running() == 0L),
      timeout = 5,
      wait = 0.1
    )
  }
  expect_silent(x$launch(n = 1L))
  for (index in seq_len(2)) {
    crew_wait(
      ~(x$controllers[[index]]$launcher$running() == 1L),
      timeout = 5,
      wait = 0.1
    )
  }
  x$terminate()
  for (index in seq_len(2)) {
    crew_wait(
      ~(x$controllers[[index]]$launcher$running() == 0L),
      timeout = 5,
      wait = 0.1
    )
  }
})

crew_test("crew_multi_controller() scale method", {
  skip_on_cran()
  a <- crew_mirai_controller_callr(
    name = "a",
    scale_method = "single",
    idle_time = 360
  )
  x <- crew_multi_controller(a)
  on.exit(x$terminate())
  x$connect()
  expect_equal(a$launcher$running(), 0L)
  a$queue <- list("task")
  expect_silent(x$scale())
  crew_wait(
    fun = ~identical(a$launcher$running(), 1L),
    timeout = 5,
    wait = 0.1
  )
  x$terminate()
  crew_wait(
    fun = ~identical(a$launcher$running(), 0L),
    timeout = 5,
    wait = 0.1
  )
})
