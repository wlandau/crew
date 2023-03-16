crew_test("crew_controller_group()", {
  skip_on_cran()
  crew_session_start()
  a <- crew_controller_callr(
    name = "a",
    seconds_idle = 360,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  b <- crew_controller_callr(
    name = "b",
    tasks_max = 1L,
    seconds_idle = 360,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  x <- crew_controller_group(a, b)
  expect_null(x$summary())
  on.exit({
    x$terminate()
    crew_session_terminate()
  })
  expect_silent(x$validate())
  expect_false(x$controllers[[1]]$router$listening())
  expect_false(x$controllers[[2]]$router$listening())
  x$start()
  s <- x$summary()
  expect_equal(nrow(s), 2L)
  expect_equal(s$controller, c("a", "b"))
  expect_equal(
    sort(colnames(s)),
    sort(
      c(
        "controller",
        "worker_socket",
        "worker_connected",
        "worker_busy",
        "worker_launches",
        "worker_instances",
        "tasks_assigned",
        "tasks_complete",
        "popped_tasks",
        "popped_seconds",
        "popped_errors",
        "popped_warnings"
      )
    )
  )
  s2 <- x$summary(columns = tidyselect::starts_with("popped"))
  expect_equal(
    sort(colnames(s2)),
    sort(
      c(
        "popped_tasks",
        "popped_seconds",
        "popped_errors",
        "popped_warnings"
      )
    )
  )
  expect_null(x$pop())
  x$push(command = ps::ps_pid(), name = "task_pid", controller = "b")
  x$wait(timeout = 5)
  out <- x$pop(scale = FALSE)
  expect_false(is.null(out))
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

crew_test("crew_controller_group() tidyselect", {
  skip_on_cran()
  crew_session_start()
  a <- crew_controller_callr(
    name = "a",
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  b <- crew_controller_callr(
    name = "b",
    tasks_max = 1L,
    seconds_idle = 360,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  x <- crew_controller_group(a, b)
  on.exit({
    x$terminate()
    crew_session_terminate()
  })
  expect_false(a$router$listening())
  expect_false(b$router$listening())
  x$start(controllers = tidyselect::contains("b"))
  expect_false(a$router$listening())
  expect_true(b$router$listening())
  x$terminate(controllers = tidyselect::contains("b"))
  expect_false(a$router$listening())
  expect_false(b$router$listening())
})


crew_test("crew_controller_group() collect", {
  skip_on_cran()
  crew_session_start()
  a <- crew_controller_callr(
    name = "a",
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  b <- crew_controller_callr(
    name = "b",
    tasks_max = 1L,
    seconds_idle = 360,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
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
  out <- x$pop(scale = FALSE, controllers = "a")
  expect_equal(
    out$result[[1]],
    x$controllers[[1]]$launcher$workers$handle[[1]]$get_pid()
  )
  expect_false(is.null(out))
})

crew_test("crew_controller_group() launch method", {
  skip_on_cran()
  crew_session_start()
  a <- crew_controller_callr(
    name = "a",
    seconds_idle = 360,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  b <- crew_controller_callr(
    name = "b",
    tasks_max = 1L,
    seconds_idle = 360,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
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
    auto_scale = "one",
    seconds_idle = 360,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
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
