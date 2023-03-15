crew_test("crew_controller_callr()", {
  crew_session_start()
  x <- crew_controller_callr(
    workers = 1L,
    seconds_idle = 360,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  on.exit({
    x$terminate()
    crew_session_terminate()
  })
  expect_silent(x$validate())
  expect_false(x$router$listening())
  expect_null(x$summary())
  x$start()
  s <- x$summary()
  expect_true(is.data.frame(s))
  expect_equal(nrow(s), 1L)
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
  expect_equal(s$popped_tasks, 0L)
  crew_wait(~x$router$listening(), timeout = 5, wait = 0.1)
  x$push(command = ps::ps_pid(), name = "task_pid")
  x$wait(timeout = 5)
  out <- x$pop(scale = TRUE)
  expect_equal(x$summary()$popped_tasks, 1L)
  expect_equal(x$summary()$popped_errors, 0L)
  expect_equal(x$summary()$popped_warnings, 0L)
  expect_equal(out$name, "task_pid")
  expect_equal(out$command, "ps::ps_pid()")
  expect_equal(out$result[[1]], x$launcher$workers$handle[[1]]$get_pid())
  expect_true(is.numeric(out$seconds))
  expect_false(anyNA(out$seconds))
  expect_true(out$seconds >= 0)
  expect_true(anyNA(out$error))
  expect_true(anyNA(out$traceback))
  expect_true(anyNA(out$warnings))
  x$terminate()
  expect_false(x$router$listening())
  expect_equal(x$launcher$active(), character(0L))
})

crew_test("crew_controller_callr() warnings and errors", {
  skip_on_cran()
  crew_session_start()
  x <- crew_controller_callr(
    seconds_idle = 360,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  on.exit({
    x$terminate()
    crew_session_terminate()
  })
  expect_silent(x$validate())
  expect_false(x$router$listening())
  x$start()
  expect_equal(x$summary()$popped_tasks, 0L)
  expect_equal(x$summary()$popped_errors, 0L)
  expect_equal(x$summary()$popped_warnings, 0L)
  x$push(command = {
    warning("this is a warning")
    stop("this is an error")
  }, name = "warnings_and_errors")
  x$wait(timeout = 5)
  out <- x$pop(scale = FALSE)
  expect_equal(x$summary()$popped_tasks, 1L)
  expect_equal(x$summary()$popped_errors, 1L)
  expect_equal(x$summary()$popped_warnings, 1L)
  expect_equal(out$name, "warnings_and_errors")
  expect_true(is.numeric(out$seconds))
  expect_false(anyNA(out$seconds))
  expect_true(out$seconds >= 0)
  expect_equal(out$error, "this is an error")
  expect_equal(out$warnings, "this is a warning")
  expect_false(anyNA(out$traceback))
  x$terminate()
  expect_false(x$router$listening())
  expect_equal(x$launcher$active(), character(0L))
})

crew_test("crew_controller_callr() launch method", {
  skip_on_cran()
  crew_session_start()
  x <- crew_controller_callr(
    seconds_idle = 360,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  on.exit({
    x$terminate()
    crew_session_terminate()
  })
  x$start()
  expect_equal(length(x$launcher$active()), 0L)
  x$launch(n = 1L)
  crew_wait(~length(x$launcher$active()) > 0L, wait = 0.1, timeout = 5)
  x$terminate()
  crew_wait(~length(x$launcher$active()) == 0L, wait = 0.1, timeout = 5)
})

crew_test("crew_controller_callr() collect method", {
  skip_on_cran()
  crew_session_start()
  x <- crew_controller_callr(
    workers = 1L,
    seconds_idle = 360,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  on.exit({
    x$terminate()
    crew_session_terminate()
  })
  expect_silent(x$validate())
  expect_false(x$router$listening())
  x$start()
  crew_wait(~x$router$listening(), timeout = 5, wait = 0.1)
  x$launch(n = 1L)
  for (index in seq_len(4L)) {
    x$push(command = quote(1L), name = "task_pid", scale = FALSE)
  }
  crew_wait(
    fun = ~{
      for (task in x$queue) {
        if (mirai::unresolved(task$handle[[1]])) {
          return(FALSE)
        }
      }
      TRUE
    },
    wait = 0.01,
    timeout = 5
  )
  expect_equal(length(x$queue), 4L)
  expect_equal(length(x$results), 0L)
  x$collect(n = 2L)
  expect_equal(length(x$queue), 2L)
  expect_equal(length(x$results), 2L)
  for (index in seq_len(4L)) {
    expect_equal(x$pop()$result[[1L]], 1L)
  }
})
