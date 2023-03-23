crew_test("crew_controller_callr()", {
  crew_session_start()
  x <- crew_controller_callr(
    workers = 1L,
    seconds_idle = 360
  )
  on.exit({
    x$terminate()
    crew_session_terminate()
    crew_test_sleep()
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
  crew_wait(
    ~x$router$listening(),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  x$push(command = ps::ps_pid(), name = "task_pid")
  x$wait(seconds_timeout = 5)
  # pid task
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
  windows_or_cran <- identical(tolower(Sys.info()[["sysname"]]), "windows") ||
    !identical(Sys.getenv("NOT_CRAN"), "true")
  if (!windows_or_cran) {
    # data task
    expect_false(exists(x = ".crew_y", envir = globalenv()))
    x$push(
      command = paste0("a", x, .crew_y, sample.int(n = 1e9L, size = 1L)),
      data = list(x = "b"),
      globals = list(.crew_y = "c"),
      seed = 0L
    )
    x$wait(seconds_timeout = 5)
    out <- x$pop()
    exp <- withr::with_seed(0L, paste0("abc", sample.int(n = 1e9L, size = 1L)))
    expect_equal(out$result[[1]], exp)
    expect_equal(out$error, NA_character_)
    expect_false(exists(x = ".crew_y", envir = globalenv()))
    # package task
    x$push(
      command = base64enc(arg),
      data = list(arg = "x"),
      packages = "nanonext"
    )
    x$wait(seconds_timeout = 5)
    out <- x$pop()
    expect_equal(out$result[[1]], nanonext::base64enc("x"))
  }
  # terminate
  x$terminate()
  expect_false(x$router$listening())
  crew_wait(
    ~identical(x$launcher$active(), character(0L)),
    seconds_interval = 0.001,
    seconds_timeout = 5,
  )
})

crew_test("crew_controller_callr() warnings and errors", {
  skip_on_cran()
  skip_on_os("windows")
  crew_session_start()
  x <- crew_controller_callr(
    seconds_idle = 360
  )
  on.exit({
    x$terminate()
    crew_session_terminate()
    crew_test_sleep()
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
  x$wait(seconds_timeout = 5)
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
  skip_on_os("windows")
  crew_session_start()
  x <- crew_controller_callr(
    seconds_idle = 360
  )
  on.exit({
    x$terminate()
    crew_session_terminate()
    crew_test_sleep()
  })
  x$start()
  expect_equal(length(x$launcher$active()), 0L)
  x$launch(n = 1L)
  crew_wait(
    ~length(x$launcher$active()) > 0L,
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  x$terminate()
  crew_wait(
    ~length(x$launcher$active()) == 0L,
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
})
