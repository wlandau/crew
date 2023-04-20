crew_test("crew_controller_local()", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    workers = 1L,
    seconds_idle = 360
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  expect_silent(x$validate())
  expect_false(x$router$listening())
  expect_null(x$summary())
  x$start()
  expect_true(x$empty())
  crew_retry(
    ~{
      x$wait(seconds_timeout = 30)
      TRUE
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  s <- x$summary()
  expect_true(is.data.frame(s))
  expect_equal(nrow(s), 1L)
  expect_equal(
    sort(colnames(s)),
    sort(
      c(
        "controller",
        "popped_tasks",
        "popped_seconds",
        "popped_errors",
        "popped_warnings",
        "tasks_assigned",
        "tasks_complete",
        "worker_connected",
        "worker_launches",
        "worker_instances",
        "worker_socket"
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
  crew_retry(
    ~x$router$listening(),
    seconds_interval = 0.01,
    seconds_timeout = 10
  )
  instance <- parse_instance(rownames(x$router$daemons))
  x$push(command = Sys.getenv("CREW_INSTANCE"), name = "task")
  expect_false(x$empty())
  x$wait(seconds_timeout = 5)
  expect_false(x$empty())
  # first task
  envir <- new.env(parent = emptyenv())
  crew_retry(
    ~{
      envir$out <- x$pop(scale = TRUE)
      !is.null(envir$out)
    },
    seconds_interval = 0.01,
    seconds_timeout = 10
  )
  out <- envir$out
  expect_true(x$empty())
  expect_equal(x$summary()$popped_tasks, 1L)
  expect_equal(x$summary()$popped_errors, 0L)
  expect_equal(x$summary()$popped_warnings, 0L)
  expect_equal(out$name, "task")
  expect_equal(out$command, "Sys.getenv(\"CREW_INSTANCE\")")
  expect_equal(out$result[[1]], instance)
  expect_false(any(instance == Sys.getenv("CREW_INSTANCE")))
  expect_true(is.numeric(out$seconds))
  expect_false(anyNA(out$seconds))
  expect_true(out$seconds >= 0)
  expect_true(anyNA(out$error))
  expect_true(anyNA(out$trace))
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
  handle <- x$launcher$workers$handle[[1]]
  x$terminate()
  expect_false(x$router$listening())
  crew_retry(
    ~!handle$is_alive(),
    seconds_interval = 0.001,
    seconds_timeout = 5,
  )
})

crew_test("crew_controller_local() substitute = FALSE", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(seconds_idle = 360)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  expect_silent(x$validate())
  expect_false(x$router$listening())
  x$start()
  expect_equal(x$summary()$popped_tasks, 0L)
  expect_equal(x$summary()$popped_errors, 0L)
  expect_equal(x$summary()$popped_warnings, 0L)
  command <- quote(sqrt(4L) + sqrt(9L))
  x$push(command = command, substitute = FALSE, name = "substitute")
  x$wait(seconds_timeout = 10)
  out <- x$pop(scale = FALSE)
  expect_equal(out$result[[1]], 5L)
  expect_equal(out$name, "substitute")
  expect_true(is.numeric(out$seconds))
  expect_false(anyNA(out$seconds))
  expect_true(out$seconds >= 0)
  expect_true(anyNA(out$error))
  expect_true(anyNA(out$warnings))
  expect_true(anyNA(out$trace))
  handle <- x$launcher$workers$handle[[1]]
  x$terminate()
  expect_false(x$router$listening())
  crew_retry(
    ~!handle$is_alive(),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
})

crew_test("crew_controller_local() warnings and errors", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(seconds_idle = 360)
  on.exit({
    x$terminate()
    rm(x)
    gc()
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
  expect_false(anyNA(out$trace))
  handle <- x$launcher$workers$handle[[1]]
  x$terminate()
  expect_false(x$router$listening())
  crew_retry(
    ~!handle$is_alive(),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
})

crew_test("crew_controller_local() can terminate a lost worker", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(seconds_idle = 360, seconds_launch = 180)
  x$start()
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$launcher$workers$launches <- 1L
  bin <- if_any(tolower(Sys.info()[["sysname"]]) == "windows", "R.exe", "R")
  path <- file.path(R.home("bin"), bin)
  call <- "Sys.sleep(300)"
  handle <- processx::process$new(command = path, args = c("-e", call))
  crew_retry(
    ~handle$is_alive(),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  x$launcher$workers$handle[[1L]] <- handle
  x$launcher$workers$socket[1L] <- rownames(x$router$daemons)
  x$launcher$workers$start[1L] <- - Inf
  x$launcher$workers$launches[1L] <- 1L
  expect_true(handle$is_alive())
  x$scale()
  crew_retry(
    ~!handle$is_alive(),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_false(handle$is_alive())
})

crew_test("crew_controller_local() launch method", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(seconds_idle = 360)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  x$launch(n = 1L)
  handle <- x$launcher$workers$handle[[1]]
  crew_retry(
    ~handle$is_alive(),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_true(handle$is_alive())
  x$terminate()
  crew_retry(
    ~!handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  expect_false(handle$is_alive())
})
