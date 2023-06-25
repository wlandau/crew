crew_test("crew_controller_local()", {
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
  expect_null(x$client$started)
  expect_null(x$summary())
  x$start()
  expect_true(x$empty())
  expect_false(x$saturated())
  crew_retry(
    ~{
      x$wait(seconds_timeout = 30, seconds_interval = 0.5)
      TRUE
    },
    seconds_interval = 0.5,
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
        "worker",
        "tasks",
        "seconds",
        "errors",
        "warnings"
      )
    )
  )
  expect_equal(s$tasks, 0L)
  expect_true(x$client$started)
  # first task
  x$push(
    command = Sys.getenv("CREW_INSTANCE"),
    name = "task",
    save_command = TRUE
  )
  expect_false(x$empty())
  expect_true(x$nonempty())
  x$wait(seconds_timeout = 5)
  expect_false(x$empty())
  expect_true(x$nonempty())
  envir <- new.env(parent = emptyenv())
  crew_retry(
    ~{
      envir$out <- x$pop(scale = TRUE)
      !is.null(envir$out)
    },
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  out <- envir$out
  expect_true(x$empty())
  expect_false(x$nonempty())
  expect_equal(x$summary()$tasks, 1L)
  expect_equal(x$summary()$errors, 0L)
  expect_equal(x$summary()$warnings, 0L)
  instance <- parse_instance(x$client$summary()$socket)
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
      seed = 0L,
      save_command = FALSE,
      seconds_timeout = 100
    )
    x$wait(seconds_timeout = 5)
    out <- x$pop()
    set.seed(0L)
    exp <- paste0("abc", sample.int(n = 1e9L, size = 1L))
    expect_true(anyNA(out$command))
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
  expect_false(x$client$started)
  crew_retry(
    ~!handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5,
  )
})

crew_test("crew_controller_local() substitute = FALSE and quick push", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    seconds_idle = 360
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  expect_silent(x$validate())
  expect_null(x$client$started)
  x$start()
  expect_equal(x$summary()$tasks, 0L)
  expect_equal(x$summary()$errors, 0L)
  expect_equal(x$summary()$warnings, 0L)
  command <- quote(sqrt(4L) + sqrt(9L))
  # regular push
  x$push(command = command, substitute = FALSE, name = "substitute")
  x$wait(seconds_timeout = 10)
  # just list
  out <- x$schedule$list()[[1L]]
  expect_equal(out$result[[1L]], 5L)
  expect_equal(out$name, "substitute")
  expect_true(is.numeric(out$seconds))
  expect_false(anyNA(out$seconds))
  expect_true(out$seconds >= 0)
  expect_true(anyNA(out$error))
  expect_true(anyNA(out$warnings))
  expect_true(anyNA(out$trace))
  # full pop
  out <- x$pop(scale = FALSE)
  expect_equal(out$result[[1]], 5L)
  expect_equal(out$name, "substitute")
  expect_true(is.numeric(out$seconds))
  expect_false(anyNA(out$seconds))
  expect_true(out$seconds >= 0)
  expect_true(anyNA(out$error))
  expect_true(anyNA(out$warnings))
  expect_true(anyNA(out$trace))
  # quick push
  x$shove(command = command, name = "substitute")
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
  # cleanup
  handle <- x$launcher$workers$handle[[1]]
  x$terminate()
  expect_false(x$client$started)
  crew_retry(
    ~!handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
})

crew_test("crew_controller_local() warnings and errors", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    seconds_idle = 360
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  expect_silent(x$validate())
  expect_null(x$client$started)
  x$start()
  expect_equal(x$summary()$tasks, 0L)
  expect_equal(x$summary()$errors, 0L)
  expect_equal(x$summary()$warnings, 0L)
  x$push(command = {
    warning("this is a warning")
    stop("this is an error")
  }, name = "warnings_and_errors")
  x$wait(seconds_timeout = 5)
  out <- x$pop(scale = FALSE)
  expect_equal(x$summary()$tasks, 1L)
  expect_equal(x$summary()$errors, 1L)
  expect_equal(x$summary()$warnings, 1L)
  expect_equal(out$name, "warnings_and_errors")
  expect_true(is.numeric(out$seconds))
  expect_false(anyNA(out$seconds))
  expect_true(out$seconds >= 0)
  expect_equal(out$error, "this is an error")
  expect_equal(out$warnings, "this is a warning")
  expect_false(anyNA(out$trace))
  handle <- x$launcher$workers$handle[[1]]
  x$terminate()
  expect_false(x$client$started)
  crew_retry(
    ~!handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
})

crew_test("crew_controller_local() can terminate a lost worker", {
  skip_on_cran()
  skip_on_os("windows")
  if (isTRUE(as.logical(Sys.getenv("CI", "false")))) {
    skip_on_os("mac")
  }
  x <- crew_controller_local(
    workers = 1L,
    seconds_idle = 360,
    seconds_launch = 180
  )
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
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  x$launcher$workers$handle[[1L]] <- handle
  x$launcher$workers$socket[1L] <- x$client$summary()$socket
  x$launcher$workers$start[1L] <- - Inf
  x$launcher$workers$launches[1L] <- 1L
  expect_true(handle$is_alive())
  x$launcher$rotate(index = 1L)
  crew_retry(
    ~!handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 60
  )
  expect_false(handle$is_alive())
})

crew_test("crew_controller_local() launch method", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    seconds_idle = 360
  )
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
    seconds_interval = 0.1,
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

crew_test("task collection and results stack work", {
  skip_on_cran()
  x <- crew_controller_local(seconds_idle = 120)
  x$start()
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  n <- 200L
  for (i in seq_len(3L)) {
    for (index in seq_len(n)) {
      name <- paste0("task_", index)
      x$push(name = name, command = index, data = list(index = index))
    }
    results <- list()
    while (length(results) < n) {
      out <- x$pop()
      if (!is.null(out)) {
        results[[length(results) + 1L]] <- out
      }
    }
    results <- tibble::as_tibble(do.call(rbind, results))
    results$result <- as.integer(results$result)
    expect_equal(sort(results$result), seq_len(200L))
  }
  x$terminate()
})
