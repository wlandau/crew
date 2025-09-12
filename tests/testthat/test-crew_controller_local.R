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
  expect_false(x$client$started)
  expect_false(x$started())
  expect_null(x$summary())
  expect_null(x$loop)
  expect_equal(suppressWarnings(length(x$pids())), 1L)
  x$start()
  expect_true(x$empty())
  expect_false(x$saturated())
  crew_retry(
    ~ {
      x$wait(mode = "all", seconds_timeout = 30)
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
        "seconds",
        "tasks",
        "success",
        "error",
        "crash",
        "cancel",
        "warning"
      )
    )
  )
  expect_true(x$client$started)
  expect_true(x$started())
  expect_equal(suppressWarnings(length(x$pids())), 1L)
  # first task
  task <- x$push(
    command = Sys.getenv("CREW_CONTROLLER"),
    name = "task"
  )
  expect_s3_class(task, "mirai")
  expect_false(x$empty())
  expect_true(x$nonempty())
  x$wait(mode = "one", seconds_timeout = 5)
  expect_false(x$empty())
  expect_true(x$nonempty())
  envir <- new.env(parent = emptyenv())
  crew_retry(
    ~ {
      envir$out <- x$pop(scale = TRUE)
      !is.null(envir$out)
    },
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  out <- envir$out
  expect_true(x$empty())
  expect_false(x$nonempty())
  expect_equal(x$summary()$error, 0L)
  expect_equal(x$summary()$warning, 0L)
  expect_equal(out$name, "task")
  expect_equal(out$command, "Sys.getenv(\"CREW_CONTROLLER\")")
  expect_equal(out$result[[1]], out$controller)
  expect_false(any(out$controller == Sys.getenv("CREW_CONTROLLER")))
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
      command = {
        paste0("a", x, .crew_y, sample.int(n = 1e9L, size = 1L))
      },
      data = list(x = "b"),
      globals = list(.crew_y = "c"),
      seed = 0L,
      algorithm = "L'Ecuyer-CMRG",
      seconds_timeout = 100
    )
    x$wait(seconds_timeout = 5)
    out <- x$pop()
    set.seed(seed = 0L, kind = "L'Ecuyer-CMRG")
    exp <- paste0("abc", sample.int(n = 1e9L, size = 1L))
    expect_false(anyNA(out$command))
    expect_equal(out$result[[1]], exp)
    expect_equal(out$error, NA_character_)
    expect_false(exists(x = ".crew_y", envir = globalenv()))
    # package task
    x$push(
      command = paste0(arg, "y"),
      data = list(arg = "x"),
      packages = "nanonext"
    )
    x$wait(seconds_timeout = 5)
    out <- x$pop()
    expect_equal(out$result[[1]], "xy")
  }
  # terminate
  handle <- unlist(x$launcher$launches$handle)[[1]]
  x$terminate()
  expect_false(x$client$started)
  expect_false(x$started())
  crew_retry(
    ~ !handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
})

crew_test("crew_controller_local() substitute = FALSE and quick push", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    seconds_idle = 360,
    r_arguments = c("--no-save", "--no-restore")
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  expect_silent(x$validate())
  expect_false(x$client$started)
  x$start()
  expect_equal(x$summary()$error, 0L)
  expect_equal(x$summary()$warning, 0L)
  command <- quote(sqrt(4L) + sqrt(9L))
  # regular push
  x$push(command = command, substitute = FALSE, name = "substitute")
  x$wait(seconds_timeout = 10)
  # just the mirai task data
  out <- as.list(x$tasks)[[1L]]$data
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
  # cleanup
  handle <- unlist(x$launcher$launches$handle)[[1]]
  x$terminate()
  expect_false(x$client$started)
  crew_retry(
    ~ !handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
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
  handle <- unlist(x$launcher$launches$handle)[[1]]
  crew_retry(
    ~ handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  expect_true(handle$is_alive())
  x$terminate()
  crew_retry(
    ~ !handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  expect_false(handle$is_alive())
})

crew_test("exit status and code", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local()
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  x$push(Sys.sleep(0.01), name = "short")
  x$wait(mode = "one")
  task <- x$pop()
  expect_equal(task$status, "success")
  expect_equal(task$code, 0L)
  x$push(stop("message"), name = "broken")
  x$wait(mode = "one")
  task <- x$pop()
  expect_equal(task$status, "error")
  expect_equal(task$code, -1L)
  x$push(Sys.sleep(10000), name = "long")
  x$cancel(names = "long")
  x$wait()
  task <- x$pop()
  expect_equal(task$status, "cancel")
  expect_equal(task$code, 20L)
})

crew_test("crew_controller_local() resource usage metric logging", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("autometric", minimum_version = "0.1.0")
  log <- tempfile()
  x <- crew_controller_local(
    seconds_idle = 360,
    options_metrics = crew_options_metrics(
      path = log,
      seconds_interval = 0.25
    )
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  x$push(name = "monitored_task", Sys.sleep(2))
  x$wait(mode = "all", seconds_timeout = 30)
  x$terminate()
  expect_true(dir.exists(log))
  data <- autometric::log_read(log)
  expect_true(any("monitored_task" %in% data$phase))
  expect_true(is.data.frame(data))
  expect_gt(nrow(data), 0L)
  expect_equal(unique(data$status), 0L)
})

crew_test("crew_controller_local() resource usage metrics with stdout", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("autometric", minimum_version = "0.1.0")
  log <- tempfile()
  x <- crew_controller_local(
    seconds_idle = 360,
    options_metrics = crew_options_metrics(
      path = "/dev/stdout",
      seconds_interval = 0.25
    ),
    options_local = crew_options_local(
      log_directory = log
    )
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  x$push(Sys.sleep(2))
  x$wait(mode = "all", seconds_timeout = 30)
  x$terminate()
  expect_true(dir.exists(log))
  data <- autometric::log_read(log)
  expect_true(is.data.frame(data))
  expect_gt(nrow(data), 0L)
  expect_equal(unique(data$status), 0L)
})

crew_test("joined logs", {
  skip_on_cran()
  skip_on_covr()
  skip_on_os("windows")
  dir <- tempfile()
  x <- crew_controller_local(
    workers = 1L,
    seconds_idle = 60,
    options_local = crew_options_local(
      log_directory = dir,
      log_join = TRUE
    )
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
    unlink(dir, recursive = TRUE)
  })
  x$start()
  x$push(print("this-print"))
  # All these waits are needed to avoid an auto-scaling race condition.
  x$wait(mode = "all")
  x$push(message("this-message"))
  x$wait(mode = "all")
  x$push(warning("this-warning"))
  x$wait(mode = "all")
  x$push(stop("this-stop"))
  x$wait(mode = "all")
  dir <- x$launcher$options_local$log_directory
  logs <- list.files(dir, full.names = TRUE)
  expect_equal(length(logs), 1L)
  lines <- readLines(logs)
  expect_true(any(grepl("this-print", lines, fixed = TRUE)))
  expect_true(any(grepl("this-message", lines, fixed = TRUE)))
  expect_true(any(grepl("Warning: this-warning", lines, fixed = TRUE)))
  expect_true(any(grepl("Error: this-stop", lines, fixed = TRUE)))
})

crew_test("separate logs", {
  skip_on_cran()
  skip_on_covr()
  skip_on_os("windows")
  dir <- tempfile()
  x <- crew_controller_local(
    workers = 1L,
    seconds_idle = 60,
    options_local = crew_options_local(
      log_directory = dir,
      log_join = FALSE
    )
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
    unlink(dir, recursive = TRUE)
  })
  x$start()
  x$push(print("this-print"))
  # All these waits are needed to avoid an auto-scaling race condition.
  x$wait(mode = "all")
  x$push(message("this-message"))
  x$wait(mode = "all")
  x$push(warning("this-warning"))
  x$wait(mode = "all")
  x$push(stop("this-stop"))
  x$wait(mode = "all")
  logs <- list.files(dir, full.names = TRUE)
  expect_equal(length(logs), 2L)
  stderr <- readLines(logs[1L])
  stdout <- readLines(logs[2L])
  expect_true(any(grepl("this-print", stdout, fixed = TRUE)))
  expect_true(any(grepl("this-message", stderr, fixed = TRUE)))
  expect_true(any(grepl("Warning: this-warning", stderr, fixed = TRUE)))
  expect_true(any(grepl("Error: this-stop", stderr, fixed = TRUE)))
})

crew_test("custom compute profile", {
  skip_on_cran()
  x <- crew_controller_local(
    host = "127.0.0.1",
    port = "57000",
    profile = "__abc__"
  )
  expect_equal(x$client$profile, "__abc__")
  on.exit(x$terminate())
  x$start()
  url <- nanonext::parse_url(mirai::nextget("url", .compute = "__abc__"))
  expect_equal(as.character(url["host"]), "127.0.0.1:57000")
})

crew_test("deprecate seconds_exit", {
  expect_warning(
    x <- crew_controller_local(
      workers = 1L,
      seconds_idle = 360,
      seconds_exit = 1
    ),
    class = "crew_deprecate"
  )
})
