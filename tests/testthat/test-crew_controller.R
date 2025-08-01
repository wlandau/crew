crew_test("active bindings", {
  x <- crew_controller_local(crashes_max = 1L)
  expect_equal(x$crashes_max, 1L)
  expect_null(x$backup)
})

crew_test("warnings and errors", {
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
  expect_null(x$cancel())
  expect_false(x$client$started)
  expect_null(x$pop())
  x$start()
  expect_silent(x$validate())
  expect_equal(x$summary()$tasks, 0L)
  expect_equal(x$summary()$error, 0L)
  expect_equal(x$summary()$warning, 0L)
  x$push(command = {
    warning("this is a warning")
    stop("this is an error")
  }, name = "warnings_and_errors")
  x$wait(seconds_timeout = 5)
  out <- x$pop(scale = FALSE)
  skip_on_covr() # error handling is mysteriously messed up with covr
  expect_equal(x$summary()$tasks, 1L)
  expect_equal(x$summary()$error, 1L)
  expect_equal(x$summary()$warning, 1L)
  expect_equal(out$name, "warnings_and_errors")
  expect_true(is.numeric(out$seconds))
  expect_false(anyNA(out$seconds))
  expect_true(out$seconds >= 0)
  expect_equal(out$error, "this is an error")
  expect_equal(out$warnings, "this is a warning")
  expect_false(anyNA(out$trace))
  handle <- x$launcher$instances$handle[[1]]
  x$terminate()
  expect_false(x$client$started)
  crew_retry(
    ~!handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 10
  )
})

crew_test("can relay task errors as local errors", {
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
  x$push(command =  stop("this is an error"), name = "warnings_and_errors")
  x$wait(seconds_timeout = 30)
  expect_silent(
    if_any(
      isTRUE(as.logical(Sys.getenv("R_COVR", "false"))),
      suppressWarnings(
        try(
          x$pop(scale = FALSE, error = "stop"),
          silent = TRUE
        )
      ),
      expect_crew_error(x$pop(scale = FALSE, error = "stop"))
    )
  )
})

crew_test("can relay task errors as local warnings", {
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
  x$push(command =  stop("this is an error"), name = "warnings_and_errors")
  x$wait(seconds_timeout = 30)
  expect_silent(
    if_any(
      isTRUE(as.logical(Sys.getenv("R_COVR", "false"))),
      suppressWarnings(x$pop(scale = FALSE, error = "warn")),
      expect_warning(
        x$pop(scale = FALSE, error = "warn"),
        class = "crew_warning"
      )
    )
  )
})

crew_test("can terminate a lost worker", {
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
  private <- crew_private(x$launcher)
  bin <- if_any(tolower(Sys.info()[["sysname"]]) == "windows", "R.exe", "R")
  path <- file.path(R.home("bin"), bin)
  call <- "Sys.sleep(300)"
  handle <- processx::process$new(command = path, args = c("-e", call))
  crew_retry(
    ~handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  private$.instances <- tibble::add_row(
    private$.instances,
    handle = list(handle),
    id = 99L,
    start = - Inf,
    online = FALSE,
    discovered = FALSE
  )
  x$scale()
  crew_retry(
    ~!handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 60
  )
  expect_false(handle$is_alive())
})

crew_test("deprecate auto_scale", {
  skip_on_cran()
  suppressWarnings(
    crew_controller(
      client = crew_client(host = "127.0.0.1"),
      launcher = crew_launcher_local(),
      auto_scale = "demand"
    )
  )
  expect_true(TRUE)
})

crew_test("task collection and results stack work", {
  skip_on_cran()
  skip_on_os("windows")
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

crew_test("controller walk()", {
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
  x$start()
  f <- function(x, y) x + y
  out <- x$walk(
    command = f(x, y) + a + b,
    iterate = list(x = c(1L, 2L), y = c(3L, 4L)),
    data = list(a = 5L),
    globals = list(f = f, b = 6L),
    seed = 0L
  )
  expect_true(is.list(out))
  expect_s3_class(out[[1L]], "mirai")
  expect_s3_class(out[[2L]], "mirai")
  x$wait(mode = "all")
  task1 <- x$pop()
  task2 <- x$pop()
  expect_true(tibble::is_tibble(task1))
  expect_true(tibble::is_tibble(task2))
  expect_equal(
    sort(c(task1$result[[1L]], task2$result[[1L]])),
    c(15L, 17L)
  )
})

crew_test("controller collect() success", {
  skip_on_cran()
  skip_on_os("windows")
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x <- crew_controller_local(workers = 1L, seconds_idle = 30L)
  x$start()
  for (index in seq_len(2L)) x$push("done")
  x$wait(mode = "all")
  for (index in seq_len(2L)) x$push(Sys.sleep(120))
  expect_equal(length(x$tasks), 4L)
  out <- x$collect()
  expect_equal(length(x$tasks), 2L)
  expect_equal(x$queue$data, character(0L))
  expect_equal(x$queue$head, 1L)
  expect_equal(nrow(out), 2L)
  expect_equal(as.character(out$result), rep("done", 2))
  expect_null(x$collect())
  expect_crew_error(x$collect(error = "bad"))
})

crew_test("controller collect() silent error", {
  skip_on_cran()
  skip_on_os("windows")
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x <- crew_controller_local(workers = 1L, seconds_idle = 30L)
  x$start()
  x$push("success")
  x$push(stop("failure 1"))
  x$push(stop("failure 2"))
  x$wait(mode = "all")
  expect_silent(out <- x$collect(error = "silent"))
  expect_true("failure 1" %in% out$error)
})

crew_test("controller collect() error as warning", {
  skip_on_cran()
  skip_on_os("windows")
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x <- crew_controller_local(workers = 1L, seconds_idle = 30L)
  x$start()
  x$push("success")
  x$push(stop("failure 1"))
  x$push(stop("failure 2"))
  x$wait(mode = "all")
  suppressWarnings(
    expect_warning(x$collect(error = "warn"), class = "crew_warning")
  )
})

crew_test("controller collect() stop on error", {
  skip_on_cran()
  skip_on_os("windows")
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x <- crew_controller_local(workers = 1L, seconds_idle = 30L)
  x$start()
  x$push("success")
  x$push(stop("failure 1"))
  x$push(stop("failure 2"))
  x$wait(mode = "all")
  expect_crew_error(x$collect(error = "stop"))
})

crew_test("controller map() works", {
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
  x$start()
  f <- function(x, y) x + y
  out <- x$map(
    command = f(x, y) + a + b,
    iterate = list(x = c(1L, 2L), y = c(3L, 4L)),
    data = list(a = 5L),
    globals = list(f = f, b = 6L),
    seed = 0L,
    verbose = TRUE
  )
  x$terminate()
  expect_null(x$error)
  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 2L)
  expect_equal(colnames(out), monad_names)
  expect_true(all(grepl("^unnamed_task_", out$name)))
  expect_equal(gsub("^.*_", "", out$name), c("1", "2"))
  expect_equal(out$command, rep("f(x, y) + a + b", 2L))
  expect_equal(out$result, list(15L, 17L))
  expect_true(all(out$seconds >= 0))
  expect_true(is.integer(out$seed))
  expect_true(anyDuplicated(out$seed) < 1L)
  expect_equal(out$error, rep(NA_character_, 2L))
  expect_equal(out$trace, rep(NA_character_, 2L))
  expect_equal(out$warnings, rep(NA_character_, 2L))
  expect_equal(out$controller, rep(x$launcher$name, 2L))
  sum <- x$summary()
  expect_equal(sum$tasks, 2L)
  expect_equal(sum$error, 0L)
  expect_equal(sum$warning, 0L)
})

crew_test("map() works with errors and names and command strings", {
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
  x$start()
  f <- function(x, y) {
    warning("message")
    x + y
  }
  x$map(
    command = f(x, y) + a + b,
    iterate = list(x = c(1L, 2L), y = c(3L, 4L), id = c("z", "w")),
    data = list(a = 5L),
    globals = list(f = f),
    names = "id",
    error = "silent",
    warnings = FALSE,
    verbose = FALSE
  )
  expect_null(x$error)
  expect_error(
    x$map(
      command = f(x, y) + a + b,
      iterate = list(x = c(1L, 2L), y = c(3L, 4L), id = c("z", "w")),
      data = list(a = 5L),
      globals = list(f = f),
      names = "id",
      error = "stop",
      warnings = FALSE,
      verbose = TRUE
    ),
    class = "crew_error"
  )
  expect_true(is.data.frame(x$error))
  expect_warning(
    out <- x$map(
      command = f(x, y) + a + b,
      iterate = list(x = c(1L, 2L), y = c(3L, 4L), id = c("z", "w")),
      data = list(a = 5L),
      globals = list(f = f),
      names = "id",
      error = "warn",
      warnings = FALSE,
      verbose = TRUE
    ),
    class = "crew_warning"
  )
  x$terminate()
  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 2L)
  expect_equal(colnames(out), monad_names)
  expect_equal(out$name, c("z", "w"))
  expect_equal(out$command, rep("f(x, y) + a + b", 2L))
  expect_equal(out$result, list(NA, NA))
  expect_true(all(out$seconds >= 0))
  expect_true(is.integer(out$seed))
  expect_true(all(is.na(out$seed)))
  expect_false(anyNA(out$error))
  expect_false(anyNA(out$trace))
  expect_false(anyNA(out$warnings))
  expect_equal(out$controller, rep(x$launcher$name, 2L))
  sum <- x$summary()
  expect_equal(sum$tasks, 6L)
  expect_equal(sum$error, 6L)
  expect_equal(sum$warning, 6L)
})

crew_test("map() does not need a started controller", {
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
  results <- x$map(command = TRUE, iterate = list(x = c(1, 2)))
  expect_equal(nrow(results), 2L)
})

crew_test("map() needs an empty controller", {
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
  x$start()
  x$push(command = TRUE)
  x$wait(seconds_timeout = 30)
  expect_equal(x$unresolved(), 0L)
  expect_equal(x$resolved(), 1L)
  expect_crew_error(x$map(command = TRUE, iterate = list(x = c(1, 2))))
})

crew_test("map() can relay warnings", {
  skip_on_cran()
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
  x$start()
  f <- function(x, y) {
    warning("message")
    x + y
  }
  expect_warning(
    x$map(
      command = f(x, y) + a + b,
      iterate = list(x = c(1L, 2L), y = c(3L, 4L), id = c("z", "w")),
      data = list(a = 5L),
      globals = list(f = f),
      names = "id",
      error = "silent",
      warnings = TRUE,
      verbose = FALSE
    ),
    class = "crew_warning"
  )
})

crew_test("backlog with no tasks", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    workers = 2L,
    seconds_idle = 360
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  expect_true(inherits(x$backlog, "crew_class_queue"))
  expect_equal(x$pop_backlog(), character(0L))
  tasks <- paste0("my_task", seq_len(4L))
  for (task in tasks) {
    x$push_backlog(name = task)
  }
  expect_equal(x$backlog$list(), tasks)
  expect_equal(x$pop_backlog(), tasks[c(1L, 2L)])
  expect_equal(x$backlog$list(), tasks[c(3L, 4L)])
  expect_equal(x$pop_backlog(), tasks[c(3L, 4L)])
  expect_equal(x$backlog$list(), character(0L))
  expect_equal(x$pop_backlog(), character(0L))
})

crew_test("backlog with one task and no saturation", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    workers = 2L,
    seconds_idle = 360
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  x$push(Sys.sleep(30), scale = FALSE)
  expect_equal(x$backlog$list(), character(0L))
  expect_equal(x$pop_backlog(), character(0L))
  tasks <- paste0("my_task", seq_len(4L))
  for (task in tasks) {
    x$push_backlog(name = task)
  }
  expect_equal(x$backlog$list(), tasks)
  expect_equal(x$pop_backlog(), tasks[1L])
  expect_equal(x$backlog$list(), tasks[c(2L, 3L, 4L)])
  expect_equal(x$pop_backlog(), tasks[2L])
  expect_equal(x$backlog$list(), tasks[c(3L, 4L)])
  expect_equal(x$pop_backlog(), tasks[3L])
  expect_equal(x$backlog$list(), tasks[4L])
  expect_equal(x$pop_backlog(), tasks[4L])
  expect_equal(x$backlog$list(), character(0L))
  expect_equal(x$pop_backlog(), character(0L))
})

crew_test("backlog with saturation", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    workers = 2L,
    seconds_idle = 360
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  for (index in seq_len(2L)) {
    x$push(Sys.sleep(30), scale = FALSE)
  }
  tasks <- paste0("my_task", seq_len(4L))
  expect_equal(x$backlog$list(), character(0L))
  expect_equal(x$pop_backlog(), character(0L))
  for (task in tasks) {
    x$push_backlog(name = task)
  }
  for (index in seq_len(4L)) {
    expect_equal(x$backlog$list(), tasks)
    expect_equal(x$pop_backlog(), character(0L))
    x$push(Sys.sleep(30), scale = FALSE)
  }
})

crew_test("descale", {
  controller <- crew_controller_local()
  expect_false(controller$autoscaling)
  controller$descale()
  expect_false(controller$autoscaling)
})

crew_test("cancel() all", {
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
  x$start()
  for (index in seq_len(4L)) {
    x$push(Sys.sleep(1000))
  }
  x$cancel(all = TRUE)
  x$wait(mode = "all", seconds_timeout = 30)
  tasks <- x$collect()
  expect_equal(nrow(tasks), 4L)
  expect_true(all(grepl("operation canceled", tolower(tasks$error))))
})

crew_test("cancel() named", {
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
  x$start()
  x$push(Sys.sleep(1000), name = "x")
  x$push(Sys.sleep(1000), name = "y")
  x$push(Sys.sleep(1000), name = "z")
  x$cancel(names = c("x", "z"))
  for (index in seq_len(2L)) {
    x$wait(mode = "one", seconds_timeout = 30)
  }
  tasks <- x$collect()
  expect_equal(nrow(tasks), 2L)
  expect_equal(sort(tasks$name), sort(c("x", "z")))
  expect_true(all(grepl("operation canceled", tolower(tasks$error))))
  expect_equal(names(x$tasks), "y")
  s <- x$summary()
  expect_equal(s$tasks, 2L)
  expect_equal(s$success, 0L)
  expect_equal(s$error, 0L)
  expect_equal(s$crash, 0L)
  expect_equal(s$cancel, 2L)
  expect_equal(s$warning, 0L)
})

crew_test("handle duplicated names", {
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
  x$start()
  x$push(TRUE, name = "x")
  expect_crew_error(x$push(TRUE, name = "x"))
})

crew_test("crash detection with crashes_max == 0L", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    workers = 1L,
    seconds_idle = 360,
    crashes_max = 0L
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  expect_equal(x$crashes(name = "x"), 0L)
  x$start()
  expect_equal(x$crashes(name = "x"), 0L)
  x$push(TRUE, name = "x")
  expect_equal(x$crashes(name = "x"), 0L)
  x$wait()
  expect_true(tibble::is_tibble(x$pop()))
  expect_equal(x$crashes(name = "x"), 0L)
  x$push(Sys.sleep(300L), name = "x")
  crew_retry(
    ~ {
      x$scale()
      isTRUE(x$launcher$instances$online)
    },
    seconds_interval = 0.1,
    seconds_timeout = 60
  )
  Sys.sleep(0.25)
  x$launcher$terminate_workers()
  x$wait()
  expect_equal(x$crashes(name = "x"), 0L)
  expect_crew_error(x$pop())
  expect_equal(x$crashes(name = "x"), 1L)
})

crew_test("crash detection with crashes_max == 2L", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    workers = 1L,
    seconds_idle = 360,
    crashes_max = 2L
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  for (index in seq_len(2L)) {
    expect_equal(x$crashes(name = "x"), index - 1L)
    x$push(Sys.sleep(300L), name = "x", scale = TRUE)
    crew_retry(
      ~ {
        x$scale()
        isTRUE(x$launcher$instances$online)
      },
      seconds_interval = 0.1,
      seconds_timeout = 60
    )
    Sys.sleep(0.25)
    x$launcher$terminate_workers()
    x$wait()
    expect_true(tibble::is_tibble(x$pop()))
    expect_equal(x$crashes(name = "x"), index)
  }
  x$push(Sys.sleep(300L), name = "x", scale = TRUE)
  crew_retry(
    ~ {
      x$scale()
      isTRUE(x$launcher$instances$online)
    },
    seconds_interval = 0.1,
    seconds_timeout = 60
  )
  Sys.sleep(0.25)
  x$launcher$terminate_workers()
  x$wait()
  expect_crew_error(x$pop())
  expect_equal(x$crashes(name = "x"), 3L)
  expect_equal(x$summary()$crash, 3L)
})

crew_test("crash detection resets, crashes_max == 2L", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    workers = 1L,
    seconds_idle = 360,
    crashes_max = 2L
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  for (index in seq_len(6L)) {
    expect_equal(x$crashes(name = "x"), 0L)
    x$push(Sys.sleep(300L), name = "x", scale = TRUE)
    crew_retry(
      ~ {
        x$scale()
        isTRUE(x$launcher$instances$online)
      },
      seconds_interval = 0.1,
      seconds_timeout = 60
    )
    Sys.sleep(0.25)
    x$launcher$terminate_workers()
    x$wait()
    expect_true(tibble::is_tibble(x$pop()))
    expect_equal(x$crashes(name = "x"), 1L)
    x$push(TRUE, name = "x")
    x$wait()
    expect_true(tibble::is_tibble(x$pop()))
    expect_equal(x$crashes(name = "x"), 0L)
  }
})

crew_test("crash detection with crashes_max == 2L and collect()", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    workers = 1L,
    seconds_idle = 360,
    crashes_max = 2L
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  for (index in seq_len(2L)) {
    expect_equal(x$crashes(name = "x"), index - 1L)
    x$push(Sys.sleep(300L), name = "x", scale = TRUE)
    crew_retry(
      ~ {
        x$scale()
        isTRUE(x$launcher$instances$online)
      },
      seconds_interval = 0.1,
      seconds_timeout = 60
    )
    Sys.sleep(0.25)
    x$launcher$terminate_workers()
    x$wait()
    expect_true(tibble::is_tibble(x$collect()))
    expect_equal(x$crashes(name = "x"), index)
  }
  x$push(Sys.sleep(300L), name = "x", scale = TRUE)
  crew_retry(
    ~ {
      x$scale()
      isTRUE(x$launcher$instances$online)
    },
    seconds_interval = 0.1,
    seconds_timeout = 60
  )
  Sys.sleep(0.25)
  x$launcher$terminate_workers()
  x$wait()
  expect_crew_error(x$collect())
  expect_equal(x$crashes(name = "x"), 3L)
})

crew_test("backup cannot be a controller group", {
  a <- crew_controller_local()
  b <- crew_controller_group(a)
  expect_crew_error(crew_controller_local(backup = b))
})

crew_test("serialization", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    workers = 1L,
    seconds_idle = 360,
    crashes_max = 2L,
    serialization = mirai::serial_config(
      class = "custom",
      sfunc = function(x) {
        "serialization successful"
      },
      ufunc = identity
    )
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  x$push(
    object,
    data = list(object = structure(list("abc"), class = "custom"))
  )
  x$wait(seconds_timeout = 30)
  out <- x$pop()
  expect_equal(out$status, "success")
})
