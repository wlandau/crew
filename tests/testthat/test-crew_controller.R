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
  out <- x$collect()
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
  expect_equal(out$name, c("1", "2"))
  expect_equal(out$command, rep(NA_character_, 2L))
  expect_equal(out$result, list(15L, 17L))
  expect_true(all(out$seconds >= 0))
  expect_true(is.integer(out$seed))
  expect_true(anyDuplicated(out$seed) < 1L)
  expect_equal(out$error, rep(NA_character_, 2L))
  expect_equal(out$trace, rep(NA_character_, 2L))
  expect_equal(out$warnings, rep(NA_character_, 2L))
  expect_equal(out$worker, rep(1L, 2L))
  expect_equal(out$launcher, rep(x$launcher$name, 2L))
  expect_false(anyNA(out$instance))
  sum <- x$summary()
  expect_equal(sum$worker, 1L)
  expect_equal(sum$tasks, 2L)
  expect_equal(sum$errors, 0L)
  expect_equal(sum$warnings, 0L)
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
    save_command = TRUE,
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
      save_command = TRUE,
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
      save_command = TRUE,
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
  expect_equal(out$worker, rep(1L, 2L))
  expect_equal(out$launcher, rep(x$launcher$name, 2L))
  expect_false(anyNA(out$instance))
  sum <- x$summary()
  expect_equal(sum$worker, 1L)
  expect_equal(sum$tasks, 6L)
  expect_equal(sum$errors, 6L)
  expect_equal(sum$warnings, 6L)
})

crew_test("map() tasks attributed to correct workers", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    workers = 4L,
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
    Sys.sleep(0.25)
    x + y
  }
  x$launcher$launch(index = 3L)
  out <- x$map(
    command = f(x, y) + a + b,
    iterate = list(x = 1L, y = 3L, id = "z"),
    data = list(a = 5L),
    globals = list(f = f),
    save_command = TRUE,
    names = "id",
    error = "silent",
    warnings = FALSE
  )
  sum <- x$summary()
  expect_equal(sum$worker, seq_len(4L))
  expect_equal(sum$tasks, c(0L, 0L, 1L, 0L))
  expect_equal(
    sum$seconds > sqrt(.Machine$double.eps),
    c(FALSE, FALSE, TRUE, FALSE)
  )
  expect_equal(sum$errors, c(0L, 0L, 1L, 0L))
  expect_equal(sum$warnings, c(0L, 0L, 1L, 0L))
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
  expect_warning(
    x$map(
      command = f(x, y) + a + b,
      iterate = list(x = c(1L, 2L), y = c(3L, 4L), id = c("z", "w")),
      data = list(a = 5L),
      globals = list(f = f),
      save_command = TRUE,
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
  expect_equal(x$backlog, character(0L))
  expect_equal(x$pop_backlog(), character(0L))
  tasks <- paste0("my_task", seq_len(4L))
  for (task in tasks) {
    x$push_backlog(name = task)
  }
  expect_equal(x$backlog, tasks)
  expect_equal(x$pop_backlog(), tasks[c(1L, 2L)])
  expect_equal(x$backlog, tasks[c(3L, 4L)])
  expect_equal(x$pop_backlog(), tasks[c(3L, 4L)])
  expect_equal(x$backlog, character(0L))
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
  expect_equal(x$backlog, character(0L))
  expect_equal(x$pop_backlog(), character(0L))
  tasks <- paste0("my_task", seq_len(4L))
  for (task in tasks) {
    x$push_backlog(name = task)
  }
  expect_equal(x$backlog, tasks)
  expect_equal(x$pop_backlog(), tasks[1L])
  expect_equal(x$backlog, tasks[c(2L, 3L, 4L)])
  expect_equal(x$pop_backlog(), tasks[2L])
  expect_equal(x$backlog, tasks[c(3L, 4L)])
  expect_equal(x$pop_backlog(), tasks[3L])
  expect_equal(x$backlog, tasks[4L])
  expect_equal(x$pop_backlog(), tasks[4L])
  expect_equal(x$backlog, character(0L))
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
  expect_equal(x$backlog, character(0L))
  expect_equal(x$pop_backlog(), character(0L))
  for (task in tasks) {
    x$push_backlog(name = task)
  }
  for (index in seq_len(4L)) {
    expect_equal(x$backlog, tasks)
    expect_equal(x$pop_backlog(), character(0L))
    x$push(Sys.sleep(30), scale = FALSE)
  }
})

crew_test("descale", {
  controller <- crew_controller_local()
  expect_null(controller$autoscaling)
  controller$descale()
  expect_false(controller$autoscaling)
})
