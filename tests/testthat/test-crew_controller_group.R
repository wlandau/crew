crew_test("crew_controller_group() method and signature compatibility", {
  x <- crew_controller_local()
  y <- crew_controller_group(x = x)
  common <- intersect(names(x), names(y))
  methods <- fltr(common, ~is.function(x[[.x]]))
  methods <- setdiff(methods, "initialize")
  for (method in methods) {
    expect_equal(names(formals(x[[method]])), names(formals(y[[method]])))
  }
})

crew_test("crew_controller_group() active bindings for covr", {
  x <- crew_controller_local()
  y <- crew_controller_group(x = x)
  expect_true(inherits(y$relay, "crew_class_relay"))
})

crew_test("crew_controller_group()", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    name = "a",
    seconds_idle = 360
  )
  b <- crew_controller_local(
    name = "b",
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  expect_null(x$summary())
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  expect_silent(x$validate())
  for (index in seq_len(2)) {
    expect_null(x$controllers[[index]]$client$started)
  }
  expect_false(x$started())
  expect_equal(length(x$pids()), 1L)
  x$start()
  expect_true(x$started())
  expect_true(x$empty())
  expect_false(x$saturated())
  expect_true(x$empty(controllers = "a"))
  expect_true(x$empty(controllers = "b"))
  expect_equal(length(x$pids()), 3L)
  for (index in seq_len(2)) {
    expect_true(x$controllers[[index]]$client$started)
  }
  crew_retry(
    ~{
      x$wait(seconds_timeout = 30)
      TRUE
    },
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  s <- x$summary()
  expect_equal(nrow(s), 2L)
  expect_equal(s$controller, c("a", "b"))
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
  expect_null(x$pop())
  # substitute = TRUE # nolint
  task <- x$push(
    command = ps::ps_pid(),
    name = "task_pid",
    controller = "b",
    save_command = TRUE
  )
  expect_s3_class(task, "mirai")
  expect_false(x$empty())
  expect_true(x$empty(controllers = "a"))
  expect_false(x$empty(controllers = "b"))
  x$wait(mode = "all", seconds_timeout = 5)
  expect_false(x$empty())
  expect_true(x$empty(controllers = "a"))
  expect_false(x$empty(controllers = "b"))
  out <- x$pop(scale = FALSE)
  expect_true(x$empty())
  expect_true(x$empty(controllers = "a"))
  expect_true(x$empty(controllers = "b"))
  expect_false(is.null(out))
  expect_equal(out$name, "task_pid")
  expect_equal(out$command, "ps::ps_pid()")
  expect_true(is.numeric(out$seconds))
  expect_false(anyNA(out$seconds))
  expect_true(out$seconds >= 0)
  expect_true(anyNA(out$error))
  expect_true(anyNA(out$trace))
  expect_true(anyNA(out$warnings))
  pid_out <- out$result[[1]]
  pid_exp <- x$controllers[[2]]$launcher$workers$handle[[1]]$get_pid()
  expect_equal(pid_out, pid_exp)
  # substitute = FALSE # nolint
  x$push(
    command = quote(ps::ps_pid()),
    substitute = FALSE,
    name = "task_pid2",
    controller = "a",
    save_command = FALSE
  )
  x$wait(seconds_timeout = 5)
  envir <- new.env(parent = emptyenv())
  crew_retry(
    ~{
      envir$out <- x$pop(scale = TRUE)
      !is.null(envir$out)
    },
    seconds_interval = 0.1,
    seconds_timeout = 10
  )
  out <- envir$out
  expect_false(is.null(out))
  expect_equal(out$name, "task_pid2")
  expect_true(anyNA(out$command))
  expect_true(is.numeric(out$seconds))
  expect_false(anyNA(out$seconds))
  expect_true(out$seconds >= 0)
  expect_true(anyNA(out$error))
  expect_true(anyNA(out$trace))
  expect_true(anyNA(out$warnings))
  pid_out <- out$result[[1]]
  pid_exp <- x$controllers[[1]]$launcher$workers$handle[[1]]$get_pid()
  expect_equal(pid_out, pid_exp)
  # cleanup
  handle <- x$controllers[[2]]$launcher$workers$handle[[1]]
  x$terminate()
  expect_false(x$started())
  for (index in seq_len(2)) {
    expect_false(x$controllers[[index]]$client$started)
    crew_retry(
      ~!handle$is_alive(),
      seconds_interval = 0.1,
      seconds_timeout = 5
    )
  }
})

crew_test("crew_controller_group() can relay task errors as local errors", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    seconds_idle = 360
  )
  x <- crew_controller_group(a)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  x$push(command =  stop("this is an error"), name = "warnings_and_errors")
  x$wait(seconds_timeout = 5)
  expect_crew_error(x$pop(scale = FALSE, error = "stop"))
})

crew_test("crew_controller_group() can relay task errors as local warnings", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    seconds_idle = 360
  )
  x <- crew_controller_group(a)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  x$push(command =  stop("this is an error"), name = "warnings_and_errors")
  x$wait(seconds_timeout = 5)
  expect_warning(
    x$pop(scale = FALSE, error = "warn"),
    class = "crew_warning"
  )
})

crew_test("crew_controller_group() select", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    name = "a"
  )
  b <- crew_controller_local(
    name = "b",
    tasks_max = 1L,
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  expect_null(a$client$started)
  expect_null(b$client$started)
  name <- "b"
  x$start(controllers = name)
  expect_null(a$client$started)
  expect_true(b$client$started)
  x$terminate(controllers = name)
  expect_null(a$client$started)
  expect_false(b$client$started)
})

crew_test("crew_controller_group() launch method", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    name = "a",
    seconds_idle = 360
  )
  b <- crew_controller_local(
    name = "b",
    tasks_max = 1L,
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  for (index in seq_len(2)) {
    handle <- x$controllers[[index]]$launcher$workers$handle[[1L]]
    expect_true(is_crew_null(handle))
  }
  expect_silent(x$launch(n = 1L))
  handles <- list(
    x$controllers[[1L]]$launcher$workers$handle[[1L]],
    x$controllers[[2L]]$launcher$workers$handle[[1L]]
  )
  for (index in seq_len(2)) {
    crew_retry(
      ~handles[[index]]$is_alive(),
      seconds_interval = 0.1,
      seconds_timeout = 5
    )
  }
  x$terminate()
  for (index in seq_len(2)) {
    crew_retry(
      ~!handles[[index]]$is_alive(),
      seconds_interval = 0.1,
      seconds_timeout = 5
    )
  }
})

crew_test("crew_controller_group() scale method", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    name = "a",
    seconds_idle = 360
  )
  x <- crew_controller_group(a)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  a$push(command = "x", scale = FALSE)
  x$scale()
  crew_retry(
    ~length(a$launcher$workers$handle) > 0L,
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  handle <- a$launcher$workers$handle[[1L]]
  crew_retry(
    fun = ~handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  x$terminate()
  crew_retry(
    fun = ~!handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  expect_true(TRUE)
})

crew_test("controller walk()", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    workers = 1L,
    seconds_idle = 360
  )
  x <- crew_controller_group(a)
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

crew_test("controller group collect() with one active controller", {
  skip_on_cran()
  skip_on_os("windows")
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  a <- crew_controller_local(
    name = "a",
    workers = 1L,
    seconds_idle = 360
  )
  b <- crew_controller_local(
    name = "b",
    workers = 1L,
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  x$push("done", controller = "a")
  x$push("done", controller = "a")
  x$wait(mode = "all")
  for (index in seq_len(2L)) x$push(Sys.sleep(120))
  out <- x$collect()
  expect_equal(nrow(out), 2L)
  expect_equal(as.character(out$result), rep("done", 2))
  expect_null(x$collect())
})

crew_test("controller group collect() with two active controllers", {
  skip_on_cran()
  skip_on_os("windows")
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  a <- crew_controller_local(
    name = "a",
    workers = 1L,
    seconds_idle = 360
  )
  b <- crew_controller_local(
    name = "b",
    workers = 1L,
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  x$push("done", controller = "a")
  x$push("done", controller = "b")
  x$wait(mode = "all")
  for (index in seq_len(2L)) x$push(Sys.sleep(120))
  out <- x$collect()
  expect_equal(nrow(out), 2L)
  expect_equal(as.character(out$result), rep("done", 2))
  expect_null(x$collect())
})

crew_test("controller group collect() silent error", {
  skip_on_cran()
  skip_on_os("windows")
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  a <- crew_controller_local(workers = 1L, seconds_idle = 30L)
  x <- crew_controller_group(a)
  x$start()
  x$push("success")
  x$push(stop("failure 1"))
  x$push(stop("failure 2"))
  x$wait(mode = "all")
  expect_silent(out <- x$collect(error = "silent"))
  expect_true("failure 1" %in% out$error)
})

crew_test("controller group collect() error as warning", {
  skip_on_cran()
  skip_on_os("windows")
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  a <- crew_controller_local(workers = 1L, seconds_idle = 30L)
  x <- crew_controller_group(a)
  x$start()
  x$push("success")
  x$push(stop("failure 1"))
  x$push(stop("failure 2"))
  x$wait(mode = "all")
  suppressWarnings(
    expect_warning(x$collect(error = "warn"), class = "crew_warning")
  )
})

crew_test("controller group collect() stop on error", {
  skip_on_cran()
  skip_on_os("windows")
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  a <- crew_controller_local(workers = 1L, seconds_idle = 30L)
  x <- crew_controller_group(a)
  x$start()
  x$push("success")
  x$push(stop("failure 1"))
  x$push(stop("failure 2"))
  x$wait(mode = "all")
  expect_crew_error(x$collect(error = "stop"))
})

crew_test("controller group map() works", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    workers = 1L,
    seconds_idle = 360
  )
  x <- crew_controller_group(a)
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
    seed = 0L
  )
  x$terminate()
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
  expect_equal(out$launcher, rep(a$launcher$name, 2L))
  expect_false(anyNA(out$instance))
  sum <- x$summary()
  expect_equal(sum$worker, 1L)
  expect_equal(sum$tasks, 2L)
  expect_equal(sum$errors, 0L)
  expect_equal(sum$warnings, 0L)
})

crew_test("crew_controller_group() wait one", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    name = "a",
    seconds_idle = 360
  )
  b <- crew_controller_local(
    name = "b",
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  expect_null(x$summary())
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  expect_false(x$wait(mode = "one", seconds_timeout = 30))
  x$push(
    command = "done",
    name = "task_a",
    controller = "a"
  )
  x$push(
    command = Sys.sleep(300),
    name = "task_a2",
    controller = "a"
  )
  x$push(
    command = Sys.sleep(300),
    name = "task_a",
    controller = "b"
  )
  expect_true(x$wait(mode = "one", seconds_timeout = 30))
  out <- x$pop()
  expect_equal(out$result[[1L]], "done")
})

crew_test("crew_controller_group() wait all timeout", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    name = "a",
    seconds_idle = 360
  )
  b <- crew_controller_local(
    name = "b",
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  expect_null(x$summary())
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  x$push(
    command = Sys.sleep(300),
    name = "task_a",
    controller = "b"
  )
  expect_false(x$wait(mode = "all", seconds_timeout = 0, seconds_interval = 0))
})

crew_test("controllers in groups must not already be started", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    name = "a",
    seconds_idle = 360
  )
  b <- crew_controller_local(
    name = "b",
    seconds_idle = 360
  )
  on.exit({
    b$terminate()
    rm(b)
    gc()
    crew_test_sleep()
  })
  b$start()
  expect_crew_error(crew_controller_group(a, b))
})

crew_test("backlog with no tasks", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    name = "a",
    workers = 2L,
    seconds_idle = 360
  )
  b <- crew_controller_local(
    name = "b",
    workers = 2L,
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  expect_equal(a$backlog, character(0L))
  expect_equal(b$backlog, character(0L))
  expect_equal(x$pop_backlog(), character(0L))
  tasks_a <- paste0("a_", seq_len(4L))
  for (task in tasks_a) {
    x$push_backlog(name = task, controller = "a")
  }
  tasks_b <- paste0("b_", seq_len(4L))
  for (task in tasks_b) {
    x$push_backlog(name = task, controller = "b")
  }
  expect_equal(a$backlog, tasks_a)
  expect_equal(b$backlog, tasks_b)
  expect_equal(
    sort(x$pop_backlog()),
    sort(c(tasks_a[seq_len(2L)], tasks_b[seq_len(2L)]))
  )
  expect_equal(a$backlog, tasks_a[c(3L, 4L)])
  expect_equal(b$backlog, tasks_b[c(3L, 4L)])
  expect_equal(
    sort(x$pop_backlog()),
    sort(c(tasks_a[c(3L, 4L)], tasks_b[c(3L, 4L)]))
  )
  expect_equal(a$backlog, character(0L))
  expect_equal(b$backlog, character(0L))
  expect_equal(x$pop_backlog(), character(0L))
})

crew_test("backlog with the first controller saturated`", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    name = "a",
    workers = 2L,
    seconds_idle = 360
  )
  b <- crew_controller_local(
    name = "b",
    workers = 2L,
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  expect_equal(a$backlog, character(0L))
  expect_equal(b$backlog, character(0L))
  expect_equal(x$pop_backlog(), character(0L))
  tasks_a <- paste0("a_", seq_len(4L))
  for (task in tasks_a) {
    x$push_backlog(name = task, controller = "a")
  }
  tasks_b <- paste0("b_", seq_len(4L))
  for (task in tasks_b) {
    x$push_backlog(name = task, controller = "b")
  }
  expect_equal(a$backlog, tasks_a)
  expect_equal(b$backlog, tasks_b)
  for (index in c(1L, 2L)) {
    x$push(Sys.sleep(30), controller = "a")
  }
  expect_equal(x$pop_backlog(), tasks_b[seq_len(2L)])
  expect_equal(a$backlog, tasks_a)
  expect_equal(b$backlog, tasks_b[c(3L, 4L)])
  expect_equal(x$pop_backlog(), tasks_b[c(3L, 4L)])
  expect_equal(a$backlog, tasks_a)
  expect_equal(b$backlog, character(0L))
  expect_equal(x$pop_backlog(), character(0L))
})

crew_test("backlog with the second controller saturated`", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    name = "a",
    workers = 2L,
    seconds_idle = 360
  )
  b <- crew_controller_local(
    name = "b",
    workers = 2L,
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  expect_equal(a$backlog, character(0L))
  expect_equal(b$backlog, character(0L))
  expect_equal(x$pop_backlog(), character(0L))
  tasks_a <- paste0("a_", seq_len(4L))
  for (task in tasks_a) {
    x$push_backlog(name = task, controller = "a")
  }
  tasks_b <- paste0("b_", seq_len(4L))
  for (task in tasks_b) {
    x$push_backlog(name = task, controller = "b")
  }
  expect_equal(a$backlog, tasks_a)
  expect_equal(b$backlog, tasks_b)
  for (index in c(1L, 2L)) {
    x$push(Sys.sleep(30), controller = "b")
  }
  expect_equal(x$pop_backlog(), tasks_a[seq_len(2L)])
  expect_equal(a$backlog, tasks_a[c(3L, 4L)])
  expect_equal(b$backlog, tasks_b)
  expect_equal(x$pop_backlog(), tasks_a[c(3L, 4L)])
  expect_equal(a$backlog, character(0L))
  expect_equal(b$backlog, tasks_b)
  expect_equal(x$pop_backlog(), character(0L))
})

crew_test("group helper methods (non)empty, (un)resolved, unpopped", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    name = "a",
    workers = 1L,
    seconds_idle = 360
  )
  b <- crew_controller_local(
    name = "b",
    workers = 1L,
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  expect_true(x$empty())
  expect_false(x$nonempty())
  expect_equal(x$resolved(), 0L)
  expect_equal(x$unresolved(), 0L)
  expect_equal(x$unpopped(), 0L)
  x$push(TRUE, controller = "a")
  x$push(TRUE, controller = "b")
  x$wait(mode = "all")
  expect_false(x$empty())
  expect_true(x$nonempty())
  expect_equal(x$resolved(), 2L)
  expect_equal(x$unresolved(), 0L)
  expect_equal(x$unpopped(), 2L)
  tasks <- x$collect()
  expect_true(x$empty())
  expect_false(x$nonempty())
  expect_equal(x$unpopped(), 0L)
  x$push(Sys.sleep(60), controller = "a")
  x$push(Sys.sleep(60), controller = "b")
  expect_false(x$empty())
  expect_true(x$nonempty())
  expect_equal(x$resolved(), 2L)
  expect_equal(x$unresolved(), 2L)
  expect_equal(x$unpopped(), 0L)
  x$terminate()
})

crew_test("descale", {
  controller <- crew_controller_local()
  x <- crew_controller_group(controller)
  expect_null(controller$autoscaling)
  x$descale()
  expect_false(controller$autoscaling)
})
