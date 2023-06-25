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
  x$start()
  expect_true(x$empty())
  expect_false(x$saturated())
  expect_true(x$empty(controllers = "a"))
  expect_true(x$empty(controllers = "b"))
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
  x$push(
    command = ps::ps_pid(),
    name = "task_pid",
    controller = "b",
    save_command = TRUE
  )
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
  for (index in seq_len(2)) {
    expect_false(x$controllers[[index]]$client$started)
    crew_retry(
      ~!handle$is_alive(),
      seconds_interval = 0.1,
      seconds_timeout = 5
    )
  }
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

crew_test("crew_controller_group() collect", {
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
  expect_silent(x$validate())
  expect_null(x$controllers[[1]]$client$started)
  expect_null(x$controllers[[2]]$client$started)
  x$start()
  expect_null(x$pop())
  x$push(command = ps::ps_pid(), name = "task_pid")
  crew_retry(
    fun = ~{
      x$scale()
      x$collect()
      length(x$controllers[[1]]$schedule$collected) > 0L
    },
    seconds_interval = 0.5,
    seconds_timeout = 10
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
  expect_silent(x$scale())
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
})
