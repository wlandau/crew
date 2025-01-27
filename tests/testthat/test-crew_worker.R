crew_test("crew_worker() can run mirai tasks and assigns env vars", {
  skip_on_cran()
  skip_on_os("windows")
  envvars <- c("CREW_CONTROLLER", "CREW_WORKER")
  previous <- Sys.getenv(envvars)
  Sys.unsetenv(envvars)
  on.exit(do.call(what = Sys.setenv, args = as.list(previous)))
  profile <- basename(tempfile())
  mirai::daemons(
    n = 1L,
    url = "tcp://127.0.0.1:0",
    dispatcher = TRUE,
    .compute = profile
  )
  on.exit(mirai::daemons(n = 0L, .compute = profile), add = TRUE)
  on.exit(crew_test_sleep(), add = TRUE)
  task <- mirai::mirai(
    .expr = list(
      controller = Sys.getenv("CREW_CONTROLLER"),
      worker = Sys.getenv("CREW_WORKER")
    ),
    .compute = profile
  )
  url <- mirai::status(.compute = profile)$daemons
  settings <- list(url = url, maxtasks = 1L, cleanup = 0L, dispatcher = TRUE)
  crew_worker(
    settings = settings,
    launcher = "my_controller",
    worker = "worker_id",
    options_metrics = NULL
  )
  crew_retry(
    ~!nanonext::unresolved(task),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  data <- task$data
  expect_true(is.list(data))
  expect_equal(data$controller, "my_controller")
  expect_equal(data$worker, "worker_id")
  for (var in envvars) {
    expect_equal(Sys.getenv(var, unset = ""), "")
  }
})

crew_test("crew_worker() metrics logging to a directory", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("autometric", minimum_version = "0.1.0")
  envvars <- c("CREW_CONTROLLER", "CREW_WORKER")
  previous <- Sys.getenv(envvars)
  Sys.unsetenv(envvars)
  on.exit(do.call(what = Sys.setenv, args = as.list(previous)))
  mirai::daemons(
    n = 1L,
    url = "tcp://127.0.0.1:0",
    dispatcher = TRUE
  )
  on.exit(mirai::daemons(n = 0L), add = TRUE)
  on.exit(crew_test_sleep(), add = TRUE)
  m <- mirai::mirai({
    Sys.sleep(2)
    list(
      controller = Sys.getenv("CREW_CONTROLLER"),
      worker = Sys.getenv("CREW_WORKER")
    )
  })
  envir <- new.env(parent = emptyenv())
  url <- mirai::status()$daemons
  settings <- list(url = url, maxtasks = 1L, cleanup = 0L, dispatcher = TRUE)
  log <- tempfile()
  crew_worker(
    settings = settings,
    launcher = "my_controller",
    worker = "worker_id",
    options_metrics = crew_options_metrics(
      path = log,
      seconds_interval = 0.25
    )
  )
  crew_retry(
    ~!nanonext::.unresolved(m$data),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  expect_equal(m$data$controller, "my_controller")
  expect_equal(m$data$worker, "worker_id")
  for (var in envvars) {
    expect_equal(Sys.getenv(var, unset = ""), "")
  }
  expect_true(dir.exists(log))
  data <- autometric::log_read(log)
  expect_true(is.data.frame(data))
  expect_gt(nrow(data), 0L)
  expect_equal(unique(data$status), 0L)
})
