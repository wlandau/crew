crew_test("crew_worker() can run mirai tasks and assigns env vars", {
  skip_on_cran()
  skip_on_os("windows")
  envvars <- c("CREW_LAUNCHER", "CREW_WORKER", "CREW_INSTANCE")
  previous <- Sys.getenv(envvars)
  Sys.unsetenv(envvars)
  on.exit(do.call(what = Sys.setenv, args = as.list(previous)))
  mirai::daemons(
    n = 1L,
    url = "ws://127.0.0.1:0",
    dispatcher = TRUE,
    token = TRUE
  )
  on.exit(mirai::daemons(n = 0L), add = TRUE)
  on.exit(crew_test_sleep(), add = TRUE)
  m <- mirai::mirai(
    list(
      launcher = Sys.getenv("CREW_LAUNCHER"),
      worker = Sys.getenv("CREW_WORKER"),
      instance = Sys.getenv("CREW_INSTANCE")
    )
  )
  envir <- new.env(parent = emptyenv())
  crew_retry(
    ~{
      envir$daemons <- rlang::duplicate(
        x = mirai::status()$daemons,
        shallow = FALSE
      )
      is.matrix(envir$daemons) && all(dim(envir$daemons) > 0L)
    },
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  url <- rownames(envir$daemons)[1]
  settings <- list(url = url, maxtasks = 1L, cleanup = 0L)
  instance <- parse_instance(url)
  crew_worker(
    settings = settings,
    launcher = "my_launcher",
    worker = 4L,
    instance = instance
  )
  exp <- list(
    launcher = "my_launcher",
    worker = 4L,
    instance = instance
  )
  crew_retry(
    ~!nanonext::.unresolved(m$data),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  expect_equal(m$data$launcher, "my_launcher")
  expect_equal(as.integer(m$data$worker), 4L)
  expect_equal(m$data$instance, instance)
  for (var in envvars) {
    expect_equal(Sys.getenv(var, unset = ""), "")
  }
})
