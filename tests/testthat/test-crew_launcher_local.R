crew_test("crew_launcher_local() active binding members", {
  launcher <- crew_launcher_local(
    name = "x",
    options_local = crew_options_local(
      log_directory = "y",
      log_join = FALSE
    )
  )
  expect_equal(launcher$options_local$log_directory, "y")
  expect_false(launcher$options_local$log_join)
})

crew_test("crew_launcher_local() log_prepare()", {
  skip_on_cran()
  dir <- tempfile()
  launcher <- crew_launcher_local(
    name = "x",
    options_local = crew_options_local(
      log_directory = dir,
      log_join = FALSE
    )
  )
  on.exit(unlink(dir, recursive = TRUE))
  private <- crew_private(launcher)
  expect_false(dir.exists(dir))
  private$.log_prepare()
  expect_true(dir.exists(dir))
})

crew_test("crew_launcher_local() joined log path", {
  skip_on_cran()
  launcher <- crew_launcher_local(
    name = "x",
    options_local = crew_options_local(
      log_directory = "dir",
      log_join = TRUE
    )
  )
  private <- crew_private(launcher)
  stdout <- private$.log_stdout(name = "x")
  stderr <- private$.log_stderr(name = "x")
  expect_equal(stdout, file.path("dir", "x.log"))
  expect_equal(stderr, "2>&1")
})

crew_test("crew_launcher_local() separate log paths", {
  skip_on_cran()
  launcher <- crew_launcher_local(
    name = "x",
    options_local = crew_options_local(
      log_directory = "dir",
      log_join = FALSE
    )
  )
  private <- crew_private(launcher)
  stdout <- private$.log_stdout(name = "x")
  stderr <- private$.log_stderr(name = "x")
  expect_equal(stdout, file.path("dir", "x-stdout.log"))
  expect_equal(stderr, file.path("dir", "x-stderr.log"))
})

crew_test("crew_launcher_local() can run a task on a worker", {
  skip_on_cran()
  skip_on_os("windows")
  client <- crew_client(host = "127.0.0.1", workers = 4L)
  launcher <- crew_launcher_local(name = client$profile, seconds_idle = 360)
  on.exit({
    client$terminate()
    launcher$terminate()
    rm(client)
    rm(launcher)
    gc()
    crew_test_sleep()
  })
  expect_silent(launcher$validate())
  client$start()
  launcher$start(url = client$url, profile = client$profile)
  launcher$launch()
  expect_s3_class(launcher$instances$handle[[1L]], "process")
  expect_silent(launcher$validate())
  crew::crew_retry(
    ~launcher$instances$handle[[1L]]$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  task <- mirai::mirai(ps::ps_pid(), .compute = client$profile)
  crew_retry(
    ~!nanonext::.unresolved(task),
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  expect_equal(task$data, launcher$instances$handle[[1L]]$get_pid())
  client$terminate()
  tryCatch(
    crew::crew_retry(
      ~!launcher$instances$handle[[1L]]$is_alive(),
      seconds_interval = 0.1,
      seconds_timeout = 5
    ),
    crew_expire = function(condition) {
      launcher$instances$handle[[1L]]$signal(signal = crew_terminate_signal())
    }
  )
})

crew_test("crew_launcher_local() worker tasks_max", {
  skip_on_cran()
  skip_on_os("windows")
  client <- crew_client(
    host = "127.0.0.1",
    workers = 1L
  )
  launcher <- crew_launcher_local(
    name = client$profile,
    tasks_max = 1L,
    seconds_idle = 360
  )
  on.exit({
    client$terminate()
    launcher$terminate()
    rm(client)
    rm(launcher)
    gc()
    crew_test_sleep()
  })
  client$start()
  expect_silent(launcher$validate())
  launcher$start(url = client$url, profile = client$profile)
  launcher$launch()
  crew::crew_retry(
    ~{
      handle <- launcher$instances$handle[[1]]
      !is_crew_null(handle) && handle$is_alive()
    },
    seconds_interval = 0.5,
    seconds_timeout = 30
  )
  task <- mirai::mirai(ps::ps_pid(), .compute = client$profile)
  crew::crew_retry(
    ~!nanonext::.unresolved(task),
    seconds_interval = 0.5,
    seconds_timeout = 30
  )
  expect_equal(task$data, launcher$instances$handle[[1]]$get_pid())
  crew::crew_retry(
    ~!launcher$instances$handle[[1]]$is_alive(),
    seconds_interval = 0.5,
    seconds_timeout = 30
  )
})

crew_test("crew_launcher_local() can terminate a worker", {
  skip_on_cran()
  skip_on_os("windows")
  client <- crew_client(
    host = "127.0.0.1",
    workers = 1L
  )
  launcher <- crew_launcher_local(
    name = client$profile,
    tasks_max = 1L,
    seconds_idle = 360
  )
  on.exit({
    client$terminate()
    launcher$terminate()
    rm(client)
    rm(launcher)
    gc()
    crew_test_sleep()
  })
  client$start()
  launcher$start(url = client$url, profile = client$profile)
  launcher$launch()
  handle <- launcher$instances$handle[[1L]]
  expect_s3_class(handle, "process")
  crew::crew_retry(
    ~handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  expect_silent(launcher$terminate())
  crew::crew_retry(
    ~!handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  expect_silent(launcher$terminate())
})

crew_test("deprecate seconds_exit", {
  suppressWarnings(crew_launcher_local(seconds_exit = 1))
  expect_true(TRUE)
})
