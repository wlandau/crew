crew_test("crew_launcher_local() active binding members", {
  launcher <- crew_launcher_local(
    name = "x",
    local_log_directory = "y",
    local_log_join = FALSE
  )
  expect_equal(launcher$local_log_directory, "y")
  expect_false(launcher$local_log_join)
})

crew_test("crew_launcher_local() log_prepare()", {
  skip_on_cran()
  dir <- tempfile()
  launcher <- crew_launcher_local(
    name = "x",
    local_log_directory = dir,
    local_log_join = FALSE
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
    local_log_directory = "dir",
    local_log_join = TRUE
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
    local_log_directory = "dir",
    local_log_join = FALSE
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
  launcher <- crew_launcher_local(name = client$name, seconds_idle = 360)
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
  launcher$start()
  expect_equal(nrow(launcher$workers), 4L)
  expect_s3_class(launcher$workers$handle[[2L]], "crew_null")
  expect_equal(launcher$workers$launches, rep(0L, 4L))
  launcher$launch(index = 2L)
  log <- client$summary()
  socket <- log$socket[2L]
  expect_s3_class(launcher$workers$handle[[2L]], "process")
  expect_silent(launcher$validate())
  crew::crew_retry(
    ~launcher$workers$handle[[2L]]$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  crew::crew_retry(
    ~{
      daemons <- rlang::duplicate(
        x = mirai::status(.compute = client$name)$daemons,
        shallow = FALSE
      )
      if (is.null(nrow(daemons))) {
        return(FALSE)
      }
      status <- unname(daemons[, "online", drop = TRUE])[2L]
      length(status) == 1L && status > 0L
    },
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  expect_equal(launcher$workers$launches, c(0L, 1L, 0L, 0L))
  m <- mirai::mirai(ps::ps_pid(), .compute = client$name)
  crew_retry(
    ~!unresolved(m),
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  expect_equal(m$data, launcher$workers$handle[[2L]]$get_pid())
  client$terminate()
  tryCatch(
    crew::crew_retry(
      ~!launcher$workers$handle[[2L]]$is_alive(),
      seconds_interval = 0.1,
      seconds_timeout = 5
    ),
    crew_expire = function(condition) {
      launcher$workers$handle[[2L]]$signal(signal = crew_terminate_signal())
    }
  )
})

crew_test("crew_launcher_local() can run a task and time out a worker", {
  skip_on_cran()
  skip_on_os("windows")
  client <- crew_client(
    host = "127.0.0.1",
    workers = 1L
  )
  launcher <- crew_launcher_local(
    name = client$name,
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
  launcher$start()
  launcher$launch(index = 1L)
  crew::crew_retry(
    ~{
      handle <- launcher$workers$handle[[1]]
      !is_crew_null(handle) && handle$is_alive()
    },
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  m <- mirai::mirai(ps::ps_pid(), .compute = client$name)
  crew::crew_retry(
    ~!unresolved(m),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  expect_equal(m$data, launcher$workers$handle[[1]]$get_pid())
  crew::crew_retry(
    ~!launcher$workers$handle[[1]]$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  crew::crew_retry(
    ~{
      daemons <- rlang::duplicate(
        x = mirai::status(.compute = client$name)$daemons,
        shallow = FALSE
      )
      status <- unname(daemons[, "online", drop = TRUE])
      length(status) != 1L || status < 1L
    },
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
})

crew_test("crew_launcher_local() can run a task and end a worker", {
  skip_on_cran()
  skip_on_os("windows")
  client <- crew_client(
    host = "127.0.0.1",
    workers = 1L
  )
  launcher <- crew_launcher_local(
    name = client$name,
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
  socket <- rownames(client$daemons)
  launcher$start()
  expect_true(launcher$workers$terminated[1L])
  launcher$launch(index = 1L)
  expect_false(launcher$workers$terminated[1L])
  crew::crew_retry(
    ~{
      daemons <- rlang::duplicate(
        x = mirai::status(.compute = client$name)$daemons,
        shallow = FALSE
      )
      status <- unname(daemons[, "online", drop = TRUE])
      length(status) == 1L && status > 0L
    },
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  crew::crew_retry(
    ~launcher$workers$handle[[1L]]$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  expect_false(launcher$workers$terminated[1L])
  pid <- launcher$workers$handle[[1L]]$get_pid()
  expect_s3_class(launcher$workers$handle[[1L]], "process")
  expect_silent(launcher$terminate())
  expect_s3_class(launcher$workers$handle[[1L]], "process")
  expect_true(launcher$workers$terminated[1L])
  crew::crew_retry(
    ~{
      daemons <- rlang::duplicate(
        x = mirai::status(.compute = client$name)$daemons,
        shallow = FALSE
      )
      status <- unname(daemons[, "online", drop = TRUE])
      length(status) != 1L || status < 1L
    },
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  crew::crew_retry(
    ~!launcher$workers$handle[[1]]$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  expect_equal(launcher$workers$termination[[1L]]$pid, pid)
  expect_silent(launcher$terminate())
  expect_true(launcher$workers$terminated[1L])
})

crew_test("joined logs", {
  skip_on_cran()
  skip_on_covr()
  skip_on_os("windows")
  dir <- tempfile()
  x <- crew_controller_local(
    workers = 1L,
    seconds_idle = 60,
    local_log_directory = dir,
    local_log_join = TRUE
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
  x$push(message("this-message"))
  x$push(warning("this-warning"))
  x$push(stop("this-stop"))
  x$wait(mode = "all")
  Sys.sleep(0.25)
  dir <- x$launcher$local_log_directory
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
    local_log_directory = dir,
    local_log_join = FALSE
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
  x$push(message("this-message"))
  x$push(warning("this-warning"))
  x$push(stop("this-stop"))
  x$wait(mode = "all")
  Sys.sleep(0.25)
  logs <- list.files(dir, full.names = TRUE)
  expect_equal(length(logs), 2L)
  stderr <- readLines(logs[1L])
  stdout <- readLines(logs[2L])
  expect_true(any(grepl("this-print", stdout, fixed = TRUE)))
  expect_true(any(grepl("this-message", stderr, fixed = TRUE)))
  expect_true(any(grepl("Warning: this-warning", stderr, fixed = TRUE)))
  expect_true(any(grepl("Error: this-stop", stderr, fixed = TRUE)))
})

crew_test("deprecate seconds_exit", {
  suppressWarnings(crew_launcher_local(seconds_exit = 1))
  expect_true(TRUE)
})
