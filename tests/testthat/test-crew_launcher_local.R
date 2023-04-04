crew_test("crew_launcher_local() can run a task on a worker", {
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router(workers = 4L)
  launcher <- crew_launcher_local(seconds_idle = 360)
  on.exit({
    router$terminate()
    launcher$terminate()
    crew_test_sleep()
  })
  expect_silent(launcher$validate())
  router$listen()
  launcher$start(workers = 4L)
  expect_equal(nrow(launcher$workers), 4L)
  expect_s3_class(launcher$workers$handle[[2L]], "crew_null")
  socket <- router$sockets()[2L]
  expect_equal(launcher$workers$launches, rep(0L, 4L))
  launcher$launch(sockets = socket)
  expect_s3_class(launcher$workers$handle[[2L]], "r_process")
  expect_silent(launcher$validate())
  crew::crew_wait(
    ~launcher$workers$handle[[2L]]$is_alive(),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  crew::crew_wait(
    ~{
      daemons <- mirai::daemons(.compute = router$name)$daemons
      status <- unname(daemons[, "status_online", drop = TRUE])[2L]
      length(status) == 1L && status > 0L
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_equal(launcher$workers$launches, c(0L, 1L, 0L, 0L))
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew_wait(
    ~!anyNA(m$data),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_equal(m$data, launcher$workers$handle[[2L]]$get_pid())
  router$terminate()
  tryCatch(
    crew::crew_wait(
      ~{
        handle <- launcher$workers$handle[[2L]]
        is_crew_null(handle) || !handle$is_alive()
      },
      seconds_interval = 0.001,
      seconds_timeout = 5
    ),
    crew_expire = function(condition) {
      launcher$workers$handle[[2L]]$kill()
    }
  )
})

crew_test("crew_launcher_local() okay to not have sockets to launch", {
  skip_on_cran()
  skip_on_os("windows")
  launcher <- crew_launcher_local(seconds_idle = 360)
  expect_silent(launcher$launch(sockets = character(0L)))
})

crew_test("crew_launcher_local() can run a task and time out a worker", {
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router(workers = 1L)
  launcher <- crew_launcher_local(tasks_max = 1L, seconds_idle = 360)
  on.exit({
    router$terminate()
    launcher$terminate()
    crew_test_sleep()
  })
  router$listen()
  expect_silent(launcher$validate())
  socket <- router$sockets()
  launcher$start(workers = 1L)
  launcher$launch(sockets = socket)
  crew::crew_wait(
    ~{
      handle <- launcher$workers$handle[[1]]
      !is_crew_null(handle) && handle$is_alive()
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew::crew_wait(
    ~!anyNA(m$data),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_equal(m$data, launcher$workers$handle[[1]]$get_pid())
  crew::crew_wait(
    ~{
      handle <- launcher$workers$handle[[1]]
      is_crew_null(handle) || !handle$is_alive()
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  crew::crew_wait(
    ~{
      daemons <- mirai::daemons(.compute = router$name)$daemons
      status <- unname(daemons[, "status_online", drop = TRUE])
      length(status) != 1L || status < 1L
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  router$terminate()
})

crew_test("crew_launcher_local() can run a task and end a worker", {
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router(workers = 1L)
  launcher <- crew_launcher_local(tasks_max = 1L, seconds_idle = 360)
  on.exit({
    router$terminate()
    launcher$terminate()
    crew_test_sleep()
  })
  router$listen()
  socket <- router$sockets()
  launcher$start(workers = 1L)
  launcher$launch(sockets = socket)
  crew::crew_wait(
    ~{
      daemons <- mirai::daemons(.compute = router$name)$daemons
      status <- unname(daemons[, "status_online", drop = TRUE])
      length(status) == 1L && status > 0L
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  crew::crew_wait(
    ~{
      handle <- launcher$workers$handle[[1]]
      !is_crew_null(handle) && handle$is_alive()
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_silent(launcher$terminate())
  crew::crew_wait(
    ~{
      daemons <- mirai::daemons(.compute = router$name)$daemons
      status <- unname(daemons[, "status_online", drop = TRUE])
      length(status) != 1L || status < 1L
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  crew::crew_wait(
    ~{
      handle <- launcher$workers$handle[[1]]
      is_crew_null(handle) || !handle$is_alive()
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  router$terminate()
})
