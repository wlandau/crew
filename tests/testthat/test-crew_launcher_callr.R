crew_test("crew_launcher_callr() can run a task on a worker", {
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router(
    workers = 4L,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  launcher <- crew_launcher_callr(seconds_idle = 360)
  crew_session_start()
  on.exit({
    crew_session_terminate()
    router$terminate()
    launcher$terminate()
    crew_test_sleep()
  })
  expect_silent(launcher$validate())
  router$listen()
  launcher$populate(sockets = router$daemons$worker_socket)
  expect_equal(nrow(launcher$workers), 4L)
  expect_s3_class(launcher$workers$listener[[2L]], "crew_null")
  expect_s3_class(launcher$workers$handle[[2L]], "crew_null")
  socket <- launcher$workers$socket[2L]
  expect_equal(launcher$active(), character(0L))
  expect_equal(launcher$workers$launches, rep(0L, 4L))
  expect_false(dialer_connected(launcher$workers$listener[[2]]))
  expect_false(dialer_discovered(launcher$workers$listener[[2]]))
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
  crew_wait(
    ~identical(launcher$active(), socket),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_true(dialer_connected(launcher$workers$listener[[2L]]))
  expect_true(dialer_discovered(launcher$workers$listener[[2L]]))
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
  crew::crew_wait(
    ~{
      listener <- launcher$workers$listener[[2L]]
      !dialer_connected(listener)
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  crew_wait(
    ~identical(launcher$active(), character(0)),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_false(dialer_connected(launcher$workers$listener[[2L]]))
  expect_true(dialer_discovered(launcher$workers$listener[[2L]]))
})

crew_test("crew_launcher_callr() can run a task and time out a worker", {
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router(
    workers = 1L,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  launcher <- crew_launcher_callr(tasks_max = 1L, seconds_idle = 360)
  crew_session_start()
  on.exit({
    crew_session_terminate()
    router$terminate()
    launcher$terminate()
    crew_test_sleep()
  })
  router$listen()
  expect_silent(launcher$validate())
  socket <- router$daemons$worker_socket
  launcher$populate(sockets = router$daemons$worker_socket)
  expect_equal(launcher$active(), character(0))
  launcher$launch(sockets = router$daemons$worker_socket)
  crew::crew_wait(
    ~{
      handle <- launcher$workers$handle[[1]]
      !is_crew_null(handle) && handle$is_alive()
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  crew_wait(
    ~identical(launcher$active(), socket),
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
  crew_wait(
    ~identical(launcher$active(), character()),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  router$terminate()
})

crew_test("crew_launcher_callr() can run a task and end a worker", {
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router(
    workers = 1L,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  launcher <- crew_launcher_callr(tasks_max = 1L, seconds_idle = 360)
  crew_session_start()
  on.exit({
    crew_session_terminate()
    router$terminate()
    launcher$terminate()
    crew_test_sleep()
  })
  router$listen()
  socket <- router$daemons$worker_socket
  launcher$populate(sockets = router$daemons$worker_socket)
  expect_equal(launcher$active(), character(0))
  launcher$launch(sockets = router$daemons$worker_socket)
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
  crew_wait(
    ~identical(launcher$active(), socket),
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
  crew_wait(
    ~identical(launcher$active(), character(0)),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  router$terminate()
})

crew_test("worker that immediately times out is still discovered", {
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router(
    workers = 1L,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  launcher <- crew_launcher_callr(
    tasks_timers = 0L,
    seconds_launch = 360,
    seconds_idle = 0.1
  )
  crew_session_start()
  on.exit({
    crew_session_terminate()
    router$terminate()
    launcher$terminate()
    crew_test_sleep()
  })
  router$listen()
  socket <- router$daemons$worker_socket
  launcher$populate(sockets = router$daemons$worker_socket)
  expect_equal(launcher$active(), character(0))
  launcher$launch(sockets = router$daemons$worker_socket)
  crew::crew_wait(
    ~{
      handle <- launcher$workers$handle[[1]]
      !is_crew_null(handle) && !handle$is_alive()
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  crew_wait(
    ~identical(launcher$active(), character(0)),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_false(dialer_connected(launcher$workers$listener[[1]]))
  expect_true(dialer_discovered(launcher$workers$listener[[1]]))
})

crew_test("launcher cleans up old worker", {
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router(
    workers = 1L,
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  launcher <- crew_launcher_callr(
    tasks_timers = 0L,
    seconds_launch = 360,
    seconds_idle = 360
  )
  crew_session_start()
  on.exit({
    crew_session_terminate()
    launcher$terminate()
    router$terminate()
    crew_test_sleep()
  })
  router$listen()
  socket <- router$daemons$worker_socket
  launcher$populate(sockets = router$daemons$worker_socket)
  launcher$launch(sockets = socket)
  handle <- launcher$workers$handle[[1]]
  crew_wait(
    ~handle$is_alive(),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_true(handle$is_alive())
  launcher$launch(sockets = socket)
  crew_wait(
    ~!handle$is_alive(),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_false(handle$is_alive())
})
