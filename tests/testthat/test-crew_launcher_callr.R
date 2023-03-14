crew_test("crew_launcher_callr() can run a task on a worker", {
  skip_on_cran()
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
  })
  expect_silent(launcher$validate())
  router$listen()
  launcher$populate(sockets = router$sockets)
  expect_equal(nrow(launcher$workers), 4L)
  expect_s3_class(launcher$workers$listener[[2]], "crew_null")
  expect_s3_class(launcher$workers$handle[[2]], "crew_null")
  socket <- launcher$workers$socket[2]
  expect_equal(launcher$active(), character(0))
  expect_false(dialer_connected(launcher$workers$listener[[2]]))
  expect_false(dialer_discovered(launcher$workers$listener[[2]]))
  launcher$launch(sockets = socket)
  expect_s3_class(launcher$workers$handle[[2]], "r_process")
  expect_silent(launcher$validate())
  crew::crew_wait(
    ~launcher$workers$handle[[2]]$is_alive(),
    timeout = 5,
    wait = 0.1
  )
  crew::crew_wait(
    ~{
      daemons <- mirai::daemons(.compute = router$name)$daemons
      status <- unname(daemons[, "status_online", drop = TRUE])[2L]
      length(status) == 1L && status > 0L
    },
    timeout = 5,
    wait = 0.1
  )
  crew_wait(
    ~identical(launcher$active(), socket),
    timeout = 5,
    wait = 0.1
  )
  expect_true(dialer_connected(launcher$workers$listener[[2]]))
  expect_true(dialer_discovered(launcher$workers$listener[[2]]))
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew::crew_wait(~!anyNA(m$data), timeout = 5, wait = 0.1)
  expect_equal(m$data, launcher$workers$handle[[2]]$get_pid())
  router$terminate()
  crew::crew_wait(
    ~{
      handle <- launcher$workers$handle[[2]]
      is_crew_null(handle) || !handle$is_alive()
    },
    timeout = 5,
    wait = 0.1
  )
  crew::crew_wait(
    ~{
      listener <- launcher$workers$listener[[2]]
      !dialer_connected(listener)
    },
    timeout = 5,
    wait = 0.1
  )
  crew_wait(
    ~identical(launcher$active(), character(0)),
    timeout = 5,
    wait = 0.1
  )
  expect_false(dialer_connected(launcher$workers$listener[[2]]))
  expect_true(dialer_discovered(launcher$workers$listener[[2]]))
})

crew_test("crew_launcher_callr() can run a task and time out a worker", {
  skip_on_cran()
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
  })
  router$listen()
  expect_silent(launcher$validate())
  socket <- router$sockets
  launcher$populate(sockets = router$sockets)
  expect_equal(launcher$active(), character(0))
  launcher$launch(sockets = router$sockets)
  crew::crew_wait(
    ~{
      handle <- launcher$workers$handle[[1]]
      !is_crew_null(handle) && handle$is_alive()
    },
    timeout = 5,
    wait = 0.1
  )
  crew_wait(
    ~identical(launcher$active(), socket),
    timeout = 5,
    wait = 0.1
  )
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew::crew_wait(~!anyNA(m$data), timeout = 5, wait = 0.1)
  expect_equal(m$data, launcher$workers$handle[[1]]$get_pid())
  crew::crew_wait(
    ~{
      handle <- launcher$workers$handle[[1]]
      is_crew_null(handle) || !handle$is_alive()
    },
    timeout = 5,
    wait = 0.1
  )
  crew::crew_wait(
    ~{
      daemons <- mirai::daemons(.compute = router$name)$daemons
      status <- unname(daemons[, "status_online", drop = TRUE])
      length(status) != 1L || status < 1L
    },
    timeout = 5,
    wait = 0.1
  )
  crew_wait(
    ~identical(launcher$active(), character()),
    timeout = 5,
    wait = 0.1
  )
  router$terminate()
})

crew_test("crew_launcher_callr() can run a task and end a worker", {
  skip_on_cran()
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
  })
  router$listen()
  socket <- router$sockets
  launcher$populate(sockets = router$sockets)
  expect_equal(launcher$active(), character(0))
  launcher$launch(sockets = router$sockets)
  crew::crew_wait(
    ~{
      daemons <- mirai::daemons(.compute = router$name)$daemons
      status <- unname(daemons[, "status_online", drop = TRUE])
      length(status) == 1L && status > 0L
    },
    timeout = 5,
    wait = 0.1
  )
  crew::crew_wait(
    ~{
      handle <- launcher$workers$handle[[1]]
      !is_crew_null(handle) && handle$is_alive()
    },
    timeout = 5,
    wait = 0.1
  )
  crew_wait(
    ~identical(launcher$active(), socket),
    timeout = 5,
    wait = 0.1
  )
  expect_silent(launcher$terminate())
  crew::crew_wait(
    ~{
      daemons <- mirai::daemons(.compute = router$name)$daemons
      status <- unname(daemons[, "status_online", drop = TRUE])
      length(status) != 1L || status < 1L
    },
    timeout = 5,
    wait = 0.1
  )
  crew::crew_wait(
    ~{
      handle <- launcher$workers$handle[[1]]
      is_crew_null(handle) || !handle$is_alive()
    },
    timeout = 5,
    wait = 0.1
  )
  crew_wait(
    ~identical(launcher$active(), character(0)),
    timeout = 5,
    wait = 0.1
  )
  router$terminate()
})

crew_test("worker that immediately times out is still discovered", {
  skip_on_cran()
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
  })
  router$listen()
  socket <- router$sockets
  launcher$populate(sockets = router$sockets)
  expect_equal(launcher$active(), character(0))
  launcher$launch(sockets = router$sockets)
  crew::crew_wait(
    ~{
      handle <- launcher$workers$handle[[1]]
      !is_crew_null(handle) && !handle$is_alive()
    },
    timeout = 5,
    wait = 0.1
  )
  crew_wait(
    ~identical(launcher$active(), character(0)),
    timeout = 5,
    wait = 0.1
  )
  expect_false(dialer_connected(launcher$workers$listener[[1]]))
  expect_true(dialer_discovered(launcher$workers$listener[[1]]))
})
