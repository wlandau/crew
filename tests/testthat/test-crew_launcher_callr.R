crew_test("crew_launcher_callr() can run a task on a worker", {
  router <- crew_router(workers = 4L)
  launcher <- crew_launcher_callr(seconds_idle = 360)
  crew_port_set()
  on.exit({
    crew_port_unset()
    router$terminate()
    launcher$terminate()
  })
  expect_silent(launcher$validate())
  router$listen()
  launcher$populate(sockets = router$sockets)
  expect_equal(nrow(launcher$workers), 4L)
  expect_s3_class(launcher$workers$listener[[2]], "crew_null")
  expect_s3_class(launcher$workers$handle[[2]], "crew_null")
  launcher$launch(sockets = launcher$workers$socket[2])
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
})

crew_test("crew_launcher_callr() can run a task and time out a worker", {
  skip_on_cran()
  router <- crew_router(workers = 1L)
  launcher <- crew_launcher_callr(tasks_max = 1L, seconds_idle = 360)
  crew_port_set()
  on.exit({
    crew_port_unset()
    router$terminate()
    launcher$terminate()
  })
  router$listen()
  expect_silent(launcher$validate())
  launcher$populate(sockets = router$sockets)
  launcher$launch(sockets = router$sockets)
  crew::crew_wait(
    ~{
      handle <- launcher$workers$handle[[1]]
      !is_crew_null(handle) && handle$is_alive()
    },
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
  router$terminate()
})

crew_test("crew_launcher_callr() can run a task and end a worker", {
  skip_on_cran()
  router <- crew_router(workers = 1L)
  launcher <- crew_launcher_callr(tasks_max = 1L, seconds_idle = 360)
  crew_port_set()
  on.exit({
    crew_port_unset()
    router$terminate()
    launcher$terminate()
  })
  router$listen()
  launcher$populate(sockets = router$sockets)
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
  router$terminate()
})
