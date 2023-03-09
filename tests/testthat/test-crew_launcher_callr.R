crew_test("crew_launcher_callr() can run a task on a worker", {
  router <- crew_router(workers = 1L)
  launcher <- crew_launcher_callr(seconds_idle = 360)
  on.exit({
    router$disconnect()
    launcher$terminate()
  })
  expect_silent(launcher$validate())
  router$listen()
  expect_equal(router$connected(), character(0))
  launcher$populate(sockets = router$sockets())
  launcher$launch(sockets = router$unoccupied())
  expect_equal(length(launcher$processes), 1L)
  expect_silent(launcher$validate())
  crew::crew_wait(
    ~identical(length(router$connected()), 1L),
    timeout = 5,
    wait = 0.1
  )
  crew::crew_wait(
    ~launcher$processes[[1]]$is_alive(),
    timeout = 5,
    wait = 0.1
  )
  expect_equal(length(router$connected()), 1L)
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew::crew_wait(~!anyNA(m$data), timeout = 5, wait = 0.1)
  expect_equal(m$data, launcher$processes[[1]]$get_pid())
  router$disconnect()
  crew::crew_wait(
    ~!launcher$processes[[1]]$is_alive(),
    timeout = 5,
    wait = 0.1
  )
  expect_equal(router$connected(), character(0))
})

crew_test("crew_launcher_callr() can run a task and time out a worker", {
  skip_on_cran()
  router <- crew_router()
  launcher <- crew_launcher_callr(max_tasks = 1L, idle_time = 360)
  on.exit({
    router$disconnect()
    launcher$terminate()
  })
  router$connect()
  expect_silent(launcher$validate())
  launcher$launch(sockets = router$unoccupied())
  crew::crew_wait(
    ~identical(length(router$occupied()), 1L),
    timeout = 5,
    wait = 0.1
  )
  crew::crew_wait(
    ~launcher$processes[[1]]$is_alive(),
    timeout = 5,
    wait = 0.1
  )
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew::crew_wait(~!anyNA(m$data), timeout = 5, wait = 0.1)
  expect_equal(m$data, launcher$processes[[1]]$get_pid())
  crew::crew_wait(
    ~!launcher$processes[[1]]$is_alive(),
    timeout = 5,
    wait = 0.1
  )
  crew::crew_wait(
    ~identical(length(router$occupied()), 0L),
    timeout = 5,
    wait = 0.1
  )
  expect_equal(router$occupied(), character(0))
  router$disconnect()
})

crew_test("crew_launcher_callr() can run a task and end a worker", {
  skip_on_cran()
  router <- crew_router()
  launcher <- crew_launcher_callr(max_tasks = 1L, idle_time = 360)
  on.exit({
    router$disconnect()
    launcher$terminate()
  })
  router$connect()
  launcher$launch(sockets = router$unoccupied())
  crew::crew_wait(
    ~identical(length(router$occupied()), 1L),
    timeout = 5,
    wait = 0.1
  )
  crew::crew_wait(
    ~launcher$processes[[1]]$is_alive(),
    timeout = 5,
    wait = 0.1
  )
  expect_silent(launcher$terminate())
  crew::crew_wait(
    ~identical(length(router$occupied()), 0L),
    timeout = 5,
    wait = 0.1
  )
  crew::crew_wait(
    ~!launcher$processes[[1]]$is_alive(),
    timeout = 5,
    wait = 0.1
  )
  router$disconnect()
})
