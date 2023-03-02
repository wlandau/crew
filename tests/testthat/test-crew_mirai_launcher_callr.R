crew_test("crew_mirai_launcher_callr() can run a task on a worker", {
  router <- crew_mirai_router()
  launcher <- crew_mirai_launcher_callr()
  expect_silent(launcher$validate())
  expect_equal(launcher$running(), 0L)
  router$connect()
  launcher$populate(sockets = router$sockets_listening())
  expect_silent(launcher$validate())
  expect_equal(router$sockets_occupied(), character(0))
  expect_equal(launcher$running(), 0L)
  launcher$launch(n = 1L)
  expect_equal(launcher$running(), 1L)
  crew::crew_wait(
    ~identical(router$sockets_available(), character(0)),
    timeout = 5,
    wait = 0.1
  )
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew::crew_wait(~!anyNA(m$data), timeout = 5, wait = 0.1)
  expect_equal(m$data, launcher$workers[[1]]$get_pid())
  router$disconnect()
  crew::crew_wait(~!launcher$running(), timeout = 5, wait = 0.1)
  expect_equal(router$sockets_occupied(), character(0))
})

crew_test("crew_mirai_launcher_callr() can run a task and then exit worker", {
  router <- crew_mirai_router()
  launcher <- crew_mirai_launcher_callr(max_tasks = 1L)
  router$connect()
  launcher$populate(sockets = router$sockets_listening())
  expect_silent(launcher$validate())
  launcher$launch(n = 1L)
  crew::crew_wait(
    ~identical(router$sockets_available(), character(0)),
    timeout = 5,
    wait = 0.1
  )
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew::crew_wait(~!anyNA(m$data), timeout = 5, wait = 0.1)
  expect_equal(m$data, launcher$workers[[1]]$get_pid())
  crew::crew_wait(~!launcher$running(), timeout = 5, wait = 0.1)
  expect_equal(router$sockets_occupied(), character(0))
  router$disconnect()
})
