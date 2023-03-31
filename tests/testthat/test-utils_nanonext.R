crew_test("connections on crew_null", {
  expect_false(connection_opened(crew_null))
  expect_true(connection_closed(crew_null))
  expect_false(listener_connected(crew_null))
})

crew_test("condition variables", {
  port <- free_port()
  listen <- connection_listen(
    host = local_ip(),
    port = port,
    token = "example"
  )
  on.exit(close(listen))
  connection_wait_opened(listen)
  cv <- condition_variable(listen)
  expect_equal(cv_value(cv), 0L)
  dial <- connection_dial(
    host = local_ip(),
    port = port,
    token = "example"
  )
  connection_wait_opened(dial)
  crew_wait(
    ~all(cv_value(cv) == 1L),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_equal(cv_value(cv), 1L)
  close(dial)
  connection_wait_closed(dial)
  crew_wait(
    ~all(cv_value(cv) == 2L),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_equal(cv_value(cv), 2L)
})

crew_test("listener_connected()", {
  skip_on_cran()
  port <- free_port()
  dial <- connection_dial(
    host = local_ip(),
    port = port,
    token = "example"
  )
  connection_wait_opened(dial)
  expect_equal(dial$state, "opened")
  expect_equal(
    dial$dialer[[1]]$url,
    sprintf("ws://%s:%s/example", local_ip(), port)
  )
  expect_false(listener_connected(dial))
  listen <- connection_listen(
    host = local_ip(),
    port = port,
    token = "example"
  )
  connection_wait_opened(listen)
  crew_wait(
    ~listener_connected(dial),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_true(listener_connected(dial))
  close(listen)
  connection_wait_closed(listen)
  crew_wait(
    ~!listener_connected(dial),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_false(listener_connected(dial))
  close(dial)
  connection_wait_closed(dial)
  crew_wait(
    ~identical(dial$state, "closed"),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_equal(dial$state, "closed")
})
