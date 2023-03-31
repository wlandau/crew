crew_test("connections on crew_null", {
  expect_false(connection_opened(crew_null))
  expect_true(connection_closed(crew_null))
  expect_false(dialer_connected(crew_null))
  expect_false(dialer_discovered(crew_null))
})

crew_test("condition_null", {
  expect_true(inherits(condition_null, "conditionVariable"))
 # expect_equal(cv_value(condition_null), 0L)
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

crew_test("listener-oriented connection functions", {
  skip_on_cran()
  port <- free_port()
  listen <- connection_listen(
    host = local_ip(),
    port = port,
    token = "example"
  )
  connection_wait_opened(listen)
  expect_equal(listen$state, "opened")
  expect_equal(
    listen$listener[[1]]$url,
    sprintf("ws://%s:%s/example", local_ip(), port)
  )
  expect_false(dialer_connected(listen))
  expect_false(dialer_discovered(listen))
  expect_true(dialer_not_discovered(listen))
  dial <- connection_dial(
    host = local_ip(),
    port = port,
    token = "example"
  )
  connection_wait_opened(dial)
  crew_wait(
    ~dialer_connected(listen),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  crew_wait(
    ~dialer_discovered(listen),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_true(dialer_discovered(listen))
  expect_false(dialer_not_discovered(listen))
  close(dial)
  connection_wait_closed(dial)
  crew_wait(
    ~!dialer_connected(listen),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_true(dialer_discovered(listen))
  expect_false(dialer_not_discovered(listen))
  close(listen)
  connection_wait_closed(listen)
  expect_false(dialer_connected(listen))
  expect_false(dialer_discovered(listen))
  expect_true(dialer_not_discovered(listen))
  crew_wait(
    ~identical(listen$state, "closed"),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_equal(listen$state, "closed")
})

crew_test("dialer-oriented connection functions", {
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
