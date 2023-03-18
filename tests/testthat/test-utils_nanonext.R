crew_test("connections on crew_null", {
  expect_false(dialer_connected(crew_null))
  expect_false(dialer_discovered(crew_null))
})

crew_test("connections", {
  skip_on_cran()
  port <- free_port()
  listen <- connection_listen(
    host = local_ip(),
    port = port,
    token = "example"
  )
  expect_equal(listen$state, "opened")
  expect_equal(
    listen$listener[[1]]$url,
    sprintf("ws://%s:%s/example", local_ip(), port)
  )
  expect_false(dialer_connected(listen))
  expect_false(dialer_discovered(listen))
  dial <- connection_dial(
    host = local_ip(),
    port = port,
    token = "example"
  )
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
  close(dial)
  connection_wait_closed(dial)
  crew_wait(
    ~!dialer_connected(listen),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_true(dialer_discovered(listen))
  close(listen)
  connection_wait_closed(listen)
  expect_false(dialer_connected(listen))
  expect_false(dialer_discovered(listen))
  crew_wait(
    ~identical(listen$state, "closed"),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_equal(listen$state, "closed")
})
