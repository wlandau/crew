crew_test("connections on crew_null", {
  expect_false(dialer_connected(crew_null))
  expect_false(dialer_discovered(crew_null))
})

crew_test("connections", {
  port <- free_port()
  listen <- connection_listen(port = port, suffix = "example")
  expect_equal(listen$state, "opened")
  expect_equal(
    listen$listener[[1]]$url,
    sprintf("ws://%s:%s/example", local_ip(), port)
  )
  expect_false(dialer_connected(listen))
  expect_false(dialer_discovered(listen))
  dial <- connection_dial(port = port, suffix = "example")
  crew_wait(~dialer_connected(listen), timeout = 5, wait = 0.001)
  crew_wait(~dialer_discovered(listen), timeout = 5, wait = 0.001)
  expect_true(dialer_discovered(listen))
  close(dial)
  connection_wait_closed(dial)
  expect_false(dialer_connected(listen))
  expect_true(dialer_discovered(listen))
  close(listen)
  expect_false(dialer_connected(listen))
  expect_false(dialer_discovered(listen))
  crew_wait(~identical(listen$state, "closed"), timeout = 5, wait = 0.1)
  expect_equal(listen$state, "closed")
})
