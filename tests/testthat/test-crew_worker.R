crew_test("crew_worker_socket()", {
  exp <- "ws://127.0.32.1:50010/my_token"
  sockets <- c(
    "ws://127.0.32.1:50010",
    "ws://127.0.32.1:50010/1",
    "ws://127.0.32.1:50010/2",
    "ws://127.0.32.1:50010/5757"
  )
  for (socket in sockets) {
    expect_equal(crew_worker_socket(socket, token = "my_token"), exp)
  }
})

crew_test("crew_worker_connection()", {
  port <- free_port()
  token <- "this_token"
  listener <- connection_listen(port = port, suffix = token)
  expect_false(dialer_connected(listener))
  expect_false(dialer_discovered(listener))
  socket <- sprintf("ws://%s:%s/%s", local_ip(), port, token)
  dialer <- crew_worker_connection(socket = socket, token = token)
  crew_wait(~dialer_connected(listener), timeout = 5, wait = 0.001)
  crew_wait(~dialer_discovered(listener), timeout = 5, wait = 0.001)
  expect_true(dialer_connected(listener))
  expect_true(dialer_discovered(listener))
  close(dialer)
  connection_wait_closed(dialer)
  expect_false(dialer_connected(listener))
  expect_true(dialer_discovered(listener))
  close(listener)
  connection_wait_closed(listener)
})

crew_test("crew_worker() connects back to custom NNG bus socket", {
  port <- free_port()
  token <- "this_token"
  listener <- connection_listen(port = port, suffix = token)
  on.exit(close(listener))
  socket <- sprintf("ws://%s:%s/%s", local_ip(), port, token)
  settings <- list(url = socket, timerstart = 0L, idletime = 5)
  expect_false(dialer_connected(listener))
  expect_false(dialer_discovered(listener))
  crew_worker(settings = settings, token = token)
  crew_wait(~dialer_discovered(listener), timeout = 5, wait = 0.001)
  expect_true(dialer_discovered(listener))
})

crew_test("crew_worker() can run mirai tasks", {
  port <- free_port()
  socket <- sprintf("ws://%s:%s", local_ip(), port)
  mirai::daemons(n = 1L, url = socket)
  on.exit(mirai::daemons(n = 0L))
  m <- mirai::mirai("done")
  crew_wait(~mirai::daemons()$connections > 0L, timeout = 5, wait = 0.001)
  url <- rownames(mirai::daemons()$daemons)[1]
  settings <- list(url = url, maxtasks = 1L, cleanup = FALSE)
  crew_worker(settings = settings, token = "this_token")
  crew_wait(~identical(m$data, "done"), timeout = 5, wait = 0.001)
  expect_equal(m$data, "done")
})
