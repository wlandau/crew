crew_test("crew_worker() connects back to custom NNG bus socket", {
  crew_port_set()
  on.exit(crew_port_unset())
  port <- free_port()
  token <- "this_token"
  listener <- connection_listen(port = crew_port_get(), suffix = token)
  on.exit(close(listener), add = TRUE)
  socket <- sprintf("ws://%s:%s/%s", local_ip(), port, token)
  settings <- list(url = socket, timerstart = 0L, idletime = 5)
  expect_false(dialer_connected(listener))
  expect_false(dialer_discovered(listener))
  crew_worker(
    settings = settings,
    host = local_ip(),
    port = crew_port_get(),
    token = token
  )
  crew_wait(~dialer_discovered(listener), timeout = 5, wait = 0.001)
  expect_true(dialer_discovered(listener))
})

crew_test("crew_worker() can run mirai tasks", {
  crew_port_set()
  on.exit(crew_port_unset())
  port <- free_port()
  socket <- sprintf("ws://%s:%s", local_ip(), port)
  mirai::daemons(n = 1L, url = socket)
  on.exit(mirai::daemons(n = 0L), add = TRUE)
  m <- mirai::mirai("done")
  crew_wait(~mirai::daemons()$connections > 0L, timeout = 5, wait = 0.001)
  url <- rownames(mirai::daemons()$daemons)[1]
  settings <- list(url = url, maxtasks = 1L, cleanup = FALSE)
  crew_worker(
    settings = settings,
    host = local_ip(),
    port = crew_port_get(),
    token = "this_token"
  )
  crew_wait(~identical(m$data, "done"), timeout = 5, wait = 0.001)
  expect_equal(m$data, "done")
})
