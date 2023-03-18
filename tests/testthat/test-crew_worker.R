crew_test("crew_worker() connects back to custom NNG bus socket", {
  skip_on_cran()
  crew_session_start()
  on.exit(crew_session_terminate())
  port <- free_port()
  token <- "this_token"
  listener <- connection_listen(
    host = local_ip(),
    port = crew_session_port(),
    token = token
  )
  on.exit(close(listener), add = TRUE)
  on.exit(crew_test_sleep(), add = TRUE)
  socket <- sprintf("ws://%s:%s/%s", local_ip(), port, token)
  settings <- list(url = socket, timerstart = 0L, idletime = 5)
  expect_false(dialer_connected(listener))
  expect_false(dialer_discovered(listener))
  crew_worker(
    settings = settings,
    host = local_ip(),
    port = crew_session_port(),
    token = token
  )
  crew_wait(
    ~dialer_discovered(listener),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_true(dialer_discovered(listener))
})

crew_test("crew_worker() can run mirai tasks and assigns env vars", {
  skip_on_cran()
  skip_on_os("windows")
  previous <- Sys.getenv(c("CREW_SOCKET_DATA", "CREW_SOCKET_SESSION"))
  Sys.unsetenv(c("CREW_SOCKET_DATA", "CREW_SOCKET_SESSION"))
  on.exit(do.call(what = Sys.setenv, args = as.list(previous)))
  crew_session_start()
  on.exit(crew_session_terminate(), add = TRUE)
  port <- free_port()
  socket <- sprintf("ws://%s:%s", local_ip(), port)
  mirai::daemons(n = 1L, url = socket)
  on.exit(mirai::daemons(n = 0L), add = TRUE)
  on.exit(crew_test_sleep(), add = TRUE)
  m <- mirai::mirai(
    list(
      mirai = Sys.getenv("CREW_SOCKET_DATA"),
      session = Sys.getenv("CREW_SOCKET_SESSION")
    )
  )
  crew_wait(
    ~mirai::daemons()$connections > 0L,
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  url <- rownames(mirai::daemons()$daemons)[1]
  settings <- list(url = url, maxtasks = 1L, cleanup = FALSE)
  token <- "this_token"
  crew_worker(
    settings = settings,
    host = local_ip(),
    port = crew_session_port(),
    token = token
  )
  session <- connection_socket(
    host = local_ip(),
    port = crew_session_port(),
    token = token
  )
  exp <- list(mirai = socket, session = session)
  crew_wait(
    ~identical(m$data, exp),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_equal(m$data, exp)
  expect_equal(Sys.getenv("CREW_SOCKET_DATA", unset = ""), "")
  expect_equal(Sys.getenv("CREW_SOCKET_SESSION", unset = ""), "")
})
