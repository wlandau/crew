crew_test("crew_client() validate on an empty object", {
  client <- crew_client(host = "127.0.0.1")
  expect_silent(client$validate())
})

crew_test("crew_client() active bindings", {
  client <- crew_client(
    host = "127.0.0.1",
    port = 123L,
    seconds_interval = 123,
    seconds_timeout = 456,
    retry_tasks = FALSE
  )
  expect_equal(client$host, "127.0.0.1")
  expect_equal(client$port, 123L)
  expect_equal(client$seconds_interval, 123)
  expect_equal(client$seconds_timeout, 456)
  expect_false(client$retry_tasks)
  expect_true(inherits(client$tls, "crew_class_tls"))
  expect_silent(client$validate())
})

crew_test("crew_client() works", {
  skip_on_cran()
  skip_on_os("windows")
  client <- crew_client(host = "127.0.0.1")
  on.exit({
    client$terminate()
    rm(client)
    crew_test_sleep()
  })
  expect_null(client$started)
  expect_null(client$dispatcher)
  expect_silent(client$start())
  expect_true(client$started)
  log <- client$summary()
  expect_equal(
    sort(colnames(log)),
    sort(
      c(
        "worker",
        "online",
        "instances",
        "assigned",
        "complete",
        "socket"
      )
    )
  )
  expect_true(is.integer(client$dispatcher))
  expect_equal(length(client$dispatcher), 1L)
  expect_false(anyNA(client$dispatcher))
  handle <- ps::ps_handle(pid = client$dispatcher)
  crew_retry(
    ~ps::ps_is_running(handle),
    seconds_interval = 0.01,
    seconds_timeout = 30
  )
  socket <- log$socket
  expect_true(is.character(socket) && length(socket) > 0L)
  expect_true(nzchar(socket) && !anyNA(socket))
  expect_equal(length(socket), 1L)
  expect_false(log$online)
  expect_equal(log$assigned, 0L)
  expect_equal(log$complete, 0L)
  expect_equal(log$instances, 0L)
  bin <- if_any(tolower(Sys.info()[["sysname"]]) == "windows", "R.exe", "R")
  path <- file.path(R.home("bin"), bin)
  call <- sprintf("mirai::daemon('%s')", socket)
  px <- processx::process$new(command = path, args = c("-e", call))
  crew_retry(
    ~{
      daemons <- mirai::status(.compute = client$name)$daemons
      identical(
        as.integer(unname(daemons[, "online", drop = TRUE])),
        1L
      )
    },
    seconds_interval = 0.5,
    seconds_timeout = 60
  )
  m <- mirai::mirai(ps::ps_pid(), .compute = client$name)
  crew_retry(
    ~!unresolved(m),
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  expect_false(unresolved(m))
  expect_true(is.numeric(m$data))
  expect_true(abs(m$data - ps::ps_pid()) > 0.5)
  expect_true(client$started)
  expect_true(client$summary()$online)
  expect_silent(client$start())
  for (index in seq_len(2L)) {
    expect_silent(client$terminate())
    expect_false(client$started)
    crew_retry(
      ~!ps::ps_is_running(handle),
      seconds_interval = 0.01,
      seconds_timeout = 30
    )
  }
  px$signal(signal = crew_terminate_signal())
  expect_null(client$summary())
})

crew_test("crew_client() cover a line", {
  client <- crew_client(host = "127.0.0.1")
  private <- crew_private(client)
  private$.started <- TRUE
  expect_null(client$terminate())
})


crew_test("crew_client() deprecate tls_enable", {
  expect_warning(
    crew_client(host = "127.0.0.1", tls_enable = TRUE),
    class = "crew_deprecate"
  )
})

crew_test("crew_client() deprecate tls_config", {
  expect_warning(
    crew_client(host = "127.0.0.1", tls_config = list()),
    class = "crew_deprecate"
  )
})
