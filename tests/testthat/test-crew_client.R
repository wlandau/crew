crew_test("crew_client() validate", {
  client <- crew_client(host = "127.0.0.1")
  expect_silent(client$validate())
  client$name <- NULL
  expect_crew_error(client$validate())
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
  expect_silent(client$terminate())
  expect_silent(client$terminate())
  client$started <- TRUE
  client$dispatcher <- NULL
  expect_silent(client$terminate())
  expect_false(client$started)
  px$kill()
  expect_null(client$summary())
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
