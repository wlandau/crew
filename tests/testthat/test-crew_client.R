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
  on.exit(client$terminate())
  expect_equal(client$host, "127.0.0.1")
  expect_equal(client$port, 123L)
  expect_true(inherits(client$tls, "crew_class_tls"))
  expect_equal(client$seconds_interval, 123)
  expect_equal(client$seconds_timeout, 456)
  expect_false(client$started)
  expect_null(client$url)
  expect_null(client$profile)
  expect_null(client$client)
  expect_null(client$dispatcher)
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
  expect_false(client$started)
  expect_null(client$dispatcher)
  expect_null(client$url)
  expect_equal(client$resolved(), 0L)
  expect_silent(client$start())
  expect_silent(client$validate())
  expect_true(client$started)
  url <- client$url
  expect_true(is.character(url) && length(url) == 1L)
  expect_true(nzchar(url) && !anyNA(url))
  expect_s3_class(client$client, "ps_handle")
  expect_s3_class(client$dispatcher, "ps_handle")
  expect_equal(length(client$dispatcher), 1L)
  handle <- client$dispatcher
  crew_retry(
    ~ps::ps_is_running(handle),
    seconds_interval = 0.01,
    seconds_timeout = 30
  )
  bin <- if_any(tolower(Sys.info()[["sysname"]]) == "windows", "R.exe", "R")
  path <- file.path(R.home("bin"), bin)
  call <- sprintf("mirai::daemon('%s', dispatcher = TRUE)", url)
  px <- processx::process$new(command = path, args = c("-e", call))
  crew_retry(
    ~{
      identical(
        as.integer(mirai::status(.compute = client$profile)$connections),
        1L
      )
    },
    seconds_interval = 0.5,
    seconds_timeout = 60
  )
  task <- mirai::mirai(ps::ps_pid(), .compute = client$profile)
  crew_retry(
    ~!nanonext::.unresolved(task),
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  expect_false(nanonext::.unresolved(task))
  expect_true(is.numeric(task$data))
  expect_true(abs(task$data - ps::ps_pid()) > 0.5)
  expect_true(client$started)
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
})

crew_test("crew_client() cover a line", {
  client <- crew_client(host = "127.0.0.1")
  private <- crew_private(client)
  private$.started <- TRUE
  private$.profile <- "abc"
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
