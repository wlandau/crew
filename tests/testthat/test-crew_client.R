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
    profile = "abc",
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
  expect_equal(client$profile, "abc")
  expect_null(client$serialization)
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
  expect_null(client$url)
  expect_silent(client$start())
  expect_silent(client$validate())
  expect_true(client$started)
  url <- client$url
  expect_true(is.character(url) && length(url) == 1L)
  expect_true(nzchar(url) && !anyNA(url))
  bin <- if_any(tolower(Sys.info()[["sysname"]]) == "windows", "R.exe", "R")
  path <- file.path(R.home("bin"), bin)
  call <- sprintf("mirai::daemon('%s', dispatcher = TRUE)", url)
  px <- processx::process$new(command = path, args = c("-e", call))
  crew_retry(
    ~ {
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
    ~ !nanonext::.unresolved(task),
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

crew_test("crew_client() deprecate pids()", {
  skip_on_cran()
  expect_warning(pid <- crew_client()$pids(), class = "crew_deprecate")
  skip_on_os("windows")
  expect_equal(pid, Sys.getpid())
})

crew_test("crew_client() custom profile", {
  skip_on_cran()
  x <- crew_client(
    host = "127.0.0.1",
    port = "57000",
    profile = "__abc__"
  )
  expect_equal(x$profile, "__abc__")
  on.exit(x$terminate())
  x$start()
  url <- nanonext::parse_url(mirai::nextget("url", .compute = "__abc__"))
  expect_equal(as.character(url["host"]), "127.0.0.1:57000")
})

crew_test("crew_client() profile conflicts", {
  skip_on_cran()
  skip_if_not(is.null(mirai::nextget("url", .compute = "__abc__")))
  x <- crew_client(
    host = "127.0.0.1",
    port = "57000",
    profile = "__abc__"
  )
  y <- crew_client(
    host = "127.0.0.1",
    port = "57001",
    profile = "__abc__"
  )
  on.exit({
    x$terminate()
    y$terminate()
  })
  x$start()
  expect_crew_error(y$start())
  x$terminate()
  expect_no_error(y$start())
})
