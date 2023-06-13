crew_test("crew_client() validate", {
  client <- crew_client()
  expect_silent(client$validate())
  client$name <- NULL
  expect_crew_error(client$validate())
})

crew_test("crew_client() works", {
  skip_if_low_dep_versions()
  skip_on_cran()
  skip_on_os("windows")
  client <- crew_client()
  on.exit({
    client$terminate()
    rm(client)
    crew_test_sleep()
  })
  expect_null(client$started)
  expect_null(client$started)
  expect_null(client$dispatcher)
  expect_null(client$daemons)
  expect_silent(client$poll())
  expect_null(client$daemons)
  expect_silent(client$start())
  expect_true(client$started)
  crew_retry(
    ~client$started,
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  expect_true(all(dim(client$daemons) > 0L))
  expect_equal(
    sort(colnames(client$log())),
    sort(
      c(
        "tasks_assigned",
        "tasks_complete",
        "worker_connected",
        "worker_instances",
        "worker_socket"
      )
    )
  )
  expect_true(is.integer(client$dispatcher))
  expect_equal(length(client$dispatcher), 1L)
  expect_false(anyNA(client$dispatcher))
  daemons <- client$daemons
  socket <- as.character(rownames(daemons))
  expect_true(is.character(socket) && length(socket) > 0L)
  expect_true(nzchar(socket) && !anyNA(socket))
  expect_equal(length(socket), 1L)
  expect_false(client$log()$worker_connected)
  expect_equal(socket, as.character(rownames(daemons)))
  expect_true(all(daemons == 0L))
  bin <- if_any(tolower(Sys.info()[["sysname"]]) == "windows", "R.exe", "R")
  path <- file.path(R.home("bin"), bin)
  call <- sprintf("mirai::server('%s')", socket)
  px <- processx::process$new(command = path, args = c("-e", call))
  crew_retry(
    ~{
      daemons <- mirai::daemons(.compute = client$name)$daemons
      identical(
        as.integer(unname(daemons[, "online", drop = TRUE])),
        1L
      )
    },
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  m <- mirai::mirai(ps::ps_pid(), .compute = client$name)
  crew_retry(
    ~!anyNA(m$data),
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  expect_false(anyNA(m$data))
  expect_true(is.numeric(m$data))
  expect_true(abs(m$data - ps::ps_pid()) > 0.5)
  expect_true(client$started)
  expect_true(client$log()$worker_connected)
  expect_silent(client$terminate())
  expect_false(client$started)
  expect_false(client$started)
  px$kill()
  expect_null(client$daemons)
  expect_null(client$log())
  expect_silent(client$poll())
  expect_null(client$log())
})

crew_test("client$route()", {
  skip_if_low_dep_versions()
  skip_on_cran()
  skip_on_os("windows")
  client <- crew_client(workers = 2L)
  client$start()
  on.exit(client$terminate())
  client$poll()
  expect_equal(client$tallied, c(FALSE, FALSE))
  client$tallied <- c(TRUE, TRUE)
  # First instance of worker 2.
  client$route(index = 2L)
  expect_equal(client$tallied, c(TRUE, FALSE))
  # Second instance of worker 2.
  client$route(index = 2L)
  expect_equal(client$tallied, c(TRUE, FALSE))
  # First instance of worker 1.
  client$route(index = 1L)
  expect_equal(client$tallied, c(FALSE, FALSE))
})

crew_test("client$tally()", {
  skip_if_low_dep_versions()
  skip_on_cran()
  skip_on_os("windows")
  client <- crew_client(workers = 8L)
  expect_null(client$assigned)
  expect_null(client$complete)
  expect_null(client$tallied)
  client$start()
  expect_equal(client$assigned, rep(0L, client$workers))
  expect_equal(client$complete, rep(0L, client$workers))
  expect_equal(client$tallied, rep(FALSE, client$workers))
  on.exit(client$terminate())
  client$assigned <- seq_len(8L) * 100L
  client$complete <- seq_len(8L) * 1000L
  client$tallied <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  client$daemons[, "online"] <- as.integer(c(0, 0, 1, 1, 0, 0, 1, 1))
  client$daemons[, "instance"] <- as.integer(c(0, 1, 0, 1, 0, 1, 0, 1))
  client$daemons[, "assigned"] <- seq_len(8L)
  client$daemons[, "complete"] <- seq_len(8L) * 10L
  client$tally()
  exp <- seq_len(8L) * 100L
  exp[6L] <- 606L
  expect_equal(client$assigned, exp)
  exp <- seq_len(8L) * 1000L
  exp[6L] <- 6060L
  expect_equal(client$complete, exp)
  exp <- c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
  expect_equal(client$tallied, exp)
  client$route(index = 2)
  exp <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
  expect_equal(client$tallied, exp)
  client$route(index = 8)
  exp <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
  expect_equal(client$tallied, exp)
})
