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
  expect_null(client$dispatcher)
  expect_silent(client$start())
  expect_true(client$started)
  log <- client$log()
  expect_equal(
    sort(colnames(log)),
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
  socket <- log$worker_socket
  expect_true(is.character(socket) && length(socket) > 0L)
  expect_true(nzchar(socket) && !anyNA(socket))
  expect_equal(length(socket), 1L)
  expect_false(log$worker_connected)
  expect_equal(log$tasks_assigned, 0)
  expect_equal(log$tasks_complete, 0)
  expect_equal(log$worker_connected, FALSE)
  expect_equal(log$worker_instances, 0)
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
  px$kill()
  expect_null(client$log())
})
