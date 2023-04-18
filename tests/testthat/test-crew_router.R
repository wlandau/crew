crew_test("crew_router() validate", {
  router <- crew_router()
  expect_silent(router$validate())
  router$name <- NULL
  expect_crew_error(router$validate())
})

crew_test("crew_router() works", {
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router()
  on.exit({
    router$terminate()
    rm(router)
    crew_test_sleep()
  })
  expect_false(router$listening())
  expect_null(router$started)
  expect_null(router$dispatcher)
  expect_null(router$daemons)
  expect_silent(router$poll(error = FALSE))
  expect_error(router$poll(error = TRUE), class = "crew_error")
  expect_null(router$daemons)
  expect_silent(router$start())
  expect_true(router$started)
  crew_retry(
    ~router$listening(),
    seconds_interval = 0.01,
    seconds_timeout = 5
  )
  expect_true(all(dim(router$daemons) > 0L))
  expect_equal(
    sort(colnames(router$log())),
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
  expect_true(is.integer(router$dispatcher))
  expect_equal(length(router$dispatcher), 1L)
  expect_false(anyNA(router$dispatcher))
  daemons <- router$daemons
  socket <- as.character(rownames(daemons))
  expect_true(is.character(socket) && length(socket) > 0L)
  expect_true(nzchar(socket) && !anyNA(socket))
  expect_equal(length(socket), 1L)
  expect_false(router$log()$worker_connected)
  expect_equal(socket, as.character(rownames(daemons)))
  expect_true(all(daemons == 0L))
  bin <- if_any(tolower(Sys.info()[["sysname"]]) == "windows", "R.exe", "R")
  path <- file.path(R.home("bin"), bin)
  call <- sprintf("mirai::server('%s')", socket)
  px <- processx::process$new(command = path, args = c("-e", call))
  crew_retry(
    ~{
      daemons <- mirai::daemons(.compute = router$name)$daemons
      identical(
        as.integer(unname(daemons[, "online", drop = TRUE])),
        1L
      )
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew_retry(
    ~!anyNA(m$data),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_false(anyNA(m$data))
  expect_true(is.numeric(m$data))
  expect_true(abs(m$data - ps::ps_pid()) > 0.5)
  expect_true(router$listening())
  expect_true(router$log()$worker_connected)
  expect_silent(router$terminate())
  expect_false(router$started)
  expect_false(router$listening())
  px$kill()
  expect_null(router$daemons)
  expect_null(router$log())
  expect_silent(router$poll(error = FALSE))
  expect_error(router$poll(error = TRUE), class = "crew_error")
  expect_null(router$log())
})

crew_test("router websocket rotation", {
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router(workers = 2L)
  router$start()
  on.exit(router$terminate())
  router$poll(error = TRUE)
  expect_equal(router$rotations, c(-1L, -1L))
  # First instance of worker 2.
  old <- rownames(router$daemons)
  new <- router$route(index = 2L)
  expect_equal(router$rotations, c(-1L, 0L))
  router$poll(error = TRUE)
  expect_equal(router$rotations, c(-1L, 0L))
  expect_equal(old == rownames(router$daemons), c(TRUE, TRUE))
  expect_equal(new, old[2L])
  expect_equal(new, rownames(router$daemons)[2L])
  # Second instance of worker 2.
  old <- rownames(router$daemons)
  new <- router$route(index = 2L)
  expect_equal(router$rotations, c(-1L, 1L))
  router$poll(error = TRUE)
  expect_equal(router$rotations, c(-1L, 1L))
  expect_equal(old == rownames(router$daemons), c(TRUE, FALSE))
  expect_false(new == old[2L])
  expect_equal(new, rownames(router$daemons)[2L])
  # First instance of worker 1.
  old <- rownames(router$daemons)
  new <- router$route(index = 1L)
  expect_equal(router$rotations, c(0L, 1L))
  router$poll(error = TRUE)
  expect_equal(router$rotations, c(0L, 1L))
  expect_equal(new, old[1L])
  expect_equal(new, rownames(router$daemons)[1L])
})
