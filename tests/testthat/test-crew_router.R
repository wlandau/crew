crew_test("crew_router() validate", {
  router <- crew_router()
  expect_silent(router$validate())
  router$name <- NULL
  expect_crew_error(router$validate())
})

crew_test("crew_router() works", {
  skip_if_low_dep_versions()
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router()
  on.exit({
    router$terminate()
    rm(router)
    crew_test_sleep()
  })
  expect_null(router$started)
  expect_null(router$started)
  expect_null(router$dispatcher)
  expect_null(router$daemons)
  expect_silent(router$poll())
  expect_null(router$daemons)
  expect_silent(router$start())
  expect_true(router$started)
  crew_retry(
    ~router$started,
    seconds_interval = 0.5,
    seconds_timeout = 10
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
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew_retry(
    ~!anyNA(m$data),
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  expect_false(anyNA(m$data))
  expect_true(is.numeric(m$data))
  expect_true(abs(m$data - ps::ps_pid()) > 0.5)
  expect_true(router$started)
  expect_true(router$log()$worker_connected)
  expect_silent(router$terminate())
  expect_false(router$started)
  expect_false(router$started)
  px$kill()
  expect_null(router$daemons)
  expect_null(router$log())
  expect_silent(router$poll())
  expect_null(router$log())
})

crew_test("router$route()", {
  skip_if_low_dep_versions()
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router(
    workers = 2L
  )
  router$start()
  on.exit(router$terminate())
  Sys.sleep(0.5)
  router$poll()
  expect_equal(router$rotated, c(FALSE, FALSE))
  expect_equal(router$tallied, c(FALSE, FALSE))
  router$tallied <- c(TRUE, TRUE)
  # First instance of worker 2.
  old <- rownames(router$daemons)
  new <- router$route(index = 2L)
  expect_equal(router$rotated, c(FALSE, TRUE))
  expect_equal(router$tallied, c(TRUE, FALSE))
  Sys.sleep(0.5)
  router$poll()
  expect_equal(router$rotated, c(FALSE, TRUE))
  expect_equal(router$tallied, c(TRUE, FALSE))
  expect_equal(old == rownames(router$daemons), c(TRUE, TRUE))
  expect_equal(new, old[2L])
  expect_equal(new, rownames(router$daemons)[2L])
  # Second instance of worker 2.
  old <- rownames(router$daemons)
  new <- router$route(index = 2L)
  expect_equal(router$rotated, c(FALSE, TRUE))
  expect_equal(router$tallied, c(TRUE, FALSE))
  Sys.sleep(0.5)
  router$poll()
  expect_equal(router$rotated, c(FALSE, TRUE))
  expect_equal(old == rownames(router$daemons), c(TRUE, FALSE))
  expect_false(new == old[2L])
  expect_equal(new, rownames(router$daemons)[2L])
  # First instance of worker 1.
  old <- rownames(router$daemons)
  new <- router$route(index = 1L)
  expect_equal(router$rotated, c(TRUE, TRUE))
  expect_equal(router$tallied, c(FALSE, FALSE))
  Sys.sleep(0.5)
  router$poll()
  expect_equal(router$rotated, c(TRUE, TRUE))
  expect_equal(new, old[1L])
  expect_equal(new, rownames(router$daemons)[1L])
})

crew_test("router$tally()", {
  skip_if_low_dep_versions()
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router(workers = 8L)
  expect_null(router$assigned)
  expect_null(router$complete)
  expect_null(router$tallied)
  router$start()
  expect_equal(router$assigned, rep(0L, router$workers))
  expect_equal(router$complete, rep(0L, router$workers))
  expect_equal(router$tallied, rep(FALSE, router$workers))
  on.exit(router$terminate())
  router$assigned <- seq_len(8L) * 100L
  router$complete <- seq_len(8L) * 1000L
  router$tallied <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  router$daemons[, "online"] <- as.integer(c(0, 0, 1, 1, 0, 0, 1, 1))
  router$daemons[, "instance"] <- as.integer(c(0, 1, 0, 1, 0, 1, 0, 1))
  router$daemons[, "assigned"] <- seq_len(8L)
  router$daemons[, "complete"] <- seq_len(8L) * 10L
  router$tally()
  exp <- seq_len(8L) * 100L
  exp[6L] <- 606L
  expect_equal(router$assigned, exp)
  exp <- seq_len(8L) * 1000L
  exp[6L] <- 6060L
  expect_equal(router$complete, exp)
  exp <- c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
  expect_equal(router$tallied, exp)
  router$route(index = 2)
  exp <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
  expect_equal(router$tallied, exp)
  router$route(index = 8)
  exp <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
  expect_equal(router$tallied, exp)
})
