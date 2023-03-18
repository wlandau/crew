crew_test("crew_router() validate", {
  router <- crew_router(
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  expect_silent(router$validate())
  router$name <- NULL
  expect_crew_error(router$validate())
})

crew_test("crew_router() works", {
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router(
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  on.exit({
    router$terminate()
    crew_test_sleep()
  })
  expect_false(router$listening())
  expect_null(router$dispatcher)
  expect_null(router$daemons)
  expect_silent(router$listen())
  expect_true(router$listening())
  true(all(dim(router$daemons) > 0L))
  expect_equal(
    sort(colnames(router$daemons)),
    sort(
      c(
        "worker_socket",
        "worker_connected",
        "worker_busy",
        "worker_instances",
        "tasks_assigned",
        "tasks_complete"
      )
    )
  )
  expect_true(is.integer(router$dispatcher))
  expect_equal(length(router$dispatcher), 1L)
  expect_false(anyNA(router$dispatcher))
  socket <- router$daemons$worker_socket
  expect_true(is.character(socket) && length(socket) > 0L)
  expect_true(nzchar(socket) && !anyNA(socket))
  expect_equal(length(socket), 1L)
  expect_false(router$daemons$worker_connected)
  expect_false(router$daemons$worker_busy)
  daemons <- mirai::daemons(.compute = router$name)$daemons
  expect_equal(socket, as.character(rownames(daemons)))
  expect_true(all(daemons == 0L))
  px <- callr::r_bg(
    function(socket) mirai::server(socket),
    args = list(socket = socket)
  )
  crew_wait(
    ~{
      daemons <- mirai::daemons(.compute = router$name)$daemons
      identical(unname(daemons[, "status_online", drop = TRUE]), 1L)
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew_wait(
    ~!anyNA(m$data),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_false(anyNA(m$data))
  expect_true(is.numeric(m$data))
  expect_true(abs(m$data - ps::ps_pid()) > 0.5)
  expect_true(router$listening())
  router$poll()
  expect_true(router$daemons$worker_connected)
  expect_false(router$daemons$worker_busy)
  expect_silent(router$terminate())
  expect_false(router$listening())
  px$kill()
  expect_silent(router$poll())
  true(all(dim(router$daemons) > 0L))
  expect_true(is.na(router$daemons$worker_connected))
  expect_true(is.na(router$daemons$worker_busy))
})
