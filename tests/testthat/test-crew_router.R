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
  router <- crew_router(
    seconds_poll_high = 0.01,
    seconds_poll_low = 0.1
  )
  on.exit(router$terminate())
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
        "worker_instances",
        "tasks_assigned",
        "tasks_complete",
        "router_name"
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
  daemons <- mirai::daemons(.compute = router$name)$daemons
  expect_equal(socket, as.character(rownames(daemons)))
  expect_true(all(daemons == 0L))
  px <- callr::r_bg(
    function(socket) mirai::server(socket),
    args = list(socket = socket)
  )
  crew::crew_wait(
    ~{
      daemons <- mirai::daemons(.compute = router$name)$daemons
      identical(unname(daemons[, "status_online", drop = TRUE]), 1L)
    },
    timeout = 5,
    wait = 0.1
  )
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew::crew_wait(~!anyNA(m$data), timeout = 5, wait = 0.1)
  expect_false(anyNA(m$data))
  expect_true(is.numeric(m$data))
  expect_true(abs(m$data - ps::ps_pid()) > 0.5)
  expect_true(router$listening())
  expect_silent(router$terminate())
  expect_false(router$listening())
  px$kill()
  expect_silent(router$poll())
  true(all(dim(router$daemons) > 0L))
})
