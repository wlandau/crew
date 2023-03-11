crew_test("crew_router() validate", {
  router <- crew_router()
  expect_silent(router$validate())
  router$name <- NULL
  expect_crew_error(router$validate())
})

crew_test("crew_router() works", {
  router <- crew_router()
  on.exit(router$terminate())
  expect_false(router$listening())
  expect_null(router$sockets)
  expect_silent(router$listen())
  expect_true(router$listening())
  socket <- router$sockets
  expect_true(is.character(socket) && length(socket) > 0L)
  expect_true(nzchar(socket) && !anyNA(socket))
  expect_equal(length(socket), 1L)
  nodes <- mirai::daemons(.compute = router$name)$nodes
  expect_equal(socket, rownames(nodes))
  expect_true(all(nodes == 0L))
  px <- callr::r_bg(
    function(socket) mirai::server(socket),
    args = list(socket = socket)
  )
  crew::crew_wait(
    ~{
      nodes <- mirai::daemons(.compute = router$name)$nodes
      identical(unname(nodes[, "status_online", drop = TRUE]), 1L)
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
  expect_null(router$sockets)
  px$kill()
})
