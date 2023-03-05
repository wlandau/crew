crew_test("crew_router() validate", {
  router <- crew_router()
  expect_silent(router$validate())
  router$name <- NULL
  expect_crew_error(router$validate())
})

crew_test("crew_router() works", {
  router <- crew_router()
  on.exit(router$disconnect())
  expect_false(router$is_connected())
  expect_equal(router$sockets_listening(), character(0))
  expect_equal(router$sockets_occupied(), character(0))
  expect_equal(router$sockets_available(), character(0))
  expect_silent(router$connect())
  expect_true(router$is_connected())
  socket <- router$sockets_listening()
  expect_equal(router$sockets_occupied(), character(0))
  expect_true(is.character(socket) && length(socket) > 0L)
  expect_true(nzchar(socket) && !anyNA(socket))
  expect_equal(router$sockets_available(), socket)
  px <- callr::r_bg(
    function(socket) mirai::server(socket),
    args = list(socket = socket)
  )
  crew::crew_wait(
    ~identical(router$sockets_occupied(), socket),
    timeout = 5,
    wait = 0.1
  )
  expect_equal(router$sockets_occupied(), socket)
  expect_equal(router$sockets_available(), character(0))
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew::crew_wait(~!anyNA(m$data), timeout = 5, wait = 0.1)
  expect_false(anyNA(m$data))
  expect_true(is.numeric(m$data))
  expect_true(abs(m$data - ps::ps_pid()) > 0.5)
  expect_true(router$is_connected())
  expect_silent(router$disconnect())
  expect_false(router$is_connected())
  px$kill()
})
