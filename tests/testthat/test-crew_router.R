crew_test("crew_router() validate", {
  router <- crew_router()
  expect_silent(router$validate())
  router$name <- NULL
  expect_crew_error(router$validate())
})

crew_test("crew_router() works", {
  router <- crew_router()
  on.exit(router$disconnect())
  expect_false(router$connected())
  expect_equal(router$sockets(), character(0))
  expect_equal(router$occupied(), character(0))
  expect_equal(router$unoccupied(), character(0))
  expect_silent(router$connect())
  expect_true(router$connected())
  socket <- router$sockets()
  expect_equal(router$occupied(), character(0))
  expect_true(is.character(socket) && length(socket) > 0L)
  expect_true(nzchar(socket) && !anyNA(socket))
  expect_equal(router$unoccupied(), socket)
  px <- callr::r_bg(
    function(socket) mirai::server(socket),
    args = list(socket = socket)
  )
  crew::crew_wait(
    ~identical(router$occupied(), socket),
    timeout = 5,
    wait = 0.1
  )
  expect_equal(router$occupied(), socket)
  expect_equal(router$unoccupied(), character(0))
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew::crew_wait(~!anyNA(m$data), timeout = 5, wait = 0.1)
  expect_false(anyNA(m$data))
  expect_true(is.numeric(m$data))
  expect_true(abs(m$data - ps::ps_pid()) > 0.5)
  expect_true(router$connected())
  expect_silent(router$disconnect())
  expect_false(router$connected())
  px$kill()
})
