test_that("crew_mirai_router() validate", {
  router <- crew_mirai_router()
  expect_silent(router$validate())
  router$name <- NULL
  expect_crew_error(router$validate())
})

test_that("crew_mirai_router() works", {
  skip_on_cran()
  router <- crew_mirai_router()
  expect_false(router$connected())
  expect_equal(router$connections(), character(0))
  expect_silent(router$connect())
  expect_true(router$connected())
  expect_equal(router$connections(), character(0))
  socket <- router$sockets()
  exp <- tcp_sockets(host = router$host, ports = router$ports)
  expect_equal(socket, exp)
  expect_equal(router$available_sockets(), socket)
  px <- callr::r_session$new(wait = TRUE)
  px$call(function(socket) mirai::server(socket), args = list(socket = socket))
  for (index in seq_len(300)) {
    if (identical(router$connections(), socket)) {
      break
    }
    Sys.sleep(0.1)
  }
  expect_equal(router$connections(), socket)
  expect_equal(router$available_sockets(), character(0))
  m <- mirai::mirai(ps::ps_pid())
  for (index in seq_len(300)) {
    if (!anyNA(m$data)) {
      break
    }
    Sys.sleep(0.1)
  }
  expect_false(anyNA(m$data))
  expect_true(is.numeric(m$data))
  expect_true(abs(m$data - ps::ps_pid()) > 0.5)
  expect_true(router$connected())
  expect_silent(router$disconnect())
  expect_false(router$connected())
  px$kill()
})
