test_that("crew_mirai_router() validate", {
  router <- crew_mirai_router()
  expect_silent(router$validate())
  router$name <- NULL
  expect_crew_error(router$validate())
})

test_that("crew_mirai_router() works", {
  skip_on_cran()
  router <- crew_mirai_router()
  expect_false(router$is_connected())
  expect_equal(router$sockets_occupied(), character(0))
  expect_silent(router$connect())
  expect_true(router$is_connected())
  expect_equal(router$sockets_occupied(), character(0))
  socket <- router$sockets
  expect_true(is.character(socket) && length(socket) > 0L)
  expect_true(nzchar(socket) && !anyNA(socket))
  expect_equal(router$sockets_available(), router$sockets)
  px <- callr::r_session$new(wait = TRUE)
  px$call(function(socket) mirai::server(socket), args = list(socket = socket))
  for (index in seq_len(300)) {
    if (identical(router$sockets_occupied(), socket)) {
      break
    }
    Sys.sleep(0.1)
  }
  expect_equal(router$sockets_occupied(), socket)
  expect_equal(router$sockets_available(), character(0))
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
  expect_true(router$is_connected())
  expect_silent(router$disconnect())
  expect_false(router$is_connected())
  px$kill()
})
