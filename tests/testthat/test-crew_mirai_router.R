# test_that("crew_mirai_router() validate", {
#   router <- crew_mirai_router()
#   expect_silent(router$validate())
#   router$name <- NULL
#   expect_crew_error(router$validate())
# })

test_that("crew_mirai_router() works", {
  skip_on_cran()
   router <- crew_mirai_router()
   expect_false(router$connected())
   expect_equal(router$connections(), 0L)
   expect_silent(router$connect())
   expect_true(router$connected())
   expect_equal(router$connections(), 0L)
   socket <- router$socket()
   exp <- tcp_socket(host = router$host, port = router$port)
   expect_equal(socket, exp)
   px <- callr::r_session$new(wait = TRUE)
   px$call(function(socket) mirai::server(socket), args = list(socket = socket))
   for (index in seq_len(300)) {
     if (identical(router$connections(), 1L)) {
       break
     }
     Sys.sleep(0.1)
   }
   expect_equal(router$connections(), 1L)
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
