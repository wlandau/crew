test_that("random_port()", {
  port <- random_port(lower = 49152L, upper = 65355L)
  expect_true(length(port) == 1L)
  expect_true(is.integer(port))
  expect_true(is.finite(port))
  expect_true(port >= 49152L)
  expect_true(port <= 65355L)
})
