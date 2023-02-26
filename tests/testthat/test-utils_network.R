crew_test("tcp_socket()", {
  out <- tcp_socket(host = "127.0.0.1", port = 8000)
  expect_equal(out, "tcp://127.0.0.1:8000")
})

crew_test("local_ipv4()", {
  out <- local_ipv4()
  expect_true(is.character(out))
  expect_true(all(nzchar(out)))
  expect_equal(length(out), 1L)
  split <- strsplit(out, split = ".", fixed = TRUE)[[1]]
  expect_equal(paste(split, collapse = "."), out)
  expect_true(all(nzchar(split)))
  expect_equal(length(split), 4L)
})

crew_test("random_port()", {
  port <- random_port(lower = 49152L, upper = 65355L)
  expect_true(is.character(port))
  expect_false(anyNA(port))
  expect_true(nzchar(port) > 0L)
  expect_equal(length(port), 1L)
  port <- as.integer(port)
  expect_true(is.integer(port))
  expect_true(is.finite(port))
  expect_true(port >= 49152L)
  expect_true(port <= 65535L)
})
