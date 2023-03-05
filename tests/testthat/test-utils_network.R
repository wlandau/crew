crew_test("tcp_socket()", {
  out <- tcp_sockets(host = "127.0.0.1", ports = c(8000, 9000))
  expect_equal(out, c("tcp://127.0.0.1:8000", "tcp://127.0.0.1:9000"))
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

crew_test("free_port()", {
  port <- free_port()
  expect_true(is.integer(port))
  expect_true(length(port) == 1L)
  expect_false(anyNA(port))
  expect_true(port >= 49152L)
  expect_true(port <= 65535L)
})
