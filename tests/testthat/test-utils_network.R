crew_test("free_port()", {
  port <- free_port()
  expect_true(is.integer(port))
  expect_true(length(port) == 1L)
  expect_false(anyNA(port))
  expect_true(port >= 49152L)
  expect_true(port <= 65535L)
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

crew_test("web_sockets()", {
  out <- web_sockets(host = "127.0.0.1", port = 8000, n = 2)
  exp <- c("ws://127.0.0.1:8000/worker1", "ws://127.0.0.1:8000/worker2")
  expect_equal(out, exp)
})
