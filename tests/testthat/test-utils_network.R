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

crew_test("random_ports()", {
  skip_on_cran()
  port <- random_ports(n = 1L, lower = 49152L, upper = 65355L)
  expect_true(is.integer(port))
  expect_false(anyNA(port))
  expect_true(nzchar(as.character(port)) > 0L)
  expect_equal(length(port), 1L)
  expect_true(is.integer(port))
  expect_true(is.finite(port))
  expect_true(port >= 49152L)
  expect_true(port <= 65535L)
})

crew_test("random_ports() with n > 1", {
  skip_on_cran()
  ports <- random_ports(n = 3L, lower = 49152L, upper = 65355L)
  expect_equal(length(ports), 3L)
  expect_equal(length(unique(ports)), 3L)
  expect_true(is.integer(ports))
  expect_equal(ports, sort(ports))
  expect_true(all(ports >= 49152L))
  expect_true(all(ports <= 65355L))
})
