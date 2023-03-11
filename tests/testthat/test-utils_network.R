crew_test("local_ip()", {
  out <- local_ip()
  expect_true(is.character(out))
  expect_false(anyNA(out))
  expect_equal(length(out), 1L)
  expect_true(all(nzchar(out)))
  split <- as.integer(strsplit(out, split = ".", fixed = TRUE)[[1]])
  expect_equal(length(split), 4L)
  expect_true(all(split >= 0L))
})

crew_test("free_port()", {
  out <- free_port()
  expect_true(is.integer(out))
  expect_false(anyNA(out))
  expect_equal(length(out), 1L)
  expect_true(out >= 49152L)
  expect_true(out <= 65535L)
})

crew_test("connection_bus()", {
  port <- free_port()
  con <- connection_bus(port = port, suffix = "example", wait = TRUE)
  expect_equal(con$state, "opened")
  expect_equal(
    con$listener[[1]]$url,
    sprintf("ws://%s:%s/example", local_ip(), port)
  )
  close(con)
  expect_equal(con$state, "closed")
})
