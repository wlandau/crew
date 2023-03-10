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
