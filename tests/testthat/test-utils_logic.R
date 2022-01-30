test_that("%||%", {
  expect_equal("a" %||% "b", "a")
  expect_equal(NULL %||% "b", "b")
})

test_that("if_any()", {
  expect_equal(if_any(TRUE, "a", "b"), "a")
  expect_equal(if_any(FALSE, "a", "b"), "b")
})
