crew_test("random_name()", {
  out <- random_name()
  expect_true(is.character(out))
  expect_false(anyNA(out))
  expect_equal(length(out), 1L)
  expect_gt(nzchar(out), 0L)
})

crew_test("is_named", {
  expect_true(is_named(NULL))
  expect_true(is_named(list()))
  expect_true(is_named(list(a = 1)))
  expect_true(is_named(list(a = 1, b = 1)))
  expect_false(is_named(list(a = 1, a = 1)))
  expect_false(is_named(list(a = 1, 2)))
})

crew_test("parse_socket() with index ", {
  socket <- "ws://10.0.0.9:56546/3/a6f9411a4b98c2e851f08387345c722597a9b5b7"
  out <- parse_socket(socket)
  expect_equal(out$index, 3L)
  expect_equal(out$instance, "a6f9411a4b98c2e851f08387345c722597a9b5b7")
})

crew_test("parse_socket() without index ", {
  socket <- "ws://10.0.0.9:56546/cba033e58a8561d7f49ab87d72b7ad86e28384de"
  out <- parse_socket(socket)
  expect_equal(out$index, 1L)
  expect_equal(out$instance, "cba033e58a8561d7f49ab87d72b7ad86e28384de")
})

crew_test("eval_tidyselect()", {
  expr <- rlang::new_quosure(quote(tidyselect::any_of(c("b", "c"))))
  out <- eval_tidyselect(expr = expr, choices = letters[seq_len(4L)])
  expect_equal(out, c("b", "c"))
  out <- eval_tidyselect(expr = expr, choices = character(0))
  expect_equal(out, character(0L))
  out <- eval_tidyselect(expr = rlang::new_quosure(NULL), choices = "x")
  expect_equal(out, character(0L))
})
