crew_test("empty monad is valid", {
  expect_silent(monad_validate(monad_init()))
})

crew_test("bad name", {
  out <- monad_init()
  out$name <- 0
  expect_crew_error(monad_validate(out))
})

crew_test("bad field", {
  out <- monad_init()
  out$nope <- 0
  expect_crew_error(monad_validate(out))
})

crew_test("monad_tibble()", {
  out <- monad_tibble(monad_init(name = "x"))
  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 1L)
})
