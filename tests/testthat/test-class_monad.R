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

crew_test("print monad", {
  out <- utils::capture.output(print(monad_init()))
  expect_true(any(grepl(pattern = "<crew_monad>", x = out, fixed = TRUE)))
})
