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

crew_test("monads can be row-bound together", {
  out <- rbind(
    monad_init(name = "a"),
    monad_init(name = "b"),
    monad_init(name = "c", result = emptyenv()),
    monad_init(name = "d", result = emptyenv())
  )
  expect_equal(nrow(out), 4L)
  expect_equal(out$name, letters[seq_len(4L)])
  exp <- list(NA, NA, emptyenv(), emptyenv())
  expect_equal(out$result, exp)
})
