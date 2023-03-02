crew_test("valid monad from crew_eval()", {
  monad <- crew_eval(quote(1L + 1L), baseenv())
  expect_silent(monad_validate(monad))
})

crew_test("keep track of seconds", {
  monad <- crew_eval(quote(1L + 1L), baseenv())
  out <- monad$seconds
  expect_true(is.numeric(out))
  expect_equal(length(out), 1L)
})

crew_test("run without error", {
  monad <- crew_eval(quote(1L + 1L), baseenv())
  expect_equal(monad$result, list(2L))
  expect_true(anyNA(monad$error))
  expect_true(anyNA(monad$traceback))
})

crew_test("run with error", {
  monad <- crew_eval(quote(stop(12345)), baseenv())
  expect_equal(monad$result, list(NA))
  expect_true(any(grepl("12345", monad$error)))
  expect_true(any(grepl("12345", monad$traceback)))
})

crew_test("error with no message", {
  monad <- crew_eval(quote(stop()), baseenv())
  expect_equal(monad$result, list(NA))
  expect_equal(monad$error, ".")
  expect_true(is.character(monad$traceback))
  expect_true(length(monad$traceback) > 0L)
  expect_true(any(nzchar(monad$traceback)))
})

crew_test("run with warning, warning recorded", {
  skip_on_cran()
  monad <- crew_eval(quote(warning("12345")), baseenv())
  expect_true(any(grepl("12345", monad$warnings)))
})

crew_test("warning with no message, warning recorded", {
  skip_on_cran()
  monad <- crew_eval(quote(warning()), baseenv())
  expect_equal(monad$warnings, ".")
})

crew_test("warning character limit", {
  monad <- crew_eval(
    quote(
      lapply(
        seq_len(1e3),
        function(i) warning(paste(rep("a", 65), collapse = ""))
      )
    ),
    envir = baseenv()
  )
  expect_equal(nchar(monad$warnings), crew_eval_max_nchar)
})

crew_test("encoding issue error handling", {
  monad <- crew_eval(
    quote(warning("<<\"pT\xbf\xbfD\x80QY\x94C\xd1")),
    baseenv()
  )
  expect_true(grepl("text encoding issue", monad$warnings))
})
