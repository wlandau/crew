crew_test("valid monad from crew_eval()", {
  monad <- crew_eval(quote(1L + 1L))
  expect_silent(monad_validate(monad))
})

crew_test("keep track of seconds", {
  monad <- crew_eval(quote(1L + 1L))
  out <- monad$seconds
  expect_true(is.numeric(out))
  expect_equal(length(out), 1L)
})

crew_test("run without error", {
  monad <- crew_eval(quote(1L + 1L))
  expect_equal(monad$result, list(2L))
  expect_true(anyNA(monad$error))
  expect_true(anyNA(monad$traceback))
})

crew_test("run with error", {
  monad <- crew_eval(quote(stop(12345)))
  expect_equal(monad$result, list(NA))
  expect_true(any(grepl("12345", monad$error)))
  expect_true(any(grepl("12345", monad$traceback)))
})

crew_test("error with no message", {
  monad <- crew_eval(quote(stop()))
  expect_equal(monad$result, list(NA))
  expect_equal(monad$error, ".")
  expect_true(is.character(monad$traceback))
  expect_true(length(monad$traceback) > 0L)
  expect_true(any(nzchar(monad$traceback)))
})

crew_test("run with warning, warning recorded", {
  skip_on_cran()
  monad <- crew_eval(quote(warning("12345")))
  expect_true(any(grepl("12345", monad$warnings)))
})

crew_test("warning with no message, warning recorded", {
  skip_on_cran()
  monad <- crew_eval(quote(warning()))
  expect_equal(monad$warnings, ".")
})

crew_test("warning character limit", {
  skip_on_cran()
  monad <- crew_eval(
    quote(
      lapply(
        seq_len(1e3),
        function(i) warning(paste(rep("a", 65), collapse = ""))
      )
    )
  )
  expect_equal(nchar(monad$warnings), crew_eval_max_nchar)
})

crew_test("encoding issue error handling", {
  skip_on_cran()
  monad <- crew_eval(
    quote(warning("<<\"pT\xbf\xbfD\x80QY\x94C\xd1"))
  )
  expect_true(grepl("text encoding issue", monad$warnings))
})

crew_test("crew_eval() seed", {
  out1 <- crew_eval(quote(sample.int(n = 1e9L, size = 1L)), seed = 1L)
  out2 <- crew_eval(quote(sample.int(n = 1e9L, size = 1L)), seed = 1L)
  out3 <- crew_eval(quote(sample.int(n = 1e9L, size = 1L)), seed = 2L)
  expect_equal(out1$seed, 1L)
  expect_equal(out2$seed, 1L)
  expect_equal(out3$seed, 2L)
  expect_equal(out1$result[[1]], out2$result[[1]])
  expect_true(abs(out1$result[[1]] - out3$result[[1]]) > 1L)
})

crew_test("crew_eval() local data and garbage collection", {
  out <- crew_eval(
    quote(local_object + 1L),
    data = list(local_object = 2L),
    garbage_collection = TRUE
  )
  expect_equal(out$result[[1L]], 3L)
})

crew_test("envir_state() and envir_restore()", {
  names <- c("a", "b", "c", "d")
  envir <- new.env(parent = emptyenv())
  envir$c <- "old_c"
  envir$d <- "old_d"
  envir$e <- "old_e"
  state <- envir_state(names = names, envir = envir)
  expect_equal(sort(c(state$delete)), sort(c("a", "b")))
  expect_equal(state$revert, list(c = "old_c", d = "old_d"))
  new <- list(a = "new_a", b = "new_b", c = "new_c", d = "new_d")
  list2env(new, envir = envir)
  expect_equal(sort(names(envir)), sort(letters[seq_len(5)]))
  expect_equal(envir$a, "new_a")
  expect_equal(envir$b, "new_b")
  expect_equal(envir$c, "new_c")
  expect_equal(envir$d, "new_d")
  expect_equal(envir$e, "old_e")
  envir_restore(state = state, envir = envir)
  expect_equal(names(envir), sort(c("c", "d", "e")))
  expect_equal(envir$c, "old_c")
  expect_equal(envir$d, "old_d")
  expect_equal(envir$e, "old_e")
})

crew_test("crew_eval() globals", {
  skip_on_cran()
  expect_false(exists(x = "crew_global_object", envir = globalenv()))
  out <- crew_eval(
    quote(crew_global_object + 1L),
    globals = list(crew_global_object = 2L)
  )
  expect_equal(out$result[[1L]], 3L)
  expect_false(exists(x = "crew_global_object", envir = globalenv()))
})

crew_test("crew_eval() environment variables", {
  skip_on_cran()
  previous <- Sys.getenv(c("CREW_SOCKET_DATA", "CREW_SOCKET_SESSION"))
  Sys.unsetenv(c("CREW_SOCKET_DATA", "CREW_SOCKET_SESSION"))
  on.exit(do.call(what = Sys.setenv, args = as.list(previous)))
  out <- crew_eval(quote(L))
  expect_equal(out$socket_data, NA_character_)
  expect_equal(out$socket_session, NA_character_)
  Sys.setenv(CREW_SOCKET_DATA = "this1", CREW_SOCKET_SESSION = "this2")
  out <- crew_eval(quote(L))
  expect_equal(out$socket_data, "this1")
  expect_equal(out$socket_session, "this2")
})

crew_test("crew_eval() options", {
  skip_on_cran()
  old_options <- options()
  on.exit(options(old_options)) # Reset to old options on exit.
  expect_null(getOption("crew_option_1"))
  expect_null(getOption("crew_option_2"))
  expect_lt(getOption("warn"), 10)
  out <- crew_eval(
    quote({
      options(crew_option_1 = 1L, crew_option_2 = 2L, warn = 10)
      list(
        crew_option_1 = getOption("crew_option_1"),
        crew_option_2 = getOption("crew_option_2"),
        warn = getOption("warn")
      )
    })
  )
  expect_null(getOption("crew_option_1"))
  expect_null(getOption("crew_option_2"))
  expect_lt(getOption("warn"), 10)
  expect_equal(
    out$result[[1]],
    list(
      crew_option_1 = 1L,
      crew_option_2 = 2L,
      warn = 10L
    )
  )
  options(old_options) # Reset to old options.
})
