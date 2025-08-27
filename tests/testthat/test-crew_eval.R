crew_test("valid monad from crew_eval()", {
  monad <- crew_eval(quote(1L + 1L), name = "x")
  expect_silent(monad_validate(monad))
})

crew_test("keep track of seconds", {
  monad <- crew_eval(quote(1L + 1L), name = "x")
  out <- monad$seconds
  expect_true(is.numeric(out))
  expect_equal(length(out), 1L)
})

crew_test("run without error", {
  monad <- crew_eval(quote(1L + 1L), name = "x")
  expect_equal(monad$result, list(2L))
  expect_true(anyNA(monad$error))
  expect_true(anyNA(monad$trace))
})

crew_test("run with error", {
  monad <- crew_eval(quote(stop(12345)), name = "x")
  expect_equal(monad$result, list(NA))
  expect_true(any(grepl("12345", monad$error)))
  expect_true(any(grepl("12345", monad$trace)))
})

crew_test("error with no message", {
  monad <- crew_eval(quote(stop()), name = "x")
  expect_equal(monad$result, list(NA))
  expect_equal(monad$error, ".")
  expect_true(is.character(monad$trace))
  expect_true(length(monad$trace) > 0L)
  expect_true(any(nzchar(monad$trace)))
})

crew_test("run with warning, warning recorded", {
  skip_on_cran()
  monad <- crew_eval(quote(warning("12345")), name = "x")
  expect_true(any(grepl("12345", monad$warnings)))
})

crew_test("warning with no message, warning recorded", {
  skip_on_cran()
  monad <- crew_eval(quote(warning()), name = "x")
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
    ),
    name = "x"
  )
  expect_equal(nchar(monad$warnings), crew_eval_max_nchar)
})

crew_test("encoding issue error handling", {
  skip_on_cran()
  monad <- crew_eval(
    quote(warning("<<\"pT\xbf\xbfD\x80QY\x94C\xd1")),
    name = "x"
  )
  expect_true(grepl("text encoding issue", monad$warnings))
})

crew_test("crew_eval() seed and algorithm", {
  skip_on_cran()
  old_algorithm <- RNGkind()[1L]
  old_seed <- .subset2(.GlobalEnv, ".Random.seed")
  out1 <- crew_eval(
    quote(sample.int(n = 1e9L, size = 1L)),
    seed = 1L,
    algorithm = "Super-Duper",
    name = "x"
  )
  out2 <- crew_eval(
    quote(sample.int(n = 1e9L, size = 1L)),
    seed = 1L,
    algorithm = "Super-Duper",
    name = "x"
  )
  out3 <- crew_eval(
    quote(sample.int(n = 1e9L, size = 1L)),
    seed = 2L,
    algorithm = "Super-Duper",
    name = "x"
  )
  expect_equal(out1$seed, 1L)
  expect_equal(out2$seed, 1L)
  expect_equal(out3$seed, 2L)
  expect_equal(out1$result[[1]], out2$result[[1]])
  expect_true(abs(out1$result[[1]] - out3$result[[1]]) > 1L)
  expect_equal(RNGkind()[1L], old_algorithm)
  expect_equal(.subset2(.GlobalEnv, ".Random.seed"), old_seed)
})

crew_test("crew_eval() just seed", {
  skip_on_cran()
  old_algorithm <- RNGkind()[1L]
  old_seed <- .subset2(.GlobalEnv, ".Random.seed")
  out1 <- crew_eval(
    quote(sample.int(n = 1e9L, size = 1L)),
    seed = 1L,
    algorithm = NULL,
    name = "x"
  )
  out2 <- crew_eval(
    quote(sample.int(n = 1e9L, size = 1L)),
    seed = 1L,
    algorithm = NULL,
    name = "x"
  )
  out3 <- crew_eval(
    quote(sample.int(n = 1e9L, size = 1L)),
    seed = 2L,
    algorithm = NULL,
    name = "x"
  )
  expect_equal(out1$seed, 1L)
  expect_equal(out2$seed, 1L)
  expect_equal(out3$seed, 2L)
  expect_equal(out1$result[[1]], out2$result[[1]])
  expect_true(abs(out1$result[[1]] - out3$result[[1]]) > 1L)
  expect_equal(RNGkind()[1L], old_algorithm)
  expect_equal(.subset2(.GlobalEnv, ".Random.seed"), old_seed)
})

crew_test("crew_eval() just algorithm", {
  skip_on_cran()
  old_algorithm <- RNGkind()[1L]
  old_seed <- .subset2(.GlobalEnv, ".Random.seed")
  out1 <- crew_eval(
    quote(sample.int(n = 1e9L, size = 1L)),
    seed = NULL,
    algorithm = "Super-Duper",
    name = "x"
  )
  out2 <- crew_eval(
    quote(sample.int(n = 1e9L, size = 1L)),
    seed = NULL,
    algorithm = "Super-Duper",
    name = "x"
  )
  out3 <- crew_eval(
    quote(sample.int(n = 1e9L, size = 1L)),
    seed = NULL,
    algorithm = "Super-Duper",
    name = "x"
  )
  expect_equal(RNGkind()[1L], old_algorithm)
  expect_equal(.subset2(.GlobalEnv, ".Random.seed"), old_seed)
})

crew_test("crew_eval() RNG state not restored if not set", {
  skip_on_cran()
  old_algorithm <- RNGkind()[1L]
  old_seed <- .subset2(.GlobalEnv, ".Random.seed")
  out1 <- crew_eval(
    quote(sample.int(n = 1e9L, size = 1L)),
    seed = NULL,
    algorithm = NULL,
    name = "x"
  )
  out2 <- crew_eval(
    quote(sample.int(n = 1e9L, size = 1L)),
    seed = NULL,
    algorithm = NULL,
    name = "x"
  )
  out3 <- crew_eval(
    quote(sample.int(n = 1e9L, size = 1L)),
    seed = NULL,
    algorithm = NULL,
    name = "x"
  )
  expect_false(all(.subset2(.GlobalEnv, ".Random.seed") == old_seed))
})

crew_test("crew_eval() environment variables", {
  skip_on_cran()
  envvars <- "CREW_CONTROLLER"
  previous <- Sys.getenv(envvars)
  names(previous) <- envvars
  Sys.unsetenv(envvars)
  on.exit(do.call(what = Sys.setenv, args = as.list(previous)))
  out <- crew_eval(quote(L), name = "x")
  expect_equal(out$controller, NA_character_)
  Sys.setenv(CREW_CONTROLLER = "x1")
  out <- crew_eval(quote(L), name = "x")
  expect_equal(out$controller, "x1")
})

crew_test("crew_eval() reset_globals", {
  skip_on_cran()
  out <- crew_eval(
    quote({
      assign(x = "crew_globalenv_test", value = "result", envir = .GlobalEnv)
      get(x = "crew_globalenv_test", envir = .GlobalEnv)
    }),
    name = "test",
    reset_globals = TRUE
  )
  expect_equal(out$result[[1L]], "result")
  expect_false(exists(x = "crew_globalenv_test", envir = .GlobalEnv))
})

crew_test("crew_eval() reset_packages", {
  skip_on_cran()
  skip_if_not_installed("autometric")
  try(
    detach(name = "package:autometric", character.only = TRUE),
    silent = TRUE
  )
  expect_false("package:autometric" %in% search())
  out <- crew_eval(
    quote({
      library(autometric)
      "package:autometric" %in% search()
    }),
    name = "test",
    reset_packages = TRUE
  )
  expect_true(out$result[[1L]])
  expect_false("package:autometric" %in% search())
})

crew_test("crew_eval() reset_options", {
  skip_on_cran()
  old_options <- options()
  on.exit(options(old_options))
  out <- crew_eval(
    quote({
      options(warn = 10L)
      getOption("warn")
    }),
    name = "test",
    reset_options = TRUE
  )
  expect_equal(out$result[[1L]], 10L)
  expect_equal(getOption("warn"), old_options$warn)
})

crew_test("crew_eval() garbage_collection", {
  skip_on_cran()
  crew_eval(quote(NULL), name = "test", garbage_collection = TRUE)
  expect_true(TRUE)
})
