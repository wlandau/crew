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

crew_test("as_monad()", {
  skip_on_cran()
  task <- structure(list(data = list(2L)), class = c("mirai", "recvAio"))
  expect_equal(as_monad(task = task, name = "x"), list(2L))
  task <- mirai::mirai(stop("error_message"))
  mirai::call_mirai_(task)
  monad <- as_monad(task = task, name = "x")
  expect_equal(monad$name, "x")
  expect_equal(monad$result, list(NA))
  expect_equal(monad$status, "error")
  expect_true(grepl("error_message", monad$error))
  expect_equal(monad$code, -1L)
  task <- mirai::mirai(Sys.sleep(300))
  mirai::stop_mirai(task)
  monad <- as_monad(task = task, name = "x")
  expect_equal(monad$name, "x")
  expect_equal(monad$result, list(NA))
  expect_equal(monad$status, "cancel")
  expect_true(grepl("cancel", tolower(monad$error)))
  expect_equal(monad$code, 20L)
  on.exit(mirai::daemons(n = 0L))
  mirai::daemons(n = 1L)
  task <- mirai::mirai(Sys.sleep(300))
  Sys.sleep(0.25)
  mirai::daemons(n = 0L)
  monad <- as_monad(task = task, name = "x")
  expect_equal(monad$name, "x")
  expect_equal(monad$result, list(NA))
  expect_equal(monad$status, "crash")
  expect_true(grepl("connection reset", tolower(monad$error)))
  expect_equal(monad$code, 19L)
})
