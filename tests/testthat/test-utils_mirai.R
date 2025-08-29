crew_test("mirai_status()", {
  skip_on_cran()
  skip_on_os("windows")
  mirai::daemons(url = "tcp://127.0.0.1:0", .compute = "example")
  on.exit(mirai::daemons(n = 0L, .compute = "example"))
  out <- mirai_status(
    profile = "example",
    seconds_interval = 1L,
    seconds_timeout = 15L
  )
  expect_true(is.numeric(out))
})

crew_test("mirai_status_error()", {
  skip_on_cran()
  expect_crew_error(mirai_status_error(profile = "default"))
})
