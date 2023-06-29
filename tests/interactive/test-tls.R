crew_test("crew_launcher_local() forwards tls_config info to daemons", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    tls_enable = TRUE,
    tls_config = list(
      client = c("invalid", "invalid"),
      server = c("invalid", "invalid")
    )
  )
  expect_error(x$start())
  x$terminate()
})
