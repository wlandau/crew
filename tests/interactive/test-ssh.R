crew_test("crew_client() with crew_options_ssh()", {
  skip_on_cran()
  client <- crew_client(
    options_ssh = crew_options_ssh(host = "9999.9999.9999.9999")
  )
  on.exit(client$terminate())
  # Should print "Could not resolve hostname 9999...".
  client$start()
  expect_true(TRUE)
})
