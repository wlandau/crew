crew_test("crew_options_local", {
  out <- crew_options_local(log_directory = "x", log_join = FALSE)
  expect_equal(out$log_directory, "x")
  expect_false(out$log_join)
  expect_silent(crew_options_local_validate(out))
  out$log_directory <- 123
  expect_crew_error(crew_options_local_validate(out))
})
