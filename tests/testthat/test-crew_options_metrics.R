crew_test("crew_options_metrics()", {
  out <- crew_options_metrics(path = "x", seconds_interval = 10)
  expect_silent(crew_options_metrics_validate(out))
})
