crew_test("crew_options_metrics()", {
  out <- crew_options_metrics(
    path_local = "x",
    path_workers = "y",
    seconds_local = 10,
    seconds_workers = 11
  )
  expect_silent(crew_options_metrics_validate(out))
})
