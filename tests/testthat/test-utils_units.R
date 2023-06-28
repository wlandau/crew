crew_test("units_time()", {
  expect_match(units_time(NA_real_), "")
  expect_match(units_time(12), "seconds")
  expect_match(units_time(120), "minutes")
  expect_match(units_time(3720), "hours")
  expect_match(units_time(3600 * 24 + 1), "days")
  expect_match(units_time(3600 * 24 * 30 + 1), "months")
  expect_match(units_time(3600 * 24 * 365 + 1), "years")
})
