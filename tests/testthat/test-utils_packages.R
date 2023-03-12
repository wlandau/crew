crew_test("load packages", {
  expect_silent(load_packages(packages = "base", library = NULL))
  expect_crew_error(
    suppressWarnings(
      load_packages(packages = "does;not;exist", library = NULL)
    )
  )
})
