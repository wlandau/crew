crew_test("load packages", {
  skip_on_cran()
  expect_silent(load_packages(packages = "base", library = NULL))
  expect_crew_error(
    suppressWarnings(
      load_packages(packages = "does;not;exist", library = NULL)
    )
  )
})
