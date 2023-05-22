crew_test("load packages", {
  skip_on_cran()
  expect_silent(load_packages(packages = "base", library = NULL))
  expect_crew_error(
    suppressWarnings(
      load_packages(packages = "does;not;exist", library = NULL)
    )
  )
})

crew_test("package_version_check", {
  expect_silent(
    package_version_check(
      package = "crew",
      version = "0.0.0",
      repo = "https://wlandau.r-universe.dev"
    )
  )
  expect_message(
    package_version_check(
      package = "none",
      version = "9999.9999.9999",
      repo = "https://wlandau.r-universe.dev"
    )
  )
})
