crew_test("mirai_error()", {
  skip_on_cran()
  skip_on_os("windows")
  success <- mirai::mirai(TRUE)
  error <- mirai::mirai(stop("abc"))
  mirai::call_mirai_(success)
  mirai::call_mirai_(error)
  expect_null(mirai_error(list()))
  expect_null(mirai_error(success))
  expect_true(is.character(mirai_error(error)))
})

crew_test("mirai_status_error()", {
  skip_on_cran()
  expect_crew_error(mirai_status_error(status = 5L, profile = "default"))
})
