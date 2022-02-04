test_that("crew_catch()", {
  out <- crew_catch(crew_error())
  expect_s3_class(out, "crew_error")
})

test_that("crew_shutdown()", {
  expect_error(crew_shutdown(), class = "crew_shutdown")
})
