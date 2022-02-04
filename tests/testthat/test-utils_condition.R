test_that("crew_catch()", {
  out <- crew_catch(crew_error())
  expect_s3_class(out, "crew_error")
})

test_that("crew_terminate()", {
  expect_error(crew_terminate(), class = "crew_terminate")
})
