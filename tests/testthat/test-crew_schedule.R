test_that("crew_schedule() validate empty", {
  x <- crew_schedule()
  expect_silent(x$validate())
})

test_that("crew_schedule() validate full", {
  x <- crew_schedule()
  x$start()
  x$head <- "id"
  x$until <- nanonext::mclock()
  expect_silent(x$validate())
})
