crew_test("crew_stage() validate", {
  x <- crew_stage()
  expect_null(x$condition)
  expect_null(x$unpopped)
  expect_null(x$popped)
  expect_silent(x$validate())
  x$start()
  expect_true(inherits(x$condition, "conditionVariable"))
  expect_equal(x$unpopped, 0L)
  expect_equal(x$popped, 0L)
})

crew_test("crew_stage() validate", {
  x <- crew_stage()
  expect_null(x$condition)
  expect_null(x$unpopped)
  expect_null(x$popped)
  expect_silent(x$validate())
  x$start()
  expect_true(inherits(x$condition, "conditionVariable"))
  expect_equal(x$unpopped, 0L)
  expect_equal(x$popped, 0L)
})

crew_test("crew_stage() inherit", {
  x <- crew_stage()
  x$start()
  cv <- nanonext::cv()
  x$inherit(cv)
  nanonext::cv_signal(cv)
  expect_equal(nanonext::cv_value(x$condition), 1L)
})

crew_test("crew_stage() resolved()", {
  x <- crew_stage()
  x$start()
  expect_equal(x$resolved(), 0L)
  cv <- nanonext::cv()
  x$inherit(cv)
  nanonext::cv_signal(cv)
  expect_equal(x$resolved(), 1L)
  x$unpopped <- 40L
  x$popped <- 700L
  expect_equal(x$resolved(), 741L)
})

