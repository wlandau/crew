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

crew_test("crew_stage() pop()", {
  for (unpopped in c(0L, 7L)) {
    x <- crew_stage()
    x$start()
    cv <- nanonext::cv()
    x$inherit(cv)
    expect_equal(nanonext::cv_value(x$condition), 0L)
    replicate(3L, nanonext::cv_signal(cv))
    x$unpopped <- unpopped
    for (index in seq_len(3L) - 1L) {
      expect_equal(nanonext::cv_value(x$condition), 3L - index)
      expect_equal(x$unpopped, unpopped)
      expect_equal(x$popped, 0L + index)
      x$pop()
    }
    if (unpopped < 1L) {
      expect_crew_error(x$pop())
    } else {
      expect_silent(x$pop())
      expect_equal(nanonext::cv_value(x$condition), 0L)
      expect_equal(x$unpopped, unpopped - 1L)
      expect_equal(x$popped, 4L)
    }
  }
})
