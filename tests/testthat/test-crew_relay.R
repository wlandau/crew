crew_test("relay validate", {
  x <- crew_relay()
  expect_silent(x$validate())
  expect_null(x$condition)
  expect_null(x$unpopped)
  expect_null(x$popped)
  x$start()
  expect_silent(x$validate())
  expect_true(inherits(x$condition, "conditionVariable"))
  expect_equal(x$unpopped, 0L)
  expect_equal(x$popped, 0L)
})

crew_test("relay validate", {
  x <- crew_relay()
  expect_null(x$condition)
  expect_null(x$unpopped)
  expect_null(x$popped)
  expect_silent(x$validate())
  x$start()
  expect_true(inherits(x$condition, "conditionVariable"))
  expect_equal(x$unpopped, 0L)
  expect_equal(x$popped, 0L)
  expect_silent(x$validate())
})

crew_test("relay terminate", {
  x <- crew_relay()
  expect_null(x$condition)
  expect_null(x$unpopped)
  expect_null(x$popped)
  x$start()
  expect_true(inherits(x$condition, "conditionVariable"))
  expect_equal(x$unpopped, 0L)
  expect_equal(x$popped, 0L)
  x$terminate()
  expect_null(x$condition)
  expect_null(x$unpopped)
  expect_null(x$popped)
})

crew_test("relaying signals", {
  x <- crew_relay()
  x$from <- nanonext::cv()
  x$to <- nanonext::cv()
  x$start()
  expect_silent(x$validate())
  nanonext::cv_signal(x$from)
  nanonext::msleep(250)
  expect_equal(nanonext::cv_value(x$from), 1L)
  expect_equal(nanonext::cv_value(x$condition), 1L)
  expect_equal(nanonext::cv_value(x$to), 1L)
})

crew_test("relay resolved()", {
  x <- crew_relay()
  cv <- nanonext::cv()
  x$from <- cv
  x$start()
  expect_equal(x$resolved(), 0L)
  nanonext::cv_signal(cv)
  nanonext::msleep(250)
  expect_equal(x$resolved(), 1L)
  x$unpopped <- 40L
  x$popped <- 700L
  expect_equal(x$resolved(), 741L)
})

crew_test("relay pop()", {
  for (unpopped in c(0L, 7L)) {
    x <- crew_relay()
    cv <- nanonext::cv()
    x$from <- cv
    x$start()
    expect_equal(nanonext::cv_value(x$condition), 0L)
    replicate(3L, nanonext::cv_signal(cv))
    nanonext::msleep(250)
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

crew_test("relay wait_condition()", {
  x <- crew_relay()
  cv <- nanonext::cv()
  x$from <- cv
  x$start()
  x$wait_condition(seconds_timeout = 0.001)
  expect_equal(nanonext::cv_value(x$condition), 0L)
  expect_equal(x$unpopped, 0L)
  expect_equal(x$popped, 0L)
  nanonext::cv_signal(x$condition)
  nanonext::msleep(250)
  expect_equal(nanonext::cv_value(x$condition), 1L)
  x$wait_condition(seconds_timeout = 0.001)
  expect_equal(nanonext::cv_value(x$condition), 0L)
  expect_equal(x$unpopped, 1L)
  expect_equal(x$popped, 0L)
})

crew_test("relay wait_unpopped()", {
  x <- crew_relay()
  cv <- nanonext::cv()
  x$from <- cv
  x$start()
  nanonext::cv_signal(x$condition)
  nanonext::msleep(250)
  expect_equal(nanonext::cv_value(x$condition), 1L)
  x$wait_unpopped(seconds_timeout = 1)
  expect_equal(nanonext::cv_value(x$condition), 0L)
  expect_equal(x$unpopped, 1L)
  expect_equal(x$popped, 0L)
})

crew_test("relay wait_resolved()", {
  x <- crew_relay()
  cv <- nanonext::cv()
  x$from <- cv
  x$start()
  x$wait_resolved(seconds_timeout = 1, resolved = 0L)
  x$wait_resolved(seconds_timeout = 0, resolved = 1L)
  expect_equal(nanonext::cv_value(x$condition), 0L)
  expect_equal(x$unpopped, 0L)
  expect_equal(x$popped, 0L)
  nanonext::cv_signal(x$condition)
  nanonext::msleep(250)
  expect_equal(nanonext::cv_value(x$condition), 1L)
  x$wait_resolved(seconds_timeout = 1, resolved = 1L)
  expect_equal(nanonext::cv_value(x$condition), 1L)
  expect_equal(x$unpopped, 0L)
  expect_equal(x$popped, 0L)
})
