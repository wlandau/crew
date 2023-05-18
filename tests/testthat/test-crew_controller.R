crew_test("is_inactive()", {
  daemons <- cbind(
    online = c(0L, 1L, 0L, 0L, 1L, 0L),
    instance = c(0L, 1L, 1L, 0L, 1L, 1L)
  )
  launching <- rep(c(TRUE, FALSE), each = 3L)
  out <- is_inactive(daemons = daemons, launching = launching)
  expect_equal(out, c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE))
})

crew_test("is_lost() all expected", {
  expected <- rep(TRUE, 6L)
  daemons <- cbind(
    online = c(0L, 1L, 0L, 0L, 1L, 0L),
    instance = c(0L, 1L, 1L, 0L, 1L, 1L)
  )
  launching <- rep(c(TRUE, FALSE), each = 3L)
  out <- is_lost(expected = expected, daemons = daemons, launching = launching)
  expect_equal(out, c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
})

crew_test("is_lost() none expected", {
  expected <- rep(FALSE, 6L)
  daemons <- cbind(
    online = c(0L, 1L, 0L, 0L, 1L, 0L),
    instance = c(0L, 1L, 1L, 0L, 1L, 1L)
  )
  launching <- rep(c(TRUE, FALSE), each = 3L)
  out <- is_lost(expected = expected, daemons = daemons, launching = launching)
  expect_equal(out, rep(FALSE, 6L))
})

crew_test("deprecate auto_scale", {
  skip_on_cran()
  expect_warning(
    crew_controller(
      router = crew_router(),
      launcher = crew_launcher_local(),
      auto_scale = "demand"
    ),
    class = "crew_deprecate"
  )
})
