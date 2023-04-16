crew_test("controller_demand()", {
  expect_equal(controller_demand(tasks = 2L, workers = 4L), 0L)
  expect_equal(controller_demand(tasks = 3L, workers = 4L), 0L)
  expect_equal(controller_demand(tasks = 4L, workers = 4L), 0L)
  expect_equal(controller_demand(tasks = 5L, workers = 4L), 1L)
  expect_equal(controller_demand(tasks = 6L, workers = 5L), 1L)
  expect_equal(controller_demand(tasks = 6L, workers = 4L), 2L)
  expect_equal(controller_demand(tasks = 6L, workers = 3L), 3L)
})

crew_test("controller_n_new_workers()", {
  expect_equal(
    controller_n_new_workers(demand = 3L, auto_scale = "demand", max = Inf),
    3L
  )
  expect_equal(
    controller_n_new_workers(demand = 3L, auto_scale = "demand", max = 1L),
    1L
  )
  expect_equal(
    controller_n_new_workers(demand = 0L, auto_scale = "demand", max = Inf),
    0L
  )
  expect_equal(
    controller_n_new_workers(demand = 3L, auto_scale = "one", max = Inf),
    1L
  )
  expect_equal(
    controller_n_new_workers(demand = 0L, auto_scale = "demand", max = Inf),
    0L
  )
  expect_equal(
    controller_n_new_workers(demand = 3L, auto_scale = "none", max = Inf),
    0L
  )
  expect_equal(
    controller_n_new_workers(demand = 0L, auto_scale = "none", max = Inf),
    0L
  )
})

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
