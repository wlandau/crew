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
    controller_n_new_workers(demand = 3L, auto_scale = "demand"),
    3L
  )
  expect_equal(
    controller_n_new_workers(demand = 0L, auto_scale = "demand"),
    0L
  )
  expect_equal(
    controller_n_new_workers(demand = 3L, auto_scale = "one"),
    1L
  )
  expect_equal(
    controller_n_new_workers(demand = 0L, auto_scale = "demand"),
    0L
  )
  expect_equal(
    controller_n_new_workers(demand = 3L, auto_scale = "none"),
    0L
  )
  expect_equal(
    controller_n_new_workers(demand = 0L, auto_scale = "none"),
    0L
  )
})
