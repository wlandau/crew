crew_test("crew_controller_local() saturated", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    workers = 4L,
    seconds_idle = 360,
    seconds_interval = 0.1
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  expect_false(x$saturated())
  x$push(Sys.sleep(100))
  expect_false(x$saturated())
  x$push(Sys.sleep(100))
  expect_false(x$saturated())
  x$push(Sys.sleep(100))
  expect_false(x$saturated())
  x$push(Sys.sleep(100))
  expect_true(x$saturated())
  x$push(Sys.sleep(100))
  expect_true(x$saturated())
  x$terminate()
})

crew_test("crew_controller_group() saturated", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    name = "a",
    workers = 4L,
    seconds_idle = 360,
    seconds_interval = 0.1
  )
  b <- crew_controller_local(
    name = "b",
    workers = 4L,
    seconds_idle = 360,
    seconds_interval = 0.1
  )
  x <- crew_controller_group(a, b)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  expect_false(x$saturated())
  expect_false(x$saturated(controllers = "a"))
  expect_false(x$saturated(controllers = "b"))
  x$push(Sys.sleep(100), controller = "b")
  expect_false(x$saturated())
  expect_false(x$saturated(controllers = "a"))
  expect_false(x$saturated(controllers = "b"))
  x$push(Sys.sleep(100), controller = "b")
  expect_false(x$saturated())
  expect_false(x$saturated(controllers = "a"))
  expect_false(x$saturated(controllers = "b"))
  x$push(Sys.sleep(100), controller = "b")
  expect_false(x$saturated())
  expect_false(x$saturated(controllers = "a"))
  expect_false(x$saturated(controllers = "b"))
  x$push(Sys.sleep(100), controller = "b")
  expect_false(x$saturated())
  expect_false(x$saturated(controllers = "a"))
  expect_true(x$saturated(controllers = "b"))
  x$push(Sys.sleep(100), controller = "b")
  expect_false(x$saturated())
  expect_false(x$saturated(controllers = "a"))
  expect_true(x$saturated(controllers = "b"))
  x$terminate()
})
