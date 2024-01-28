crew_test("heavy task load for map()", {
  controller <- crew_controller_local(workers = 20L)
  on.exit(controller$terminate())
  controller$start()
  for (index in seq_len(3L)) {
    out <- controller$map(
      command = {
        if (x <= 1000L) {
          stop(x)
        } else if (x <= 4000L) {
          warning(x)
        }
        x
      },
      iterate = list(x = seq_len(6000L)),
      error = "silent",
      warnings = FALSE
    )
    testthat::expect_equal(length(controller$list), 0L)
    sum <- controller$summary()
    testthat::expect_equal(sum(sum$errors), index * 1000L)
    testthat::expect_equal(sum(sum$warnings), index * 3000L)
    name <- as.integer(out$name)
    testthat::expect_equal(name, seq_len(6000L))
    testthat::expect_true(all((name <= 1000L) == !is.na(out$error)))
    testthat::expect_true(
      all(
        ((name > 1000L) & (name <= 4000L)) == !is.na(out$warnings)
      )
    )
  }
})

crew_test("map() auto-scales", {
  controller <- crew_controller_local(workers = 20L, tasks_max = 1L)
  on.exit(controller$terminate())
  controller$start()
  out <- controller$map(
    command = {
      if (x <= 100L) {
        stop(x)
      } else if (x <= 400L) {
        warning(x)
      }
      x
    },
    iterate = list(x = seq_len(600L)),
    error = "silent",
    warnings = FALSE
  )
  testthat::expect_equal(length(controller$list), 0L)
  sum <- controller$summary()
  testthat::expect_equal(sum(sum$errors), 100L)
  testthat::expect_equal(sum(sum$warnings), 300L)
  name <- as.integer(out$name)
  testthat::expect_equal(name, seq_len(600L))
  testthat::expect_true(all((name <= 100L) == !is.na(out$error)))
  testthat::expect_true(
    all(
      ((name > 100L) & (name <= 400L)) == !is.na(out$warnings)
    )
  )
  controller$terminate()
})

crew_test("map() under load", {
  skip("very long test")
  controller <- crew::crew_controller_local(workers = 25, tasks_max = 100)
  on.exit(controller$terminate())
  controller$start()
  out <- controller$map(
    command = Sys.sleep(1),
    iterate = list(x = seq_len(10000))
  )
})
