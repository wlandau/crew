library(crew)
controller <- crew_controller_local(workers = 20L)
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
    error = "silent"
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
controller$terminate()
