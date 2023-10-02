library(crew)
controller <- crew_controller_local(
  workers = 20L,
  seconds_idle = 1
)
controller$start()
names <- character(0L)
index <- 0L
n_tasks <- 60000L
system.time(
  while (index < n_tasks || !(controller$empty())) {
    if (index < n_tasks) {
      index <- index + 1L
      cat("submit", index, "\n")
      cat("submit", index, "\n")
      controller$push(
        name = as.character(index),
        command = Sys.sleep(0.005)
      )
    }
    out <- controller$pop()
    if (!is.null(out)) {
      cat("collect", out$name, "\n")
      names[[length(names) + 1L]] <- out$name
    }
  }
)
testthat::expect_equal(sort(as.integer(names)), seq_len(n_tasks))
controller$launcher$workers$launched <- FALSE
controller$launcher$tally()
testthat::expect_equal(controller$unresolved(), 0L)
controller$terminate()
testthat::expect_equal(length(controller$tasks), 0L)
testthat::expect_equal(sum(controller$launcher$workers$assigned), n_tasks)
testthat::expect_equal(sum(controller$launcher$workers$complete), n_tasks)
