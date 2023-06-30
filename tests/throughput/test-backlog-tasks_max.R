library(crew)
controller <- crew_controller_local(
  workers = 20L,
  tasks_max = 100,
  tls_enable = TRUE
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
      controller$push(
        name = as.character(index),
        command = TRUE
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
controller$terminate()
testthat::expect_equal(length(controller$schedule$pushed), 0L)
testthat::expect_equal(length(controller$schedule$collected), 0L)
testthat::expect_equal(sum(controller$launcher$workers$assigned), n_tasks)
testthat::expect_equal(sum(controller$launcher$workers$complete), n_tasks)
