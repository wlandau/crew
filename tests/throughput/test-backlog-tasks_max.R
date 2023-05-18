library(crew)
controller <- crew_controller_local(
  workers = 20L,
  tasks_max = 100
)
controller$start()
names <- character(0L)
index <- 0L
n_tasks <- 6000L
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
testthat::expect_equal(length(controller$queue), 0L)
controller$terminate()
