library(crew)
x <- crew_mirai_controller_callr(name = "test", workers = 2L)
x$connect()
n <- 1e3
x$launch(n = 2L)
time <- system.time({
  for (index in seq_len(n)) {
    name <- paste0("task_", index)
    x$push(name = name, command = ps::ps_pid(), scale = FALSE)
    # message(paste("push", name)) # nolint
  }
})
message(time["elapsed"])
results <- list()
px <- proffer::pprof({
#time <- system.time({
  counter <- 0L
  while (length(results) < n) {
    counter <- counter + 1L
    collect <- FALSE
    if (counter > 100L) {
      collect <- TRUE
      counter <- 0L
    }
    out <- x$pop(collect = collect)
    if (!is.null(out)) {
      results[[length(results) + 1L]] <- out
      # message(paste("done", out$name, out$result[[1]])) # nolint
    }
  }
})
message(time["elapsed"])
x$terminate()
results <- tibble::as_tibble(do.call(rbind, results))
results$result <- as.integer(results$result)
table(results$result)
