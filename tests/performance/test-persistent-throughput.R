library(crew)
crew_session_start()
x <- crew_controller_callr(name = "test", workers = 2L)
x$start()
n <- 1e2
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
proffer::pprof({
time <- system.time({
  while (length(results) < n) {
    out <- x$pop()
    if (!is.null(out)) {
      results[[length(results) + 1L]] <- out
   #   message(paste("done", out$name, out$result[[1]])) # nolint
    }
  }
})
})
message(time["elapsed"])
x$terminate()
results <- tibble::as_tibble(do.call(rbind, results))
results$result <- as.integer(results$result)
table(results$result)
crew_session_terminate()
