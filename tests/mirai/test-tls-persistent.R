library(crew)
library(mirai)

# Efficient and convenient data structure to keep track of {mirai} tasks.
# It has a hash table for new tasks and a first-in/first-out linked list
# for resolved tasks. It calls nanonext::.unresolved() to collect resolved
# tasks, but otherwise it does not rely on {mirai}/{nanonext}. I highly doubt
# it is the source of the {crew} bugs in #88 or #89.
schedule <- crew::crew_schedule()
schedule$start()

# Start the {mirai} client and servers with TLS.
daemons(n = 20L, url = "wss://127.0.0.1:0", dispatcher = TRUE, token = TRUE)
for (url in environment(daemons)$..$default$urls) {
  launch_server(url = url)
  Sys.sleep(0.1)
}

# Run the tasks.
index <- 0L # current task
n_tasks <- 60000L # all tasks
results <- list()
while (index < n_tasks || schedule$nonempty()) { # while there is work to do
  # If there are still tasks to launch, launch one.
  if (index < n_tasks) {
    index <- index + 1L
    cat("push", index, "\n")
    task <- mirai(index, index = index)
    # The "schedule" is nothing fancy for the purposes of #88 and #89,
    # it is just a fast data structure for bookkeeping {mirai} objects
    # without the other frills in {crew}.
    schedule$push(task)
  }
  # Try to process the results of finished tasks.
  if (schedule$nonempty()) { # If there are still tasks to process...
    # Call nanonext::.unresolved() and move resolved tasks
    # from the hash table in schedule$pushed to the first-in/first-out
    # linked list in schedule$collected.
    schedule$collect()
    task <- schedule$pop() # Return a task that was resolved and collected.
    # pop() returns NULL if there is no resolved/collected task.
    if (!is.null(task)) {
      data <- task$data
      results[[data]] <- data
      cat("pop", data, "\n")
    }
  }
}

# Should be TRUE
all(sort(unlist(results)) == seq_len(n_tasks))

# Clean up the dispatcher.
daemons(n = 0L)
