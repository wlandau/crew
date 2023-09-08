library(crew)
library(mirai)

# Implements throttling to avoid overburdening the {mirai} dispatcher.
throttler <- crew::crew_schedule()

# Efficient and convenient data structure to keep track of {mirai} tasks.
# It has a hash table for new tasks and a first-in/first-out linked list
# for resolved tasks. It calls nanonext::.unresolved() to collect resolved
# tasks, but otherwise it does not rely on {mirai}/{nanonext}. I highly doubt
# it is the source of the {crew} bugs in #88 or #89.
schedule <- crew::crew_schedule()
schedule$start()

# Start the {mirai} client.
n <- 20L
mirai::daemons(
  n = n,
  url = "ws://127.0.0.1:5000",
  dispatcher = TRUE,
  token = TRUE
)

# Mutable structure with {crew} worker info. This is the primary
# data structure of each {crew} launcher.
workers <- new.env(parent = emptyenv()) # For mutability.
workers$workers <- tibble::tibble(
  handle = replicate(n, new.env(), simplify = FALSE), # callr::r_bg() handles
  socket = nextget("urls"), # starting URLs
  launches = rep(0L, n), # number of times a worker was launched at this index
  launched = rep(FALSE, n), # FALSE if the worker is definitely done.
  assigned = rep(0L, n), # Cumulative "assigned" stat to check backlog (#79).
  complete = rep(0L, n) # Cumulative "complete" stat to check backlog (#79).
)

# For {mirai} servers with online == 0L and instance == 1L,
# rotate the websocket URL. Also set workers$launched to FALSE,
# which signals that tally() can safely update the cumulative
# "assigned" and "complete" statistics (#79).
rotate <- function(workers) {
  info <- mirai::daemons()$daemons
  done <- which(info[, "online"] < 1L & info[, "instance"] > 0L)
  for (index in done) {
    socket <- mirai::saisei(i = index, force = FALSE)
    if (!is.null(socket)) {
      workers$workers$socket[index] <- socket # Next launch is at this URL.
      workers$workers$launched[index] <- FALSE # Lets tally() update stats.
    }
  }
}

# For workers that are definitely done and not going to dial in until the
# next launch, update the cumulative "assigned" and "complete" which {crew}
# uses to detect backlogged workers (#79). A backlogged worker is a {mirai}
# server with more assigned than complete tasks. Detecting the backlog
# is important because if a worker is disconnected and backlogged,
# then {crew} will need to relaunch it so the backlogged tasks can run.
tally <- function(workers) {
  info <- mirai::daemons()$daemons
  index <- !(workers$workers$launched) # Workers safe to update.
  workers$workers$assigned[index] <- as.integer(info[index, "assigned"])
  workers$workers$complete[index] <- as.integer(info[index, "complete"])
  invisible()
}

# In {crew}, the scale() method of the launcher class
# re-launches all backlogged non-launched workers,
# and then it may launch additional non-launched workers
# in order to meet the demand of the task load.
# The scale() function below is a simplified version which launches
# all non-launched workers.
scale <- function(workers) {
  for (index in which(!workers$workers$launched)) { # non-launched workers
    # I would have used mirai::launch_server() here, but callr::r_bg()
    # allows me to manually terminate the server without calling
    # mirai::daemons(n = 0L). This is important for updating the final
    # assigned and complete tallies later on.
    workers$workers$handle[[index]] <- callr::r_bg(
      func = function(url) {
        mirai::server(
          url = url,
          maxtasks = 100L
        )
      },
      args = list(url = workers$workers$socket[index])
    )
    # Increment the launch count.
    workers$workers$launches[index] <- workers$workers$launches[index] + 1L
    # Signal to tally() to wait for this worker to complete
    # instead of updating the cumulative assigned and complete stats.
    workers$workers$launched[index] <- TRUE
  }
}

index <- 0L # current task
n_tasks <- 6000L # all tasks
while (index < n_tasks || schedule$nonempty()) { # while there is work to do
  if (!throttler$throttle()) { # avoid overburdening the {mirai} dispatcher
    rotate(workers) # Rotate the URLs of done workers.
    tally(workers) # Update the cumulative stats for done workers.
    scale(workers) # Re-launch all the done workers.
  }
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
      cat("pop", task$data, "\n")
    }
  }
}

# Manually terminate the workers without calling mirai::daemons(n = 0L).
# This allows the final tally to be updated correctly.
for (handle in workers$workers$handle) {
  if (inherits(handle, "r_process") && handle$is_alive()) {
    handle$kill()
  }
}

# Update the final tally and clean up the dispatcher.
rotate(workers)
tally(workers)
daemons(n = 0L)

# The cumulative assigned and complete statistics should be equal for
# each worker.
print("worker info")
print(workers$workers[, c("launches", "assigned", "complete")])

# Should equal n_tasks.
print("total assigned")
print(sum(workers$workers$assigned))

# Should equal n_tasks.
print("total complete")
print(sum(workers$workers$complete))

# The backlog should be 0 for all workers.
print("backlog per worker")
print(table(workers$workers$assigned - workers$workers$complete))
