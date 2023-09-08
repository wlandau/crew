library(crew)
library(mirai)

# Implements throttling to avoid overburdening the {mirai} dispatcher.
throttler <- crew::crew_schedule()

# Efficient and convenient data structure to keep track of {mirai} tasks.
schedule <- crew::crew_schedule()
schedule$start()

# Start the {mirai} client.
n <- 20L
mirai::daemons(
  n = n,
  url = "wss://127.0.0.1:5000",
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
      func = function(url, tls) {
        mirai::server(
          url = url,
          tls = tls,
          maxtasks = 100L
        )
      },
      args = list(
        url = workers$workers$socket[index],
        tls = nextget("tls")
      )
    )
    # Increment the launch count.
    workers$workers$launches[index] <- workers$workers$launches[index] + 1L
    # Signal to tally() to wait for this worker to complete
    # instead of updating the cumulative assigned and complete stats.
    workers$workers$launched[index] <- TRUE
  }
}

index <- 0L # current task
n_tasks <- 60000L # all tasks
results <- list()
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
      data <- task$data
      results[[data]] <- data
      cat("pop", data, "\n")
    }
  }
}

# Terminate the dispatcher.
daemons(n = 0L)

# Manually terminate any remaining workers.
for (handle in workers$workers$handle) {
  if (inherits(handle, "r_process") && handle$is_alive()) {
    handle$kill()
  }
}

# Check the results.
all(sort(as.integer(unlist(results))) == seq_len(n_tasks))
