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
  time_launch = rep(-Inf, n) # nanonext::mclock() at launch
)

# For {mirai} servers with online == 0L and instance == 1L,
# rotate the websocket URL.
rotate <- function(workers) {
  info <- mirai::status()$daemons
  is_done <- info[, "online"] < 1L &
    info[, "instance"] > 0L &
    (workers$workers$time_launch - 10000) < nanonext::mclock()
  for (index in which(is_done)) {
    socket <- mirai::saisei(i = index, force = FALSE)
    if (!is.null(socket)) {
      workers$workers$launched[index] <- FALSE
    }
  }
}

# In {crew}, the scale() method of the launcher class
# re-launches new worker instances to meet the demand of the task load.
# The method below just launches all inactive workers.
scale <- function(workers) {
  for (index in which(!workers$workers$launched)) { # non-launched workers
    # I would have used mirai::launch_server() here, but callr::r_bg()
    # allows me to manually terminate the server without calling
    # mirai::daemons(n = 0L). This is important for updating the final
    # assigned and complete tallies later on.
    workers$workers$handle[[index]] <- callr::r_bg(
      func = function(url, tls) {
        mirai::daemon(
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
    workers$workers$launches[index] <- workers$workers$launches[index] + 1L
    workers$workers$launched[index] <- TRUE
    workers$workers$time_launch[index] <- nanonext::mclock()
  }
}

index <- 0L # current task
n_tasks <- 6000L # all tasks
results <- list()
while (index < n_tasks || schedule$nonempty()) { # while there is work to do
  if (!throttler$throttle()) { # avoid overburdening the {mirai} dispatcher
    rotate(workers) # Rotate the URLs of done workers.
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
