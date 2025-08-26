mirai_status <- function(profile, seconds_interval, seconds_timeout) {
  envir <- new.env(parent = emptyenv())
  iterate <- function() {
    status <- mirai::status(.compute = profile)
    valid <- is.list(status)
    retry <- is.numeric(status) && identical(as.integer(status), 5L)
    if_any(
      valid || retry,
      NULL,
      mirai_status_error(status = status, profile = profile)
    )
    envir$status <- status
    envir$valid <- valid
    valid
  }
  crew_retry(
    fun = iterate,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    error = FALSE,
    assertions = FALSE
  )
  status <- .subset2(envir, "status")
  valid <- .subset2(envir, "valid")
  if_any(valid, status, mirai_status_error(status, profile))
}

mirai_status_error <- function(status, profile) {
  message <- sprintf("'errorValue' int %s\n", nanonext::nng_error(status))
  info <- paste(
    "\nmirai::status(.compute = controller$client$profile) errored out",
    "during normal {crew} operations",
    "(probably auto-scaling or counting resolved tasks).",
    "It is possible that the mirai::dispatcher() R process is no longer",
    "running on your local machine.",
    "\n\n",
    "Please try upgrading R packages {nanonext}, {mirai}, and {crew}",
    "to their latest released versions. (Likewise {targets}, {crew.cluster}",
    "and/or {crew.aws.batch} if you are using them.)",
    "Upgrading these packages solves many kinds of errors.",
    "\n\nAnother possibility is an out-of-memory error.",
    "The dispatcher process can run out of memory if it is overwhelmed with",
    "data objects too large or too many to comfortably fit inside a single",
    "R process. Please read",
    "https://wlandau.github.io/crew/articles/logging.html to learn",
    "about proactive resource usage logging which can provide useful",
    "data in the event of a crash.",
    "\n\nIf memory consumption is a problem, then as a workaround,",
    "you could make each task save/load files instead of using/returning",
    "large R objects in memory.",
    "If you are using {targets}, you might consider",
    "storage = \"worker\", retrieval = \"worker\", and/or",
    "cloud storage as documented at",
    "https://books.ropensci.org/targets/performance.html and",
    "https://books.ropensci.org/targets/cloud-storage.html."
  )
  crew_error(paste(message, info))
}

mirai_resolved <- function(task) {
  !is_mirai(task) || !nanonext::.unresolved(task)
}

mirai_resolve <- function(task, launching) {
  if (mirai::is_mirai(task)) {
    mirai::call_mirai(task)
    mirai_assert(task, launching)
    task$data
  } else {
    task
  }
}

mirai_wait <- function(tasks, launching) {
  mirai::call_mirai(tasks)
  lapply(tasks, mirai_assert, launching = launching)
  invisible()
}

mirai_assert <- function(task, launching) {
  if (!mirai::is_mirai(task)) {
    return()
  }
  data <- .subset2(task, "data")
  if (mirai::is_mirai_error(data)) {
    if (launching) {
      crew_error(
        message = paste(
          "Error asynchronously launching a worker:",
          mirai_condition_message(data)
        )
      )
    } else {
      crew_warning(
        message = paste(
          "Error asynchronously terminating a worker:",
          mirai_condition_message(data)
        )
      )
    }
  }
}

mirai_condition_message <- function(data) {
  tryCatch(
    as.character(data),
    error = function(condition) attr(data, "message")
  )
}
