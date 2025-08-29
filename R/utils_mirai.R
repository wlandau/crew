mirai_status <- function(profile, seconds_interval, seconds_timeout) {
  envir <- new.env(parent = emptyenv())
  iterate <- function() {
    status <- mirai::info(.compute = profile)
    envir$valid <- !is.null(status)
    envir$status <- status
    !is.null(status)
  }
  crew_retry(
    fun = iterate,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    error = FALSE,
    assertions = FALSE
  )
  status <- .subset2(envir, "status")
  if_any(.subset2(envir, "valid"), status, mirai_status_error(profile))
}

mirai_status_error <- function(profile) {
  info <- paste(
    "mirai::info(.compute = controller$client$profile) errored out",
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
  crew_error(info)
}
