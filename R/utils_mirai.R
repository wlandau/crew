daemons_info <- function(name, seconds_interval, seconds_timeout) {
  envir <- new.env(parent = emptyenv())
  crew_retry(
    fun = ~{
      daemons <- mirai::status(.compute = name)$daemons
      valid <- is.matrix(daemons) && all(dim(daemons) > 0L)
      envir$daemons <- daemons
      envir$valid <- valid
    },
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    error = FALSE
  )
  daemons <- .subset2(envir, "daemons")
  valid <- .subset2(envir, "valid")
  if_any(valid, daemons, daemons_error(daemons, name))
}

daemons_error <- function(daemons, name) {
  message <- sprintf("'errorValue' int %s\n", nanonext::nng_error(daemons))
  pid <- mirai::nextget("pid", .compute = name)
  exists <- !is.null(pid) &&
    !inherits(
      try(handle <- ps::ps_handle(pid = pid), silent = TRUE),
      "try-error"
    )
  lines_common <- c(
    "Please also try upgrading R packages {nanonext}, {mirai}, and {crew}",
    "to their latest versions on CRAN. (Likewise with {targets} if you are",
    "using it.) Upgrading these packages solves many kinds of errors.",
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
  info <- if_any(
    exists,
    sprintf(
      paste(
        c(
          "A {mirai} dispatcher process is running at pid %s with status",
          "\"%s\". There may be a network connection issue,",
          "or the dispatcher is simply busy managing lots of tasks or",
          "uploading/downloading large datasets. Maybe try increasing",
          "seconds_timeout in your {crew} controller.\n\n",
          lines_common
        ),
        collapse = " "
      ),
      pid,
      ps::ps_status(handle)
    ),
    paste(
      c(
        "The {mirai} dispatcher is not running. If you are using {crew}",
        "without {targets}, be sure to call the start() method of the",
        "controller before doing anything else. If you already did,",
        "or if you are using {targets}, then the dispatcher process probably",
        "started and then failed.\n\n",
        lines_common
      ),
      collapse = " "
    )
  )
  crew_error(paste(message, info))
}

mirai_error <- function(task) {
  if_any(
    mirai::is_mirai(task) && mirai::is_mirai_error(task$data),
    as.character(task$data),
    NULL
  )
}
