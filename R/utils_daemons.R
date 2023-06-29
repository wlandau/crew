daemons_info <- function(name) {
  envir <- new.env(parent = emptyenv())
  crew_retry(
    fun = ~{
      daemons <- mirai::daemons(.compute = name)$daemons
      valid <- is.matrix(daemons) && all(dim(daemons) > 0L)
      envir$daemons <- daemons
      envir$valid <- valid
    },
    seconds_interval = 0.5,
    seconds_timeout = 60,
    error = FALSE
  )
  daemons <- .subset2(envir, "daemons")
  valid <- .subset2(envir, "valid")
  if_any(valid, daemons, daemons_error(daemons, name))
}

daemons_error <- function(daemons, name) {
  message <- sprintf("invalid daemons: %s\n", deparse1(daemons))
  pid <- environment(mirai::daemons)$..[[name]]$pid
  exists <- !is.null(pid) &&
    !inherits(
      try(handle <- ps::ps_handle(pid = pid), silent = TRUE),
      "try-error"
    )
  info <- if_any(
    exists,
    sprintf(
      "dispatcher running at pid %s with status \"%s\". Connection issue?",
      pid,
      ps::ps_status(handle)
    ),
    paste(
      "The mirai dispatcher is not running. Please call the start() method",
      "of the controller (e.g. your_controller$start()",
      "before using methods like push(), collect(), and scale().",
      "If you already did, then the dispatcher may have exited too early."
    )
  )
  crew_error(paste(message, info))
}
