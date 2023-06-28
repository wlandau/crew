daemons_info <- function(name) {
  out <- mirai::daemons(.compute = name)$daemons
  # Should not happen:
  # nocov start
  if (!daemons_valid(out)) {
    message <- sprintf("invalid daemons: %s\n", deparse1(out))
    pid <- environment(mirai::daemons)$..[[name]]$pid
    exists <- !is.null(pid) &&
      !inherits(
        try(handle <- ps::ps_handle(pid = pid), silent = TRUE),
        "try-error"
      )
    if (exists) {
      info <- sprintf(
        "dispatcher running at pid %s with status \"%s\". Connection issue?",
        pid,
        ps::ps_status(handle)
      )
    } else {
      info <- paste(
        "The mirai dispatcher is not running. Please call the start() method",
        "of the controller (e.g. your_controller$start()",
        "before using methods like push(), collect(), and scale().",
        "If you already did, then the dispatcher may have exited too early."
      )
    }
    crew_error(paste(message, info))
  }
  # nocov end
  out
}

daemons_valid <- function(daemons) {
  is.matrix(daemons) && all(dim(daemons) > 0L)
}
