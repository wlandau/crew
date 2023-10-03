crew_until <- function(condition, seconds_timeout) {
  on.exit(nanonext::cv_signal(condition))
  nanonext::until(cv = condition, msec = 1000 * seconds_timeout)
}

condition_offset <- 1e4L
