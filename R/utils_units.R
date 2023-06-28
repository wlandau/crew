units_time <- function(x) {
  if (is.na(x)) {
    return("")
  }
  if (x > 3600 * 24 * 365) {
    return(paste(round(x / (3600 * 24 * 365), 3), "years"))
  }
  if (x > 3600 * 24 * 30) {
    return(paste(round(x / (3600 * 24 * 30), 3), "months"))
  }
  if (x > 3600 * 24) {
    return(paste(round(x / (3600 * 24), 3), "days"))
  }
  if (x > 3600) {
    return(paste(round(x / 3600, 3), "hours"))
  }
  if (x > 60) {
    return(paste(round(x / 60, 3), "minutes"))
  }
  paste(round(x, 3), "seconds")
}
