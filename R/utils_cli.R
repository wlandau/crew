cli_bad <- function(message) {
  cli::cli_alert_danger(message)
}

cli_good <- function(message) {
  cli::cli_alert_success(message)
}

cli_li <- function(message) {
  cli::cli_ul()
  cli::cli_li(message)
  cli::cli_end()
}
