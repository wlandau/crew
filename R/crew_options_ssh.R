#' @title SSH options.
#' @export
#' @family options
#' @description [crew_options_ssh()] configures the
#'   `crew` controller to connect to workers through
#'   SSH connections.
#'   This is useful when the workers run on remote machines and the
#'   local IP addresses or host names of the machines running those workers
#'   are known in advance.
#' @details `crew` controllers with SSH works differently than the
#'   other controllers.
#'   With SSH, `crew` launches all its workers in advance, and it
#'   keeps them running for the duration of the controller.
#'   In addition, the machines you specify with `host` have to exist and
#'   already be running in advance.
#' @return A classed list of options for SSH connections.
#' @param host A character vector of one or more local IP addresses or
#'   host names of remote machines running `crew` workers.
#'   These IPs/hosts must be known and already running before you
#'   start the controller.
#'   The length of `host` equals the number of workers that
#'   will launch, and you can repeat the same IP addresses if you want
#'   to launch multiple workers on the same machine.
#' @param port Positive integer of length 1, port number to use for SSH.
#' @param seconds_timeout Positive numeric of length 1,
#'   number of seconds to allow before timing out.
#'   See the `timeout` argument in `?mirai::ssh_config` for details.
#' @param command Character string, command used to run SSH.
#'   See the `command` argument in `?mirai::ssh_config` for details.
#' @examples
#'   crew_options_ssh(host = "10.0.0.4")
crew_options_ssh <- function(
  host,
  port = 22L,
  seconds_timeout = 10,
  command = "ssh"
) {
  out <- structure(
    list(
      host = host,
      port = port,
      seconds_timeout = seconds_timeout,
      command = command
    ),
    class = c("crew_options_ssh", "crew_options")
  )
  crew_options_ssh_validate(out)
  out
}

crew_options_ssh_validate <- function(options) {
  crew_assert(
    inherits(options, "crew_options_ssh"),
    message = "options_ssh must come from crew_options_ssh()."
  )
  crew_assert(
    options$host,
    is.character(.),
    !anyNA(.),
    nzchar(.),
    message = c(
      "In crew_options_ssh(), host must be a valid nonempty character vector."
    )
  )
  crew_assert(
    options$port,
    is.numeric(.),
    is.finite(.),
    length(.) == 1L,
    . >= 0L,
    message = "In crew_options_ssh(), port must be a finite positive integer."
  )
  crew_assert(
    options$seconds_timeout,
    is.numeric(.),
    is.finite(.),
    length(.) == 1L,
    . >= 0L,
    message = paste(
      "In crew_options_ssh(), seconds_timeout
      must be a finite positive integer."
    )
  )
  crew_assert(
    options$command,
    is.character(.),
    !anyNA(.),
    nzchar(.),
    message = paste(
      "In crew_options_ssh(), command must be a",
      "valid nonempty character vector."
    )
  )
}
