#' @title Create a local monitor object.
#' @export
#' @family monitor
#' @description Create an `R6` object to monitor local processes created by
#'   `crew` and `mirai`.
crew_monitor_local <- function() {
  crew_class_monitor_local$new()
}

#' @title Local monitor class
#' @export
#' @family monitor
#' @description Local monitor `R6` class
#' @details See [crew_monitor_local()].
crew_class_monitor_local <- R6::R6Class(
  classname = "crew_class_monitor_local",
  cloneable = FALSE,
  public = list(
    #' @description List the process IDs of the running `mirai` dispatcher
    #'   processes.
    #' @return Integer vector of process IDs of the running `mirai` dispatcher
    #'   processes.
    #' @param user Character of length 1, user ID to filter on. `NULL`
    #'   to list processes of all users (not recommended).
    dispatchers = function(user = ps::ps_username()) {
      crew_monitor_pids(pattern = "mirai::dispatcher", user = user)
    },
    #' @description List the process IDs of the locally running `mirai` daemon
    #'   processes which are not `crew` workers.
    #' @return Integer vector of process IDs of the locally running
    #'   `mirai` daemon processes which are not `crew` workers.
    #' @param user Character of length 1, user ID to filter on. `NULL`
    #'   to list processes of all users (not recommended).
    daemons = function(user = ps::ps_username()) {
      crew_monitor_pids(pattern = "mirai::daemon", user = user)
    },
    #' @description List the process IDs of locally running `crew` workers
    #'   launched by the local controller ([crew_controller_local()]).
    #' @details Only the workers running on your local computer are listed.
    #'   Workers that are not listed include jobs on job schedulers like
    #'   SLURM or jobs on cloud services like AWS Batch. To monitor
    #'   those worker processes, please consult the monitor objects in
    #'   the relevant third-party launcher plugins such as `crew.cluster`
    #'   and `crew.aws.batch`.
    #' @return Integer vector of process IDs of locally running `crew` workers
    #'   launched by the local controller ([crew_controller_local()]).
    #' @param user Character of length 1, user ID to filter on. `NULL`
    #'   to list processes of all users (not recommended).
    workers = function(user = ps::ps_username()) {
      # Covered in tests/testthat/test-crew_monitor_local.R.
      # Suppressed in {covr} because of readRDS()-related errors.
      # nocov start
      crew_monitor_pids(pattern = "crew::crew_worker", user = user)
      # nocov end
    },
    #' @description Terminate the given process IDs.
    #' @details Termination happens with the operating system signal
    #'   given by [crew_terminate_signal()].
    #' @return `NULL` (invisibly).
    #' @param pids Integer vector of process IDs of local processes to
    #'   terminate.
    terminate = function(pids) {
      # Tested in tests/testthat/test-crew_monitor_local.R
      # but avoiding on covr because of the infamous
      # RDS file corruption error caused by terminating external processes.
      lapply(as.integer(pids), crew_terminate_process) # nocov
    }
  )
)

crew_monitor_pids <- function(pattern, user) {
  crew_assert(
    user %|||% "unknown",
    is.character(.),
    length(.) == 1L,
    !anyNA(.),
    nzchar(.),
    message = "invalid user name"
  )
  processes <- ps::ps(user = user)
  commands <- map_chr(x = processes$ps_handle, f = process_command)
  filter <- grepl(pattern = pattern, x = as.character(commands), fixed = TRUE)
  as.integer(sort(processes$pid[filter]))
}
