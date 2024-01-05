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
#' @description AWS Batch job definition `R6` class
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
    #'   to list all users (not recommended).
    dispatchers = function(user = crew::crew_user()) {
      crew_monitor_pids(pattern = "mirai::dispatcher", user = user)
    },
    #' @description List the process IDs of the locally running `mirai` daemon
    #'   processes which are not `crew` workers. The [crew_async()]
    #'   object can launch such processes: for example, when a positive
    #'   integer is supplied to the `processes` argument of e.g.
    #'   `crew.aws.batch::crew_controller_aws_batch()`.
    #' @return Integer vector of process IDs of the locally running
    #'   `mirai` daemon processes which are not `crew` workers.
    #' @param user Character of length 1, user ID to filter on. `NULL`
    #'   to list all users (not recommended).
    daemons = function(user = crew::crew_user()) {
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
    #'   to list all users (not recommended).
    workers = function(user = crew::crew_user()) {
      crew_monitor_pids(pattern = "crew::crew_worker", user = user)
    },
    #' @description Terminate the given process IDs.
    #' @details Termination happens with the operating system signal
    #'   given by [crew_terminate_signal()].
    #' @return `NULL` (invisibly)
    #' @param pids Integer vector of process IDs of local processes to
    #'   terminate.
    terminate = function(pids) {
      walk(
        as.integer(pids),
        ~ps::ps_send_signal(
          p = ps::ps_handle(.x),
          sig = crew_terminate_signal()
        )
      )
    }
  )
)

crew_monitor_pids <- function(pattern, user) {
  processes <- ps::ps(user = user)
  commands <- map_chr(x = processes$ps_handle, f = process_command)
  filter <- grepl(pattern = pattern, x = as.character(commands), fixed = TRUE)
  as.integer(sort(processes$pid[filter]))
}
