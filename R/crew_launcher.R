#' @title Launcher abstract class
#' @export
#' @family launchers
#' @description `R6` abstract class to build other subclasses
#'   which launch and manage workers.
crew_class_launcher <- R6::R6Class(
  classname = "crew_class_launcher",
  cloneable = FALSE,
  public = list(
    #' @field idle_time Maximum number of seconds that a worker can idle
    #'   since the completion of the last task. If exceeded, the worker exits.
    idle_time = NULL,
    #' @field wall_time Soft wall time in seconds. See the `wall_time`
    #'   argument of `mirai::server()`.
    wall_time = NULL,
    #' @field poll_high High polling interval in seconds for the `mirai`
    #'   active queue.
    poll_high = NULL,
    #' @field poll_low Low polling interval in seconds for the `mirai`
    #'   active queue.
    poll_low = NULL,
    #' @field launch_timeout Number of seconds to time out
    #'   waiting for a new group of workers to launch.
    launch_timeout = NULL,
    #' @field launch_wait Number of seconds to wait between checks
    #'   that newly launched workers are ready to receive tasks.
    launch_wait = NULL,
    #' @field max_tasks Maximum number of tasks that a worker will do before
    #'   exiting.
    max_tasks = NULL,
    #' @field async_dial Logical, whether the `mirai` workers should dial in
    #'   asynchronously. See the `asyncdial` argument of `mirai::server()`.
    async_dial = NULL,
    #' @description `mirai` launcher constructor.
    #' @return An `R6` object with the launcher.
    #' @param idle_time Maximum number of seconds that a worker can idle
    #'   since the completion of the last task. If exceeded, the worker exits.
    #' @param wall_time Soft wall time in seconds. See the `wall_time`
    #'   argument of `mirai::server()`.
    #' @param poll_high High polling interval in seconds for the `mirai`
    #'   active queue.
    #' @param poll_low Low polling interval in seconds for the `mirai`
    #'   active queue.
    #' @param launch_timeout Number of seconds to time out
    #'   waiting for a new group of workers to launch.
    #' @param launch_wait Number of seconds to wait between checks
    #'   that newly launched workers are ready to receive tasks.
    #' @param max_tasks Maximum number of tasks that a worker will do before
    #'   exiting.
    #' @param async_dial Logical, whether the `mirai` workers should dial in
    #'   asynchronously. See the `asyncdial` argument of `mirai::server()`.
    initialize = function(
      idle_time = NULL,
      wall_time = NULL,
      poll_high = NULL,
      poll_low = NULL,
      launch_timeout = NULL,
      launch_wait = NULL,
      max_tasks = NULL,
      async_dial = NULL
    ) {
      self$idle_time <- idle_time
      self$wall_time <- wall_time
      self$poll_high <- poll_high
      self$poll_low <- poll_low
      self$launch_timeout <- launch_timeout
      self$launch_wait <- launch_wait
      self$max_tasks <- max_tasks
      self$async_dial <- async_dial
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly).
    validate = function() {
      fields <- c(
        "idle_time",
        "wall_time",
        "poll_high",
        "poll_low",
        "launch_timeout",
        "launch_wait",
        "max_tasks"
      )
      for (field in fields) {
        true(self[[field]], is.numeric(.), . > 0, length(.) == 1L, !anyNA(.))
      }
      true(self$async_dial, isTRUE(.) || isFALSE(.))
      invisible()
    }
  )
)
