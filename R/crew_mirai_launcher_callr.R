#' @title Create a `mirai` launcher with `callr` workers.
#' @export
#' @family launchers
#' @description Create an `R6` object to launch and maintain
#'   `callr` workers for a `mirai` controller.
#' @inheritParams mirai::server
#' @param sockets TCP sockets for listening to the workers.
#' @param seconds_idle Maximum number of seconds that a worker can idle
#'   since the completion of the last task. If exceeded, the worker exits.
#' @param seconds_wall_time Soft wall time in seconds. See the `wall_time`
#'   argument of `mirai::server()`.
#' @param seconds_poll_high High polling interval in seconds for the `mirai`
#'   active queue.
#' @param seconds_poll_low Low polling interval in seconds for the `mirai`
#'   active queue.
#' @param async_dial Logical, whether the `mirai` workers should dial in
#'   asynchronously. See the `asyncdial` argument of `mirai::server()`.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' launcher <- crew_mirai_launcher_callr()
#' launcher$validate()
#' }
crew_mirai_launcher_callr <- function(
  sockets,
  seconds_idle = Inf,
  seconds_wall_time = Inf,
  seconds_poll_high = 5L,
  seconds_poll_low = 50L,
  max_tasks = Inf,
  async_dial = TRUE
) {
  true(sockets, length(.) > 0L, is.character(.), nzchar(.), !anyNA(.))
  workers <- as.list(rep(NA, length(sockets)))
  launcher <- crew_class_mirai_launcher$new(
    sockets = sockets,
    workers = workers,
    seconds_idle = seconds_idle,
    seconds_wall_time = seconds_wall_time,
    seconds_poll_high = seconds_poll_high,
    seconds_poll_low = seconds_poll_low,
    max_tasks = max_tasks,
    async_dial = async_dial
  )
  launcher$validate()
  launcher
}

#' @title `mirai` launcher class
#' @export
#' @family launchers
#' @description `R6` class to launch and manage `mirai` workers.
#' @details See [crew_mirai_launcher_callr()].
crew_class_mirai_launcher_callr <- R6::R6Class(
  classname = "crew_class_mirai_launcher_callr",
  cloneable = FALSE,
  public = list(
    #' @field sockets TCP sockets for listening to the workers.
    sockets = NULL,
    #' @field workers List of `callr::r_session()` handles for workers.
    #'   The handle is `NA` if it is not yet called.
    workers = NULL,
    #' @field seconds_idle Argument to `crew_mirai_launcher_callr()`.
    seconds_idle = NULL,
    #' @field seconds_wall_time Argument to `crew_mirai_launcher_callr()`.
    seconds_wall_time = NULL,
    #' @field seconds_poll_high Argument to `crew_mirai_launcher_callr()`.
    seconds_poll_high = NULL,
    #' @field seconds_poll_low Argument to `crew_mirai_launcher_callr()`.
    seconds_poll_low = NULL,
    #' @field max_tasks Argument to `crew_mirai_launcher_callr()`.
    max_tasks = NULL,
    #' @field async_dial Argument to `crew_mirai_launcher_callr()`.
    async_dial = NULL,
    #' @description `mirai` launcher constructor.
    #' @return An `R6` object with the launcher.
    #' @param sockets Argument to `crew_mirai_launcher_callr()`.
    #' @param workers Argument to `crew_mirai_launcher_callr()`.
    #' @param seconds_idle Argument to `crew_mirai_launcher_callr()`.
    #' @param seconds_wall_time Argument to `crew_mirai_launcher_callr()`.
    #' @param seconds_poll_high Argument to `crew_mirai_launcher_callr()`.
    #' @param seconds_poll_low Argument to `crew_mirai_launcher_callr()`.
    #' @param max_tasks Argument to `crew_mirai_launcher_callr()`.
    #' @param async_dial Argument to `crew_mirai_launcher_callr()`.
    initialize = function(
    sockets = NULL,
    workers = NULL,
    seconds_idle = NULL,
    seconds_wall_time = NULL,
    seconds_poll_high = NULL,
    seconds_poll_low = NULL,
    max_tasks = NULL,
    async_dial = NULL
    ) {
      self$sockets <- sockets
      self$workers <- workers
      self$seconds_idle <- seconds_idle
      self$seconds_wall_time <- seconds_wall_time
      self$seconds_poll_high <- seconds_poll_high
      self$seconds_poll_low <- seconds_poll_low
      self$max_tasks <- max_tasks
      self$async_dial <- async_dial
      invisible()
    },
    #' @description Disconnect at garbage collection time.
    #' @return `NULL` (invisibly).
    finalize = function() {
      self$disconnect()
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(self$sockets, is.character(.), length(.) > 0L, nzchar(.), !anyNA(.))
      true(self$workers, is.list(.), length(.) == length(self$sockets))
      fields <- c(
        "seconds_idle",
        "seconds_wall_time",
        "seconds_poll_high",
        "seconds_poll_low",
        "max_tasks"
      )
      for (field in fields) {
        true(self[[field]], is.numeric(.), . > 0, length(.) == 1L, !anyNA(.))
      }
      expect_true(self$async_dial, isTRUE(.) || isFALSE(.))
      invisible()
    }
    # TODO: POPULATE METHODS
  )
)
