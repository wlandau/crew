#' @title Launcher abstract class
#' @export
#' @family launchers
#' @description `R6` abstract class to build other subclasses
#'   which launch and manage workers.
crew_class_launcher <- R6::R6Class(
  classname = "crew_class_launcher",
  cloneable = FALSE,
  public = list(
    #' @field workers Named list of workers, where the names are the
    #'   sockets they connect to.
    workers = list(),
    #' @field seconds_start Seconds of startup time to allow.
    #'   A worker is unconditionally assumed to be alive
    #'   from the moment of its launch until `seconds_start` seconds later.
    #'   After `seconds_start` seconds, the worker is only
    #'   considered alive if it is actively connected to its assign websocket.
    seconds_start = NULL,
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
    #' @param seconds_start Seconds of startup time to allow.
    #'   A worker is unconditionally assumed to be alive
    #'   from the moment of its launch until `seconds_start` seconds later.
    #'   After `seconds_start` seconds, the worker is only
    #'   considered alive if it is actively connected to its assign websocket.
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
      seconds_start = NULL,
      idle_time = NULL,
      wall_time = NULL,
      poll_high = NULL,
      poll_low = NULL,
      launch_timeout = NULL,
      launch_wait = NULL,
      max_tasks = NULL,
      async_dial = NULL
    ) {
      self$seconds_start <- seconds_start
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
      true(is.list(self$workers))
      fields <- c(
        "seconds_start",
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
    },
    #' @description Get the workers that recently launched
    #'   (within `seconds_start` seconds).
    #' @return Character vector of worker websockets.
    starting = function() {
      threshold <- bench::hires_time() - self$seconds_start
      names(fltr(self$workers, ~(.x$start > threshold)))
    },
    #' @description Launch one or more workers.
    #' @details If a worker is already assigned to a socket,
    #'   the previous worker is terminated before the next
    #'   one is launched.
    #' @return `NULL` (invisibly).
    #' @param sockets Sockets where the workers will dial in.
    launch = function(sockets = character(0)) {
      for (socket in sockets) {
        self$terminate_worker(self$workers[[socket]])
        handle <- self$launch_worker(socket)
        self$workers[[socket]] <- list(
          handle = handle,
          socket = socket,
          start = bench::hires_time()
        )
      }
      invisible()
    },
    #' @description Terminate all workers.
    #' @return `NULL` (invisibly).
    terminate = function() {
      walk(self$workers, ~self$terminate_worker(.x$handle))
    }
  )
)
