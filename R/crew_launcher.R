#' @title Launcher abstract class
#' @export
#' @family launchers
#' @description `R6` abstract class to build other subclasses
#'   which launch and manage workers.
crew_class_launcher <- R6::R6Class(
  classname = "crew_class_launcher",
  cloneable = FALSE,
  public = list(
    #' @field workers Data frame of worker information.
    workers = tibble::tibble(
      socket = character(0L),
      start = numeric(0L),
      handle = list()
    ),
    #' @field seconds_launch See the constructor for details.
    seconds_launch = NULL,
    #' @field seconds_idle See the constructor for details.
    seconds_idle = NULL,
    #' @field seconds_wall See the constructor for details.
    seconds_wall = NULL,
    #' @field seconds_exit See the constructor for details.
    seconds_exit = NULL,
    #' @field seconds_poll_high See the constructor for details.
    seconds_poll_high = NULL,
    #' @field seconds_poll_low See the constructor for details.
    seconds_poll_low = NULL,
    #' @field tasks_max See the constructor for details.
    tasks_max = NULL,
    #' @field tasks_timers See the constructor for details.
    tasks_timers = NULL,
    #' @field async_dial See the constructor for details.
    async_dial = NULL,
    #' @description Launcher constructor.
    #' @return An `R6` object with the launcher.
    #' @param seconds_launch Seconds of launchup time to allow.
    #'   A worker is unconditionally assumed to be alive
    #'   from the moment of its launch until `seconds_launch` seconds later.
    #'   After `seconds_launch` seconds, the worker is only
    #'   considered alive if it is actively connected to its assign websocket.
    #' @param seconds_idle Maximum number of seconds that a worker can idle
    #'   since the completion of the last task. If exceeded, the worker exits.
    #'   But the timer does not launch until `tasks_timers` tasks
    #'   have completed.
    #'   See the `idletime` argument of `mirai::server()`.
    #' @param seconds_wall Soft wall time in seconds.
    #'   The timer does not launch until `tasks_timers` tasks
    #'   have completed.
    #'   See the `walltime` argument of `mirai::server()`.
    #' @param seconds_exit Number of seconds to wait for NNG websockets
    #'   to finish sending large data (in case an exit signal is received).
    #'   See the `exitdelay` argument of `mirai::server()`.
    #' @param seconds_poll_high High polling interval in seconds for the
    #'   `mirai` active queue. See the `pollfreqh` argument of
    #'   `mirai::server()`.
    #' @param seconds_poll_low Low polling interval in seconds for the `mirai`
    #'   active queue. See the `pollfreql` argument of
    #'   `mirai::server()`.
    #' @param tasks_max Maximum number of tasks that a worker will do before
    #'   exiting. See the `maxtasks` argument of `mirai::server()`.
    #' @param tasks_timers Number of tasks to do before activating
    #'   the timers for `seconds_idle` and `seconds_wall`.
    #'   See the `timerlaunch` argument of `mirai::server()`.
    #' @param async_dial Logical, whether the `mirai` workers should dial in
    #'   asynchronously. See the `asyncdial` argument of `mirai::server()`.
    initialize = function(
      seconds_launch = NULL,
      seconds_idle = NULL,
      seconds_wall = NULL,
      seconds_exit = NULL,
      seconds_poll_high = NULL,
      seconds_poll_low = NULL,
      tasks_max = NULL,
      tasks_timers = NULL,
      async_dial = NULL
    ) {
      self$seconds_launch <- seconds_launch
      self$seconds_idle <- seconds_idle
      self$seconds_wall <- seconds_wall
      self$seconds_exit <- seconds_exit
      self$seconds_poll_high <- seconds_poll_high
      self$seconds_poll_low <- seconds_poll_low
      self$tasks_max <- tasks_max
      self$tasks_timers <- tasks_timers
      self$async_dial <- async_dial
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(is.list(self$workers))
      fields <- c(
        "seconds_launch",
        "seconds_idle",
        "seconds_wall",
        "seconds_exit",
        "seconds_poll_high",
        "seconds_poll_low",
        "tasks_max",
        "tasks_timers"
      )
      for (field in fields) {
        true(self[[field]], is.numeric(.), . >= 0, length(.) == 1L, !anyNA(.))
      }
      true(self$async_dial, isTRUE(.) || isFALSE(.))
      true(self$workers, is.null(.) || is.data.frame(.))
      invisible()
    },
    #' @description Argument list for `mirai::server()`
    #' @return List of arguments for `mirai::server()`
    #' @param socket Character of length 1, websocket address of the worker
    #'   to launch.
    args = function(socket) {
      list(
        url = socket,
        nodes = NULL,
        asyncdial = self$async_dial,
        maxtasks = self$tasks_max,
        idletime = self$seconds_idle * 1000,
        walltime = self$seconds_wall * 1000,
        timerlaunch = self$tasks_timers,
        exitdelay = self$seconds_exit * 1000,
        pollfreqh = self$seconds_poll_high * 1000,
        pollfreql = self$seconds_poll_low * 1000
      )
    },
    #' @description Populate the workers data frame.
    #' @return `NULL` (invisibly).
    #' @param sockets Character vector of worker websockets.
    populate = function(sockets) {
      self$workers <- tibble::tibble(
        socket = sockets,
        start = rep(-Inf, length(sockets)),
        handle = replicate(length(sockets), NA, simplify = FALSE)
      )
      invisible()
    },
    #' @description Get the workers that attempted a launch
    #'   within `seconds_launch` seconds ago.
    #' @return Character vector of worker websockets.
    launching = function() {
      elapsed <- bench::hires_time() - self$workers$launch
      as.character(self$workers$socket[elapsed < self$seconds_launch])
    },
    #' @description Get the workers that were supposed to have
    #'   launched (within the `seconds_launch`-second window).
    #' @return Character vector of worker websockets.
    launched = function() {
      elapsed <- bench::hires_time() - self$workers$launch
      as.character(self$workers$socket[elapsed > self$seconds_launch])
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
          socket = socket,
          launch = bench::hires_time(),
          handle = handle
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
