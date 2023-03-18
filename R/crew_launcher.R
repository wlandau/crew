#' @title Create an abstract launcher.
#' @export
#' @keywords internal
#' @family launchers
#' @description This function is useful for inheriting argument documentation
#'   in functions that create custom third-party launchers. See
#'   `@inheritParams crew::crew_launcher` in the source code file of
#'   [crew_launcher_callr()].
#' @param name Name of the launcher.
#' @param seconds_launch Seconds of startup time to allow.
#'   A worker is unconditionally assumed to be alive
#'   from the moment of its launch until `seconds_launch` seconds later.
#'   After `seconds_launch` seconds, the worker is only
#'   considered alive if it is actively connected to its assign websocket.
#' @param seconds_interval Number of seconds between
#'   polling intervals waiting for certain internal
#'   synchronous operations to complete.
#' @param seconds_timeout Number of seconds until timing
#'   out while waiting for certain synchronous operations to complete.
#' @param seconds_idle Maximum number of seconds that a worker can idle
#'   since the completion of the last task. If exceeded, the worker exits.
#'   But the timer does not launch until `tasks_timers` tasks
#'   have completed.
#'   See the `idletime` argument of `mirai::server()`. `crew` does not
#'   excel with perfectly transient workers because it does not micromanage
#'   the assignment of tasks to workers, so please allow enough idle
#'   time for a new worker to be delegated a new task.
#' @param seconds_wall Soft wall time in seconds.
#'   The timer does not launch until `tasks_timers` tasks
#'   have completed.
#'   See the `walltime` argument of `mirai::server()`.
#' @param seconds_exit Number of seconds to wait for NNG websockets
#'   to finish sending large data (in case an exit signal is received).
#'   See the `exitlinger` argument of `mirai::server()`.
#' @param tasks_max Maximum number of tasks that a worker will do before
#'   exiting. See the `maxtasks` argument of `mirai::server()`.
#'   `crew` does not
#'   excel with perfectly transient workers because it does not micromanage
#'   the assignment of tasks to workers, it is recommended to set
#'   `tasks_max` to a value greater than 1.
#' @param tasks_timers Number of tasks to do before activating
#'   the timers for `seconds_idle` and `seconds_wall`.
#'   See the `timerlaunch` argument of `mirai::server()`.
#' @param async_dial Logical, whether the `mirai` workers should dial in
#'   asynchronously. See the `asyncdial` argument of `mirai::server()`.
#' @param cleanup Logical, whether to clean up global options and the
#'   global environment after every task.
#'   See the `cleanup` argument of `mirai::server()`.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_session_start()
#' router <- crew_router()
#' router$listen()
#' launcher <- crew_launcher_callr()
#' launcher$populate(sockets = router$sockets)
#' launcher$launch()
#' m <- mirai::mirai("result")
#' Sys.sleep(0.25)
#' m$data
#' router$terminate()
#' crew_session_terminate()
#' }
crew_launcher <- function(
  name = NULL,
  seconds_launch = 30,
  seconds_interval = 0.001,
  seconds_timeout = 10,
  seconds_idle = Inf,
  seconds_wall = Inf,
  seconds_exit = 0.1,
  tasks_max = Inf,
  tasks_timers = 0L,
  async_dial = TRUE,
  cleanup = FALSE
) {
  name <- as.character(name %|||% random_name())
  launcher <- crew_class_launcher_callr$new(
    name = name,
    seconds_launch = seconds_launch,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    seconds_exit = seconds_exit,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    async_dial = async_dial,
    cleanup = cleanup
  )
  launcher$validate()
  launcher
}

#' @title Launcher abstract class
#' @export
#' @family launchers
#' @description `R6` abstract class to build other subclasses
#'   which launch and manage workers.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_session_start()
#' router <- crew_router()
#' router$listen()
#' launcher <- crew_launcher_callr()
#' launcher$populate(sockets = router$sockets)
#' launcher$launch()
#' m <- mirai::mirai("result")
#' Sys.sleep(0.25)
#' m$data
#' router$terminate()
#' crew_session_terminate()
#' }
crew_class_launcher <- R6::R6Class(
  classname = "crew_class_launcher",
  cloneable = FALSE,
  portable = TRUE,
  public = list(
    #' @field workers Data frame of worker information.
    workers = tibble::tibble(
      socket = character(0L),
      launches = integer(0L),
      start = numeric(0L),
      token = character(0L),
      listener = list(),
      handle = list()
    ),
    #' @field name Name of the launcher.
    name = NULL,
    #' @field seconds_launch See [crew_launcher()].
    seconds_launch = NULL,
    #' @field seconds_interval See [crew_launcher()].
    seconds_interval = NULL,
    #' @field seconds_timeout See [crew_launcher()].
    seconds_timeout = NULL,
    #' @field seconds_idle See [crew_launcher()].
    seconds_idle = NULL,
    #' @field seconds_wall See [crew_launcher()].
    seconds_wall = NULL,
    #' @field seconds_exit See [crew_launcher()].
    seconds_exit = NULL,
    #' @field tasks_max See [crew_launcher()].
    tasks_max = NULL,
    #' @field tasks_timers See [crew_launcher()].
    tasks_timers = NULL,
    #' @field async_dial See [crew_launcher()].
    async_dial = NULL,
    #' @field cleanup See [crew_launcher()].
    cleanup = NULL,
    #' @description Launcher constructor.
    #' @return An `R6` object with the launcher.
    #' @param name See [crew_launcher()].
    #' @param seconds_launch See [crew_launcher()].
    #' @param seconds_interval See [crew_launcher()].
    #' @param seconds_timeout See [crew_launcher()].
    #' @param seconds_idle See [crew_launcher()].
    #'   See the `idletime` argument of `mirai::server()`.
    #' @param seconds_wall See [crew_launcher()].
    #' @param seconds_exit See [crew_launcher()].
    #' @param tasks_max See [crew_launcher()].
    #' @param tasks_timers See [crew_launcher()].
    #' @param async_dial See [crew_launcher()].
    #' @param cleanup See [crew_launcher()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' crew_session_start()
    #' router <- crew_router()
    #' router$listen()
    #' launcher <- crew_launcher_callr()
    #' launcher$populate(sockets = router$sockets)
    #' launcher$launch()
    #' m <- mirai::mirai("result")
    #' Sys.sleep(0.25)
    #' m$data
    #' router$terminate()
    #' crew_session_terminate()
    #' }
    initialize = function(
      name = NULL,
      seconds_launch = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      seconds_idle = NULL,
      seconds_wall = NULL,
      seconds_exit = NULL,
      tasks_max = NULL,
      tasks_timers = NULL,
      async_dial = NULL,
      cleanup = NULL
    ) {
      self$name <- name
      self$seconds_launch <- seconds_launch
      self$seconds_interval <- seconds_interval
      self$seconds_timeout <- seconds_timeout
      self$seconds_idle <- seconds_idle
      self$seconds_wall <- seconds_wall
      self$seconds_exit <- seconds_exit
      self$tasks_max <- tasks_max
      self$tasks_timers <- tasks_timers
      self$async_dial <- async_dial
      self$cleanup <- cleanup
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(self$name, is.character(.), length(.) == 1L, !anyNA(.), nzchar(.))
      fields <- c(
        "seconds_launch",
        "seconds_interval",
        "seconds_timeout",
        "seconds_idle",
        "seconds_wall",
        "seconds_exit",
        "tasks_max",
        "tasks_timers"
      )
      for (field in fields) {
        true(self[[field]], is.numeric(.), . >= 0, length(.) == 1L, !anyNA(.))
      }
      for (field in c("async_dial", "cleanup")) {
        true(self[[field]], isTRUE(.) || isFALSE(.))
      }
      true(self$workers, is.data.frame(.))
      invisible()
    },
    #' @description List of arguments for `mirai::server()`.
    #' @return List of arguments for `mirai::server()`.
    #' @param socket Character of length 1, websocket address of the worker
    #'   to launch.
    settings = function(socket) {
      list(
        url = socket,
        asyncdial = self$async_dial,
        maxtasks = self$tasks_max,
        idletime = self$seconds_idle * 1000,
        walltime = self$seconds_wall * 1000,
        timerstart = self$tasks_timers,
        exitlinger = self$seconds_exit * 1000,
        cleanup = self$cleanup
      )
    },
    #' @description Populate the workers data frame.
    #' @return `NULL` (invisibly).
    #' @param sockets Character vector of worker websockets.
    populate = function(sockets) {
      self$workers <- tibble::tibble(
        socket = as.character(sockets),
        launches = rep(0L, length(sockets)),
        start = rep(NA_real_, length(sockets)),
        token = rep(NA_character_, length(sockets)),
        listener = replicate(length(sockets), crew_null, simplify = FALSE),
        handle = replicate(length(sockets), crew_null, simplify = FALSE)
      )
      invisible()
    },
    #' @description Get the active workers.
    #' @details An active worker is a worker that should be given the chance
    #'   to run tasks. To determine if the worker is active,
    #'   `crew` monitors seconds past launch time, and it listens
    #'   to a special non-`mirai` NNG websocket that the worker
    #'   is supposed to dial into on launch.
    #'   If the worker is currently connected to the websocket,
    #'   then it is active. Otherwise, if the worker is not connected
    #'   and the startup window has expired, the worker is inactive.
    #'   Otherwise, if the worker is not connected and the startup
    #'   window has not yet expired, then the worker is active
    #'   if it has not ever connected to a websocket.
    #' @return Character vector of worker websockets.
    active = function() {
      listeners <- self$workers$listener
      bound <- self$seconds_launch
      start <- self$workers$start
      now <- bench::hires_time()
      listening <- map_lgl(listeners, connection_opened)
      connected <- map_lgl(listeners, dialer_connected)
      launching <- !is.na(start) & ((now - start) < bound)
      discovered <- connected
      index <- listening & (!connected) & launching
      discovered[index] <- map_lgl(listeners[index], dialer_discovered)
      active <- listening & (connected | (launching & (!discovered)))
      self$workers$socket[active]
    },
    #' @description Launch one or more workers.
    #' @details If a worker is already assigned to a socket,
    #'   the previous worker is terminated before the next
    #'   one is launched.
    #' @return `NULL` (invisibly).
    #' @param sockets Sockets where the workers will dial in.
    launch = function(sockets = character(0)) {
      true(
        !is.null(crew_session_port()),
        message = "call crew_session_start() before launching workers."
      )
      matches <- match(x = sockets, table = self$workers$socket)
      true(!anyNA(matches), message = "bad websocket on launch.")
      for (index in matches) {
        handle <- self$workers$handle[[index]]
        if (!is_crew_null(handle)) {
          self$terminate_worker(handle)
        }
        socket <- self$workers$socket[index]
        self$workers$start[index] <- bench::hires_time()
        token <- random_name()
        self$workers$token[index] <- token
        listener <- connection_listen(
          host = crew_session_host(),
          port = crew_session_port(),
          token = token
        )
        handle <- self$launch_worker(
          socket = socket,
          host = crew_session_host(),
          port = crew_session_port(),
          token = token,
          name = self$name
        )
        self$workers$listener[[index]] <- listener
        self$workers$handle[[index]] <- handle
        self$workers$launches[[index]] <- self$workers$launches[[index]] + 1L
      }
      invisible()
    },
    #' @description Terminate one or more workers.
    #' @return `NULL` (invisibly).
    #' @param sockets Character vector of sockets of the workers
    #'   to terminate.
    terminate = function(sockets = NULL) {
      sockets <- sockets %|||% self$workers$socket
      matches <- match(x = sockets, table = self$workers$socket)
      true(!anyNA(matches), message = "bad websocket on terminate.")
      for (index in matches) {
        handle <- self$workers$handle[[index]]
        if (!is_crew_null(handle)) {
          self$terminate_worker(handle)
        }
        con <- self$workers$listener[[index]]
        if (!is_crew_null(con) && connection_opened(con)) {
          close(con)
        }
        self$workers$start[index] <- NA_real_
        self$workers$token[index] <- NA_character_
        self$workers$listener[[index]] <- crew_null
        self$workers$handle[[index]] <- crew_null
      }
      invisible()
    }
  )
)
