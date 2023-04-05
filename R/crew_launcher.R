#' @title Create an abstract launcher.
#' @export
#' @keywords internal
#' @family launchers
#' @description This function is useful for inheriting argument documentation
#'   in functions that create custom third-party launchers. See
#'   `@inheritParams crew::crew_launcher` in the source code file of
#'   [crew_launcher_local()].
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
#' @param cleanup Logical, whether to clean up global options and the
#'   global environment after every task.
#'   See the `cleanup` argument of `mirai::server()`.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_session_start()
#' router <- crew_router()
#' router$listen()
#' launcher <- crew_launcher_local()
#' launcher$populate(sockets = router$sockets)
#' launcher$launch()
#' m <- mirai::mirai("result", .compute = router$name)
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
  cleanup = FALSE
) {
  name <- as.character(name %|||% random_name())
  launcher <- crew_class_launcher_local$new(
    name = name,
    seconds_launch = seconds_launch,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    seconds_exit = seconds_exit,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
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
#' launcher <- crew_launcher_local()
#' launcher$populate(sockets = router$sockets)
#' launcher$launch()
#' m <- mirai::mirai("result", .compute = router$name)
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
    workers = NULL,
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
    #' @param cleanup See [crew_launcher()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' crew_session_start()
    #' router <- crew_router()
    #' router$listen()
    #' launcher <- crew_launcher_local()
    #' launcher$populate(sockets = router$sockets)
    #' launcher$launch()
    #' m <- mirai::mirai("result", .compute = router$name)
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
      true(self$cleanup, isTRUE(.) || isFALSE(.))
      if (!is.null(self$workers)) {
        true(self$workers, is.data.frame(.))
        cols <- c("handle", "socket", "start", "launches")
        true(identical(colnames(self$workers), cols))
      }
      invisible()
    },
    #' @description List of arguments for `mirai::server()`.
    #' @return List of arguments for `mirai::server()`.
    #' @param socket Character of length 1, websocket address of the worker
    #'   to launch.
    settings = function(socket) {
      list(
        url = socket,
        maxtasks = self$tasks_max,
        idletime = self$seconds_idle * 1000,
        walltime = self$seconds_wall * 1000,
        timerstart = self$tasks_timers,
        exitlinger = self$seconds_exit * 1000,
        cleanup = self$cleanup,
        asyncdial = FALSE
      )
    },
    #' @description Create a call to [crew_worker()] to
    #'   help create custom launchers.
    #' @return Character of length 1 with a call to [crew_worker()].
    #' @param socket Socket where the worker will receive tasks.
    #' @param launcher Character of length 1, name of the launcher.
    #' @param worker Positive integer of length 1, index of the worker.
    #'   This worker index remains the same even when the current instance
    #'   of the worker exits and a new instance launches.
    #' @param instance Character of length 1 to uniquely identify
    #'   the instance of the worker.
    #' @examples
    #' launcher <- crew_launcher_local()
    #' launcher$call(
    #'   socket = "ws://127.0.0.1:5000/3/cba033e58",
    #'   launcher = "launcher_a",
    #'   worker = 3L,
    #'   instance = "cba033e58"
    #' )
    call = function(socket, launcher, worker, instance) {
      call <- substitute(
        crew::crew_worker(
          settings = settings,
          launcher = launcher,
          worker = worker,
          instance = instance
        ),
        env = list(
          settings = self$settings(socket),
          launcher = launcher,
          worker = worker,
          instance = instance
        )
      )
      out <- deparse_safe(expr = call, collapse = " ")
      gsub(pattern = "   *", replacement = " ", x = out)
    },
    #' @description Start the launcher.
    #' @details Creates the workers data frame.
    #'   Meant to be called once at the beginning of the launcher
    #'   life cycle.
    #' @return `NULL` (invisibly).
    #' @param workers Positive integer of length 1,
    #'   number of workers to allow.
    start = function(workers = 1L) {
      if (is.null(self$workers)) {
        self$workers <- tibble::tibble(
          handle = replicate(workers, crew_null, simplify = FALSE),
          socket = rep(NA_character_, workers),
          start = rep(NA_real_, workers),
          launches = rep(0L, workers)
        )
      }
      invisible()
    },
    #' @description Launch one or more workers.
    #' @details If a worker is already assigned to a socket,
    #'   the previous worker is terminated before the next
    #'   one is launched.
    #' @return `NULL` (invisibly).
    #' @param sockets Character vector of sockets for the workers
    #'   to launch.
    launch = function(sockets = character(0L)) {
      for (socket in sockets) {
        path <- parse_socket(socket)
        index <- path$index
        instance <- path$instance
        call <- self$call(
          socket = socket,
          launcher = self$name,
          worker = index,
          instance = instance
        )
        handle <- self$launch_worker(
          call = call,
          launcher = self$name,
          worker = index,
          instance = instance
        )
        self$workers$handle[[index]] <- handle
        self$workers$socket[index] <- socket
        self$workers$start[index] <- nanonext::mclock() / 1000
        self$workers$launches[[index]] <- self$workers$launches[[index]] + 1L
      }
      invisible()
    },
    #' @description Show whether each worker is launching.
    #' @details A worker is considered "launching" if it was started
    #'   recently (`seconds_launch` seconds ago or sooner).
    #' @return A logical vector indicating which workers are launching.
    launching = function() {
      bound <- self$seconds_launch
      start <- self$workers$start
      now <- nanonext::mclock() / 1000
      !is.na(start) & ((now - start) < bound)
    },
    #' @description Terminate one or more workers.
    #' @return `NULL` (invisibly).
    #' @param indexes Integer vector of the indexes of the workers
    #'   to terminate. If `NULL`, all current workers are terminated.
    terminate = function(indexes = NULL) {
      if (is.null(self$workers)) {
        return(invisible())
      }
      indexes <- indexes %|||% seq_len(nrow(self$workers))
      for (index in indexes) {
        handle <- self$workers$handle[[index]]
        if (!is_crew_null(handle)) {
          self$terminate_worker(handle)
        }
        self$workers$handle[[index]] <- crew_null
        self$workers$socket[index] <- NA_character_
        self$workers$start[index] <- NA_real_
      }
      invisible()
    },
    #' @description Abstract method.
    #' @details Does not actually terminate a worker. This method is a
    #'   placeholder, and its presence allows manual worker termination
    #'   to be optional.
    #' @return `NULL` (invisibly).
    #' @param handle A handle object previously
    #'   returned by `launch_worker()` which allows the termination
    #'   of the worker.
    terminate_worker = function(handle) {
      invisible()
    }
  )
)
