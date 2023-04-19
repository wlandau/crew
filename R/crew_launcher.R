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
#' @param reset_globals `TRUE` to reset global environment
#'   variables between tasks, `FALSE` to leave them alone.
#' @param reset_packages `TRUE` to unload any packages loaded during
#'   a task (runs between each task), `FALSE` to leave packages alone.
#' @param reset_options `TRUE` to reset global options to their original
#'   state between each task, `FALSE` otherwise. It is recommended to
#'   only set `reset_options = TRUE` if `reset_packages` is also `TRUE`
#'   because packages sometimes rely on options they set at loading time.
#' @param garbage_collection `TRUE` to run garbage collection between
#'   tasks, `FALSE` to skip.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_session_start()
#' router <- crew_router()
#' router$start()
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
  seconds_interval = 0.01,
  seconds_timeout = 10,
  seconds_idle = Inf,
  seconds_wall = Inf,
  seconds_exit = 0.1,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE
) {
  name <- as.character(name %|||% crew_random_name())
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
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection
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
#' router$start()
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
    #' @field reset_globals See [crew_launcher()].
    reset_globals = NULL,
    #' @field reset_packages See [crew_launcher()].
    reset_packages = NULL,
    #' @field reset_options See [crew_launcher()].
    reset_options = NULL,
    #' @field garbage_collection See [crew_launcher()].
    garbage_collection = NULL,
    #' @description Launcher constructor.
    #' @return An `R6` object with the launcher.
    #' @param name See [crew_launcher()].
    #' @param seconds_launch See [crew_launcher()].
    #' @param seconds_interval See [crew_launcher()].
    #' @param seconds_timeout See [crew_launcher()].
    #' @param seconds_idle See [crew_launcher()].
    #' @param seconds_wall See [crew_launcher()].
    #' @param seconds_exit See [crew_launcher()].
    #' @param tasks_max See [crew_launcher()].
    #' @param tasks_timers See [crew_launcher()].
    #' @param reset_globals See [crew_launcher()].
    #' @param reset_packages See [crew_launcher()].
    #' @param reset_options See [crew_launcher()].
    #' @param garbage_collection See [crew_launcher()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' crew_session_start()
    #' router <- crew_router()
    #' router$start()
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
      reset_globals = NULL,
      reset_packages = NULL,
      reset_options = NULL,
      garbage_collection = NULL
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
      self$reset_globals <- reset_globals
      self$reset_packages <- reset_packages
      self$reset_options <- reset_options
      self$garbage_collection <- garbage_collection
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(
        is.function(self$launch_worker),
        message = "launch_worker() must be a function."
      )
      names <- c("call", "launcher", "worker", "instance")
      crew_assert(
        names %in% names(formals(self$launch_worker)),
        message = paste(
          "launch_worker() must have arguments \"call\", \"launcher\",",
          "\"worker\", and \"instance\"."
        )
      )
      if (!is.null(self$terminate_worker)) {
        crew_assert(
          is.function(self$terminate_worker),
          message = "terminate_worker() must be a function or NULL."
        )
        crew_assert(
          "handle" %in% names(formals(self$terminate_worker)),
          message = "terminate_worker() must have a \"handle\" argument."
        )
      }
      crew_assert(
        self$name,
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.)
      )
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
        crew_assert(
          self[[field]],
          is.numeric(.),
          . >= 0,
          length(.) == 1L,
          !anyNA(.)
        )
      }
      fields <- c(
        "reset_globals",
        "reset_packages",
        "reset_options",
        "garbage_collection"
      )
      for (field in fields) {
        crew_assert(self[[field]], isTRUE(.) || isFALSE(.))
      }
      if (!is.null(self$workers)) {
        crew_assert(self$workers, is.data.frame(.))
        cols <- c("handle", "socket", "start", "launches")
        crew_assert(identical(colnames(self$workers), cols))
        crew_assert(nrow(self$workers) > 0L)
      }
      invisible()
    },
    #' @description List of arguments for `mirai::server()`.
    #' @return List of arguments for `mirai::server()`.
    #' @param socket Character of length 1, websocket address of the worker
    #'   to launch.
    settings = function(socket) {
      cleanup <- as.integer(isTRUE(self$reset_globals)) +
        (2L * as.integer(isTRUE(self$reset_packages))) +
        (4L * as.integer(isTRUE(self$reset_options))) +
        (8L * as.integer(isTRUE(self$garbage_collection)))
      list(
        url = socket,
        maxtasks = self$tasks_max,
        idletime = self$seconds_idle * 1000,
        walltime = self$seconds_wall * 1000,
        timerstart = self$tasks_timers,
        exitlinger = self$seconds_exit * 1000,
        cleanup = cleanup,
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
      crew_assert(workers > 0L, message = "workers must be > 0")
      self$workers <- tibble::tibble(
        handle = replicate(workers, crew_null, simplify = FALSE),
        socket = rep(NA_character_, workers),
        start = rep(NA_real_, workers),
        launches = rep(0L, workers)
      )
      invisible()
    },
    #' @description Launch a worker.
    #' @return `NULL` (invisibly).
    #' @param index Positive integer of length 1, index of the worker
    #'   to launch.
    #' @param socket Character of length 1, sockets of the worker to launch.
    launch = function(index, socket = NULL) {
      if (!length(socket) || !is.character(socket)) {
        return(invisible())
      }
      instance <- parse_instance(socket)
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
      invisible()
    },
    #' @description Show which workers are expected to launch.
    #' @return A logical vector indicating which workers are expected
    #'   to launch.
    expected = function() {
      !is.na(self$workers$socket)
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
