#' @title Create an abstract launcher.
#' @export
#' @family developer
#' @description This function is useful for inheriting argument documentation
#'   in functions that create custom third-party launchers. See
#'   `@inheritParams crew::crew_launcher` in the source code file of
#'   [crew_launcher_local()].
#' @inheritParams crew_client
#' @param name Name of the launcher.
#' @param seconds_interval Seconds to wait between asynchronous operations.
#' @param seconds_launch Seconds of startup time to allow.
#'   A worker is unconditionally assumed to be alive
#'   from the moment of its launch until `seconds_launch` seconds later.
#'   After `seconds_launch` seconds, the worker is only
#'   considered alive if it is actively connected to its assign websocket.
#' @param seconds_idle Maximum number of seconds that a worker can idle
#'   since the completion of the last task. If exceeded, the worker exits.
#'   But the timer does not launch until `tasks_timers` tasks
#'   have completed.
#'   See the `idletime` argument of `mirai::daemon()`. `crew` does not
#'   excel with perfectly transient workers because it does not micromanage
#'   the assignment of tasks to workers, so please allow enough idle
#'   time for a new worker to be delegated a new task.
#' @param seconds_wall Soft wall time in seconds.
#'   The timer does not launch until `tasks_timers` tasks
#'   have completed.
#'   See the `walltime` argument of `mirai::daemon()`.
#' @param seconds_exit Number of seconds to wait for NNG websockets
#'   to finish sending large data (when a worker exits after reaching a
#'   timeout or having completed a certain number of tasks).
#'   See the `exitlinger` argument of `mirai::daemon()`.
#' @param tasks_max Maximum number of tasks that a worker will do before
#'   exiting. See the `maxtasks` argument of `mirai::daemon()`.
#'   `crew` does not
#'   excel with perfectly transient workers because it does not micromanage
#'   the assignment of tasks to workers, it is recommended to set
#'   `tasks_max` to a value greater than 1.
#' @param tasks_timers Number of tasks to do before activating
#'   the timers for `seconds_idle` and `seconds_wall`.
#'   See the `timerstart` argument of `mirai::daemon()`.
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
#' @param launch_max Positive integer of length 1, maximum allowed
#'   consecutive launch attempts which do not complete any tasks.
#'   Enforced on a worker-by-worker basis.
#'   The futile launch count resets to back 0
#'   for each worker that completes a task.
#'   It is recommended to set `launch_max` above 0
#'   because sometimes workers are unproductive under perfectly ordinary
#'   circumstances. But `launch_max` should still be small enough
#'   to detect errors in the underlying platform.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' client$start()
#' launcher <- crew_launcher_local(name = client$name)
#' launcher$start(workers = client$workers)
#' launcher$launch(index = 1L)
#' m <- mirai::mirai("result", .compute = client$name)
#' Sys.sleep(0.25)
#' m$data
#' client$terminate()
#' }
crew_launcher <- function(
  name = NULL,
  seconds_interval = 0.25,
  seconds_launch = 30,
  seconds_idle = Inf,
  seconds_wall = Inf,
  seconds_exit = 1,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  launch_max = 5L,
  tls = crew::crew_tls()
) {
  name <- as.character(name %|||% crew_random_name())
  crew_assert(
    inherits(tls, "crew_class_tls"),
    message = "argument tls must be an object created by crew_tls()"
  )
  crew_class_launcher$new(
    name = name,
    seconds_interval = seconds_interval,
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    seconds_exit = seconds_exit,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection,
    launch_max = launch_max,
    tls = tls
  )
}

#' @title Launcher abstract class
#' @export
#' @family class
#' @description `R6` abstract class to build other subclasses
#'   which launch and manage workers.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' client$start()
#' launcher <- crew_launcher_local(name = client$name)
#' launcher$start(workers = client$workers)
#' launcher$launch(index = 1L)
#' m <- mirai::mirai("result", .compute = client$name)
#' Sys.sleep(0.25)
#' m$data
#' client$terminate()
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
    #' @field seconds_interval See [crew_launcher()].
    seconds_interval = NULL,
    #' @field seconds_launch See [crew_launcher()].
    seconds_launch = NULL,
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
    #' @field launch_max See [crew_launcher()].
    launch_max = NULL,
    #' @field tls See [crew_launcher()].
    tls = NULL,
    #' @field until Numeric of length 1, time point when throttled unlocks.
    until = NULL,
    #' @description Launcher constructor.
    #' @return An `R6` object with the launcher.
    #' @param name See [crew_launcher()].
    #' @param seconds_interval See [crew_launcher()].
    #' @param seconds_launch See [crew_launcher()].
    #' @param seconds_idle See [crew_launcher()].
    #' @param seconds_wall See [crew_launcher()].
    #' @param seconds_exit See [crew_launcher()].
    #' @param tasks_max See [crew_launcher()].
    #' @param tasks_timers See [crew_launcher()].
    #' @param reset_globals See [crew_launcher()].
    #' @param reset_packages See [crew_launcher()].
    #' @param reset_options See [crew_launcher()].
    #' @param garbage_collection See [crew_launcher()].
    #' @param launch_max See [crew_launcher()].
    #' @param tls See [crew_launcher()]
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' client <- crew_client()
    #' client$start()
    #' launcher <- crew_launcher_local(name = client$name)
    #' launcher$start(workers = client$workers)
    #' launcher$launch(index = 1L)
    #' m <- mirai::mirai("result", .compute = client$name)
    #' Sys.sleep(0.25)
    #' m$data
    #' client$terminate()
    #' }
    initialize = function(
      name = NULL,
      seconds_interval = NULL,
      seconds_launch = NULL,
      seconds_idle = NULL,
      seconds_wall = NULL,
      seconds_exit = NULL,
      tasks_max = NULL,
      tasks_timers = NULL,
      reset_globals = NULL,
      reset_packages = NULL,
      reset_options = NULL,
      garbage_collection = NULL,
      launch_max = NULL,
      tls = NULL
    ) {
      self$name <- name
      self$seconds_interval <- seconds_interval
      self$seconds_launch <- seconds_launch
      self$seconds_idle <- seconds_idle
      self$seconds_wall <- seconds_wall
      self$seconds_exit <- seconds_exit
      self$tasks_max <- tasks_max
      self$tasks_timers <- tasks_timers
      self$reset_globals <- reset_globals
      self$reset_packages <- reset_packages
      self$reset_options <- reset_options
      self$garbage_collection <- garbage_collection
      self$launch_max <- launch_max
      self$tls <- tls
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(
        is.function(self$launch_worker),
        message = "launch_worker() must be a function."
      )
      fields <- c("call", "name", "launcher", "worker", "instance")
      crew_assert(
        fields %in% names(formals(self$launch_worker)),
        message = paste(
          "launch_worker() must have the following arguments:",
          paste(sprintf("\"%s\"", fields), collapse = ", ")
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
        "seconds_interval",
        "seconds_launch",
        "seconds_idle",
        "seconds_wall",
        "seconds_exit",
        "tasks_max",
        "tasks_timers"
        # TODO: add launch_max
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
        cols <- c(
          "handle",
          "socket",
          "start",
          "launches",
          "futile",
          "launched",
          "history",
          "assigned",
          "complete"
        )
        crew_assert(identical(colnames(self$workers), cols))
        crew_assert(nrow(self$workers) > 0L)
      }
      # TODO: forbid NULL tls objects:
      if (!is.null(self$tls)) {
        crew_assert(
          inherits(self$tls, "crew_class_tls"),
          message = "field tls must be an object created by crew_tls()"
        )
      }
      invisible()
    },
    #' @description List of arguments for `mirai::daemon()`.
    #' @return List of arguments for `mirai::daemon()`.
    #' @param socket Character of length 1, websocket address of the worker
    #'   to launch.
    settings = function(socket) {
      cleanup <- as.integer(isTRUE(self$reset_globals)) +
        (2L * as.integer(isTRUE(self$reset_packages))) +
        (4L * as.integer(isTRUE(self$reset_options))) +
        (8L * as.integer(isTRUE(self$garbage_collection)))
      list(
        url = socket,
        asyncdial = FALSE,
        maxtasks = self$tasks_max,
        idletime = self$seconds_idle * 1000,
        walltime = self$seconds_wall * 1000,
        timerstart = self$tasks_timers,
        exitlinger = self$seconds_exit * 1000,
        cleanup = cleanup,
        # TODO: always use tls objects:
        tls = if_any(
          is.null(self$tls),
          NULL,
          self$tls$worker(name = self$name)
        ),
        rs = mirai::nextstream(self$name)
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
    #'   life cycle, after the client has started.
    #' @return `NULL` (invisibly).
    #' @param sockets For testing purposes only.
    start = function(sockets = NULL) {
      sockets <- sockets %|||% mirai::nextget("urls", .compute = self$name)
      n <- length(sockets)
      self$workers <- tibble::tibble(
        handle = replicate(n, crew_null, simplify = FALSE),
        socket = sockets,
        start = rep(NA_real_, n),
        launches = rep(0L, n),
        futile = rep(0L, n),
        launched = rep(FALSE, n),
        history = rep(0L, n),
        assigned = rep(0L, n),
        complete = rep(0L, n)
      )
      invisible()
    },
    #' @description Summarize the workers.
    #' @return `NULL` if the launcher is not started. Otherwise, a `tibble`
    #'   with one row per `crew` worker and the following columns:
    #'   * `worker`: integer index of the worker.
    #'   * `launches`: number of times the worker was launched. Each launch
    #'     occurs at a different websocket because the token at the end of the
    #'     URL is rotated before each new launch.
    #'   * `assigned`: cumulative number of tasks assigned, reported by
    #'     `mirai::daemons()` and summed over all
    #'     completed instances of the worker. Does not reflect the activity
    #'     of the currently running instance of the worker.
    #'   * `complete`: cumulative number of tasks completed, reported by
    #'     `mirai::daemons()` and summed over all
    #'     completed instances of the worker. Does not reflect the activity
    #'     of the currently running instance of the worker.
    #'   * `socket`: current websocket URL of the worker.
    summary = function() {
      workers <- .subset2(self, "workers")
      if (is.null(workers)) {
        return(NULL)
      }
      tibble::tibble(
        worker = seq_len(nrow(workers)),
        launches = .subset2(workers, "launches"),
        assigned = .subset2(workers, "assigned"),
        complete = .subset2(workers, "complete")
      )
    },
    #' @description Get done workers.
    #' @details A worker is "done" if it is launched and inactive.
    #'   A worker is "launched" if `launch()` was called
    #'   and the worker websocket has not been rotated since.
    #'   If a worker is currently online, then it is not inactive.
    #'   If a worker is not currently online, then it is inactive
    #'   if and only if (1) either it connected to the current
    #'   websocket at some point in the past,
    #'   or (2) `seconds_launch` seconds elapsed since launch.
    #' @return Integer index of inactive workers.
    #' @param daemons `mirai` daemons matrix. For testing only. Users
    #'   should not set this.
    done = function(daemons = NULL) {
      bound <- self$seconds_launch
      start <- self$workers$start
      now <- nanonext::mclock() / 1000
      launching <- !is.na(start) & ((now - start) < bound)
      daemons <- daemons %|||% daemons_info(name = self$name)
      online <- as.logical(daemons[, "online"])
      discovered <- as.logical(daemons[, "instance"] > 0L)
      inactive <- (!online) & (discovered | (!launching))
      launched <- self$workers$launched
      which(inactive & launched)
    },
    #' @details Rotate a websocket.
    #' @return `NULL` (invisibly).
    #' @param index Integer index of a worker.
    rotate = function(index) {
      socket <- mirai::saisei(i = index, force = FALSE, .compute = self$name)
      if (!is.null(socket)) {
        handle <- self$workers$handle[[index]]
        if (!is_crew_null(handle)) {
          self$terminate_worker(handle)
        }
        self$workers$socket[index] <- socket
        self$workers$launched[index] <- FALSE
      }
    },
    #' @description Update the cumulative assigned and complete statistics.
    #' @description Used to detect backlogged workers with more assigned
    #'   than complete tasks. If terminated, these workers need to be
    #'   relaunched until the backlog of assigned tasks is complete.
    #' @return `NULL` (invisibly).
    #' @param daemons `mirai` daemons matrix. For testing only. Users
    #'   should not set this.
    tally = function(daemons = NULL) {
      daemons <- daemons %|||% daemons_info(name = self$name)
      index <- !(self$workers$launched)
      self$workers$assigned[index] <- as.integer(daemons[index, "assigned"])
      self$workers$complete[index] <- as.integer(daemons[index, "complete"])
      invisible()
    },
    #' @description Get workers available for launch.
    #' @return Integer index of workers available for launch.
    #' @param n Maximum number of worker indexes to return.
    unlaunched  = function(n = Inf) {
      head(x = which(!self$workers$launched), n = n)
    },
    #' @description List non-launched backlogged workers.
    #' @return Integer vector of worker indexes.
    backlogged = function() {
      workers <- self$workers
      index <- !(workers$launched) & (workers$assigned > workers$complete)
      which(index)
    },
    #' @description List non-launched non-backlogged workers.
    #' @return Integer vector of worker indexes.
    #' @param n Maximum number of worker indexes to return.
    resolved = function() {
      workers <- self$workers
      index <- !(workers$launched) & !(workers$assigned > workers$complete)
      which(index)
    },
    #' @description Launch a worker.
    #' @return `NULL` (invisibly).
    #' @param index Positive integer of length 1, index of the worker
    #'   to launch.
    launch = function(index) {
      socket <- self$workers$socket[index]
      instance <- parse_instance(socket)
      call <- self$call(
        socket = socket,
        launcher = self$name,
        worker = index,
        instance = instance
      )
      name <- name_worker(
        launcher = self$name,
        worker = index,
        instance = instance
      )
      history <- self$workers$history[index]
      complete <- self$workers$complete[index]
      futile <- self$workers$futile[index]
      futile <- if_any(complete > history, 0L, futile + 1L)
      crew_assert(
        futile <= self$launch_max,
        message = paste(
          "{crew} worker",
          index,
          "launched",
          self$launch_max,
          "times in a row without completing any tasks. Either raise",
          "launch_max or troubleshoot your platform to figure out",
          "why {crew} workers are not launching or connecting."
        )
      )
      handle <- self$launch_worker(
        call = as.character(call),
        name = as.character(name),
        launcher = as.character(self$name),
        worker = as.integer(index),
        instance = as.character(instance)
      )
      self$workers$handle[[index]] <- handle
      self$workers$socket[index] <- socket
      self$workers$start[index] <- nanonext::mclock() / 1000
      self$workers$launches[index] <- self$workers$launches[index] + 1L
      self$workers$futile[index] <- futile
      self$workers$launched[index] <- TRUE
      self$workers$history[index] <- complete
      invisible()
    },
    #' @description Throttle repeated calls.
    #' @return `TRUE` to throttle, `FALSE` to continue.
    throttle = function() {
      now <- nanonext::mclock()
      if (is.null(self$until)) {
        self$until <- now + (1000 * self$seconds_interval)
      }
      if (now < self$until) {
        return(TRUE)
      } else {
        self$until <- NULL
        return(FALSE)
      }
    },
    #' @description Auto-scale workers out to meet the demand of tasks.
    #' @return `NULL` (invisibly)
    #' @param demand Number of unresolved tasks.
    #' @param throttle Logical of length 1, whether to delay auto-scaling
    #'   until the next auto-scaling request at least
    #'  `self$client$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    scale = function(demand, throttle = FALSE) {
      if (throttle && self$throttle()) {
        return(invisible())
      }
      walk(x = self$done(), f = self$rotate)
      self$tally()
      walk(x = self$backlogged(), f = self$launch)
      resolved <- self$resolved()
      active <- nrow(self$workers) - length(resolved)
      deficit <- min(length(resolved), max(0L, demand - active))
      walk(x = head(x = resolved, n = deficit), f = self$launch)
      invisible()
    },
    #' @description Terminate one or more workers.
    #' @return `NULL` (invisibly).
    #' @param index Integer vector of the indexes of the workers
    #'   to terminate. If `NULL`, all current workers are terminated.
    terminate = function(index = NULL) {
      if (is.null(self$workers)) {
        return(invisible())
      }
      index <- index %|||% seq_len(nrow(self$workers))
      for (worker in index) {
        handle <- self$workers$handle[[worker]]
        if (!is_crew_null(handle)) {
          self$terminate_worker(handle)
        }
        self$workers$handle[[worker]] <- crew_null
        self$workers$socket[worker] <- NA_character_
        self$workers$start[worker] <- NA_real_
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
