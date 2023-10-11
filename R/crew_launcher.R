#' @title Create an abstract launcher.
#' @export
#' @family developer
#' @description This function is useful for inheriting argument documentation
#'   in functions that create custom third-party launchers. See
#'   `@inheritParams crew::crew_launcher` in the source code file of
#'   [crew_launcher_local()].
#' @inheritParams crew_client
#' @param name Name of the launcher.
#' @param seconds_interval Deprecated in version 0.5.0.9003 (2023-10-02)
#'   no longer used.
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
#' @param seconds_exit Deprecated on 2023-09-21 in version 0.5.0.9002.
#'   No longer necessary.
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
#' @param processes `NULL` or positive integer of length 1,
#'   number of local processes to
#'   launch to allow worker launches to happen asynchronously. If `NULL`,
#'   then no local processes are launched. If 1 or greater, then the launcher
#'   starts the processes on `start()` and ends them on `terminate()`.
#'   Plugins that may use these processes should run asynchronous calls
#'   using `launcher$async$eval()` and expect a `mirai` task object
#'   as the return value.
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
  seconds_interval = NULL,
  seconds_launch = 30,
  seconds_idle = Inf,
  seconds_wall = Inf,
  seconds_exit = NULL,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  launch_max = 5L,
  tls = crew::crew_tls(),
  processes = NULL
) {
  crew_deprecate(
    name = "seconds_exit",
    date = "2023-09-21",
    version = "0.5.0.9002",
    alternative = "none (no longer necessary)",
    condition = "message",
    value = seconds_exit,
    frequency = "once"
  )
  crew_deprecate(
    name = "seconds_interval",
    date = "2023-10-02",
    version = "0.5.0.9003",
    alternative = "none (no longer necessary)",
    condition = "message",
    value = seconds_interval,
    frequency = "once"
  )
  name <- as.character(name %|||% crew_random_name())
  crew_assert(
    inherits(tls, "crew_class_tls"),
    message = "argument tls must be an object created by crew_tls()"
  )
  crew_class_launcher$new(
    name = name,
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection,
    launch_max = launch_max,
    tls = tls,
    processes = processes
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
    #' @field seconds_launch See [crew_launcher()].
    seconds_launch = NULL,
    #' @field seconds_idle See [crew_launcher()].
    seconds_idle = NULL,
    #' @field seconds_wall See [crew_launcher()].
    seconds_wall = NULL,
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
    #' @field processes See [crew_launcher()].
    processes = NULL,
    #' @field async A [crew_async()] object to run low-level launcher tasks
    #'   asynchronously.
    async = NULL,
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
    #' @param tls See [crew_launcher()].
    #' @param processes See [crew_launcher()].
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
      tls = NULL,
      processes = NULL
    ) {
      self$name <- name
      self$seconds_launch <- seconds_launch
      self$seconds_idle <- seconds_idle
      self$seconds_wall <- seconds_wall
      self$tasks_max <- tasks_max
      self$tasks_timers <- tasks_timers
      self$reset_globals <- reset_globals
      self$reset_packages <- reset_packages
      self$reset_options <- reset_options
      self$garbage_collection <- garbage_collection
      self$launch_max <- launch_max
      self$tls <- tls
      self$processes <- processes
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
        "seconds_launch",
        "seconds_idle",
        "seconds_wall",
        "tasks_max",
        "tasks_timers",
        "launch_max"
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
      crew_assert(
        self$processes %|||% 1L,
        is.numeric(.),
        . > 0L,
        length(.) == 1L,
        !anyNA(.)
      )
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
          "termination",
          "socket",
          "start",
          "launches",
          "futile",
          "launched",
          "terminated",
          "history",
          "online",
          "discovered",
          "assigned",
          "complete"
        )
        crew_assert(identical(colnames(self$workers), cols))
        crew_assert(nrow(self$workers) > 0L)
      }
      crew_assert(
        inherits(self$tls, "crew_class_tls"),
        message = "field tls must be an object created by crew_tls()"
      )
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
        cleanup = cleanup,
        tls = self$tls$worker(name = self$name),
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
      self$async <- crew_async(workers = self$processes)
      self$async$start()
      sockets <- sockets %|||% mirai::nextget("urls", .compute = self$name)
      n <- length(sockets)
      self$workers <- tibble::tibble(
        handle = replicate(n, crew_null, simplify = FALSE),
        termination = replicate(n, crew_null, simplify = FALSE),
        socket = sockets,
        start = rep(NA_real_, n),
        launches = rep(0L, n),
        futile = rep(0L, n),
        launched = rep(FALSE, n),
        terminated = rep(TRUE, n),
        history = rep(-1L, n),
        online = rep(FALSE, n),
        discovered = rep(FALSE, n),
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
    #'   * `online`: logical vector, whether the current instance of each
    #'     worker was actively connected to its NNG socket during the time of
    #'     the last call to `tally()`.
    #'   * `discovered`: logical vector, whether the current instance of each
    #'     worker had connected to its NNG socket at some point
    #'     (and then possibly disconnected) during the time of
    #'     the last call to `tally()`.
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
        online = .subset2(workers, "online"),
        discovered = .subset2(workers, "discovered"),
        assigned = .subset2(workers, "assigned"),
        complete = .subset2(workers, "complete")
      )
    },
    #' @description Update the `daemons`-related columns of the internal
    #'   `workers` data frame.
    #' @return `NULL` (invisibly).
    #' @param daemons `mirai` daemons matrix. For testing only. Users
    #'   should not set this.
    tally = function(daemons = NULL) {
      daemons <- daemons %|||% daemons_info(name = self$name)
      self$workers$online <- as.logical(daemons[, "online"])
      self$workers$discovered <- as.logical(daemons[, "instance"] > 0L)
      self$workers$assigned <- as.integer(daemons[, "assigned"])
      self$workers$complete <- as.integer(daemons[, "complete"])
      invisible()
    },
    #' @description Get indexes of unlaunched workers.
    #' @details A worker is "unlaunched" if it has never connected
    #'   to the current instance of its websocket. Once a worker
    #'   launches with the `launch()` method, it is considered "launched"
    #'   until it disconnects and its websocket is rotated with `rotate()`.
    #' @return Integer index of workers available for launch.
    #' @param n Maximum number of worker indexes to return.
    unlaunched = function(n = Inf) {
      head(x = which(!self$workers$launched), n = n)
    },
    #' @description Get workers that may still be booting up.
    #' @details A worker is "booting" if its launch time is within the last
    #'   `seconds_launch` seconds. `seconds_launch` is a configurable grace
    #'   period when `crew` allows a worker to start up and connect to the
    #'   `mirai` dispatcher. The `booting()` function does not know about the
    #'   actual worker connection status, it just knows about launch times,
    #'   so it may return `TRUE` for workers that have already connected
    #'   and started doing tasks.
    booting = function() {
      bound <- self$seconds_launch
      start <- self$workers$start
      now <- nanonext::mclock() / 1000
      launching <- !is.na(start) & ((now - start) < bound)
    },
    #' @description Get active workers.
    #' @details A worker is "active" if its current instance is online and
    #'   connected, or if it is within its booting time window
    #'   and has never connected.
    #'   In other words, "active" means `online | (!discovered & booting)`.
    #' @return Logical vector with `TRUE` for active workers and `FALSE` for
    #'   inactive ones.
    active = function() {
      booting <- self$booting()
      online <- self$workers$online
      discovered <- self$workers$discovered
      online | (!discovered & booting)
    },
    #' @description Get done workers.
    #' @details A worker is "done" if it is launched and inactive.
    #'   A worker is "launched" if `launch()` was called
    #'   and the worker websocket has not been rotated since.
    #' @return Integer index of inactive workers.
    done = function() {
      !self$active() & self$workers$launched
    },
    #' @details Rotate websockets at all unlaunched workers.
    #' @return `NULL` (invisibly).
    rotate = function() {
      which_done <- which(self$done())
      for (index in which_done) {
        socket <- mirai::saisei(i = index, force = FALSE, .compute = self$name)
        if (!is.null(socket)) {
          self$terminate_workers(index = index)
          self$workers$socket[index] <- socket
          self$workers$launched[index] <- FALSE
        }
      }
    },
    #' @description Launch a worker.
    #' @return `NULL` (invisibly).
    #' @param index Positive integer of length 1, index of the worker
    #'   to launch.
    launch = function(index) {
      self$forward(index = index, condition = "error")
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
      complete <- self$workers$complete[index]
      history <- self$workers$history[index]
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
          "launch_max above",
          self$launch_max,
          "or troubleshoot your platform to figure out",
          "why {crew} workers are not booting up or connecting."
        )
      )
      mirai::call_mirai(aio = self$workers$handle[[index]])
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
      self$workers$terminated[index] <- FALSE
      self$workers$history[index] <- complete
      invisible()
    },
    #' @description Forward an asynchronous launch/termination error condition
    #'   of a worker.
    #' @return Throw an error, throw a warning, or return a character string,
    #'   depending on the `condition` argument.
    #' @param index Integer of length 1, index of the worker to inspect.
    #' @param condition Character of length 1 indicating what to do
    #'   with an error if found. `"error"` to throw an error,
    #'   `"warning"` to throw a warning,
    #'   `"message"` to print a message,
    #'   and `"character"` to return a character vector of specific
    #'   task-level error messages.
    #'   The return value is `NULL` if no error is found.
    forward = function(index, condition = "error") {
      launch <- mirai_error(self$workers$handle[[index]])
      termination <- mirai_error(self$workers$termination[[index]])
      if (is.null(launch) && is.null(termination)) {
        return(NULL)
      }
      message_launch <- if_any(
        is.null(launch),
        NULL,
        sprintf("Worker %s launch: %s", index, launch)
      )
      message_termination <- if_any(
        is.null(termination),
        NULL,
        sprintf("Worker %s termination: %s", index, termination)
      )
      message <- paste0(
        "Error asynchronously launching and/or terminating a worker. ",
        "Run the errors() method of the launcher ",
        "to see all the error messages. ",
        "To troubleshoot, it may be helpful to rerun with async disabled ",
        "using processes = NULL in the launcher.\n",
        paste(c(message_launch, message_termination), collapse = "\n")
      )
      switch(
        condition,
        error = crew_error(message = message),
        warning = crew_warning(message = message),
        message = crew_message(message = message),
        character = return(c(message_launch, message_termination))
      )
      invisible()
    },
    #' @description Collect and return the most recent error messages
    #'   from asynchronous worker launching and termination.
    #' @return Character vector of all the most recent error messages
    #'   from asynchronous worker launching and termination. `NULL`
    #'   if there are no errors.
    errors = function() {
      out <- lapply(
        X = seq_len(nrow(self$workers)),
        FUN = self$forward,
        condition = "character"
      )
      unlist(out) %||% NULL
    },
    #' @description Wait for any local asynchronous launch or termination
    #'   tasks to complete.
    #' @details Only relevant if `processes` is a positive integer.
    #' @return `NULL` (invisibly).
    wait = function() {
      if (!is.null(self$async) && !is.null(self$processes)) {
        lapply(X = self$workers$handle, FUN = mirai::call_mirai)
        lapply(X = self$workers$termination, FUN = mirai::call_mirai)
      }
      invisible()
    },
    #' @description Deprecated in version 0.5.0.9000 (2023-10-02). Not used.
    #' @return `NULL`
    throttle = function() {
      crew_deprecate(
        name = "throttle()",
        date = "2023-10-02",
        version = "0.5.0.9003",
        alternative = "none (no longer necessary)",
        condition = "message",
        value = "throttle",
        skip_cran = TRUE,
        frequency = "once"
      )
    },
    #' @description Auto-scale workers out to meet the demand of tasks.
    #' @return `NULL` (invisibly)
    #' @param demand Number of unresolved tasks.
    #' @param throttle Deprecated in version 0.5.0.9000 (2023-10-02).
    scale = function(demand, throttle = NULL) {
      crew_deprecate(
        name = "throttle",
        date = "2023-10-02",
        version = "0.5.0.9003",
        alternative = "none (no longer necessary)",
        condition = "message",
        value = throttle,
        skip_cran = TRUE,
        frequency = "once"
      )
      self$tally()
      self$rotate()
      unlaunched <- self$unlaunched(n = Inf)
      active <- nrow(self$workers) - length(unlaunched)
      deficit <- min(length(unlaunched), max(0L, demand - active))
      walk(x = head(x = unlaunched, n = deficit), f = self$launch)
      invisible()
    },
    #' @description Abstract worker launch method.
    #' @details Launcher plugins will overwrite this method.
    #' @return A handle to mock the worker launch.
    #' @param call Character of length 1 with a namespaced call to
    #'   [crew_worker()] which will run in the worker and accept tasks.
    #' @param name Character of length 1 with an informative worker name.
    #' @param launcher Character of length 1, name of the launcher.
    #' @param worker Positive integer of length 1, index of the worker.
    #'   This worker index remains the same even when the current instance
    #'   of the worker exits and a new instance launches.
    #'   It is always between 1 and the maximum number of concurrent workers.
    #' @param instance Character of length 1 to uniquely identify
    #'   the current instance of the worker a the index in the launcher.
    launch_worker = function(call, name, launcher, worker, instance) {
      for (field in list(call, name, launcher, instance)) {
        crew_assert(
          field,
          is.character(.),
          length(.) == 1L,
          !anyNA(.),
          nzchar(.)
        )
      }
      crew_assert(
        worker,
        is.numeric(.),
        length(.) == 1L,
        !anyNA(.),
        . > 0L
      )
      list(abstract = TRUE)
    },
    #' @description Abstract worker termination method.
    #' @details Launcher plugins will overwrite this method.
    #' @return A handle to mock worker termination.
    #' @param handle A handle object previously
    #'   returned by `launch_worker()` which allows the termination
    #'   of the worker.
    terminate_worker = function(handle) {
      list(abstract = TRUE)
    },
    #' @description Terminate one or more workers.
    #' @return `NULL` (invisibly).
    #' @param index Integer vector of the indexes of the workers
    #'   to terminate. If `NULL`, all current workers are terminated.
    terminate_workers = function(index = NULL) {
      workers <- .subset2(self, "workers")
      if (is.null(workers)) {
        return(invisible())
      }
      index <- index %|||% seq_len(nrow(workers))
      for (worker in index) {
        should_terminate <- !workers$terminated[worker] ||
          # TODO: remove this part when crew.cluster updates on cran:
          (identical(Sys.getenv("TESTTHAT"), "true") &&
             !is_crew_null(workers$handle[[worker]]))
        if (should_terminate) {
          handle <- workers$handle[[worker]]
          mirai::call_mirai(aio = handle)
          self$workers$termination[[worker]] <-
            self$terminate_worker(handle = handle) %|||% crew_null
        }
        self$workers$socket[worker] <- NA_character_
        self$workers$start[worker] <- NA_real_
        self$workers$terminated[worker] <- TRUE
        self$forward(index = worker, condition = "warning")
      }
      invisible()
    },
    #' @description Terminate the whole launcher, including all workers.
    #' @return `NULL` (invisibly).
    terminate = function() {
      self$terminate_workers()
      if (!is.null(self$async)) {
        self$wait()
        self$async$terminate()
        lapply(
          X = seq_len(nrow(self$workers)),
          FUN = self$forward,
          condition = "error"
        )
      }
    }
  )
)
