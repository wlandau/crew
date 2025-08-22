#' @title Create an abstract launcher.
#' @export
#' @family launcher
#' @description This function is useful for inheriting argument documentation
#'   in functions that create custom third-party launchers. See
#'   `@inheritParams crew::crew_launcher` in the source code file of
#'   [crew_launcher_local()].
#' @inheritParams crew_client
#' @inheritParams crew_worker
#' @param name Character string, name of the launcher. If the name is
#'   `NULL`, then a name is automatically generated when the launcher starts.
#' @param workers Maximum number of workers to run concurrently
#'   when auto-scaling, excluding task retries and manual calls to `launch()`.
#'   Special workers allocated for task retries
#'   do not count towards this limit, so the number of workers running at a
#'   given time may exceed this maximum.
#'   A smaller number of workers may run if the number of executing tasks
#'   is smaller than the supplied value of the `workers` argument.
#' @param seconds_interval Number of seconds between
#'   polling intervals waiting for certain internal
#'   synchronous operations to complete. In certain cases, exponential
#'   backoff is used with this argument passed to `seconds_max`
#'   in a [crew_throttle()] object.
#' @param seconds_timeout Number of seconds until timing
#'   out while waiting for certain synchronous operations to complete,
#'   such as checking `mirai::status()`.
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
#' @param reset_globals Deprecated on 2025-05-30 (`crew` version 1.1.2.9004).
#'   Please use the `reset_globals` option of [crew::crew_controller()]
#'   instead.
#' @param reset_packages Deprecated on 2025-05-30 (`crew` version 1.1.2.9004).
#'   Please use the `reset_packages` option of [crew::crew_controller()]
#'   instead.
#' @param reset_options Deprecated on 2025-05-30 (`crew` version 1.1.2.9004).
#'   Please use the `reset_options` option of [crew::crew_controller()]
#'   instead.
#' @param garbage_collection Deprecated on 2025-05-30
#'   (`crew` version 1.1.2.9004).
#'   Please use the `garbage_collection` option of
#'   [crew::crew_controller()] instead.
#' @param crashes_error Deprecated on 2025-01-13 (`crew` version 0.10.2.9002).
#' @param launch_max Deprecated on 2024-11-04 (`crew` version 0.10.2.9002).
#' @param processes Deprecated on 2025-08-22 (`crew` version 1.2.1.9004).
#' @param r_arguments Optional character vector of command line arguments
#'   to pass to `Rscript` (non-Windows) or `Rscript.exe` (Windows)
#'   when starting a worker. Example:
#'   `r_arguments = c("--vanilla", "--max-connections=32")`.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' client$start()
#' launcher <- crew_launcher_local()
#' launcher$start(url = client$url, profile = client$profile)
#' launcher$launch()
#' task <- mirai::mirai("result", .compute = client$profile)
#' mirai::call_mirai(task)
#' task$data
#' client$terminate()
#' }
crew_launcher <- function(
  name = NULL,
  workers = 1L,
  seconds_interval = 1,
  seconds_timeout = 60,
  seconds_launch = 30,
  seconds_idle = 300,
  seconds_wall = Inf,
  seconds_exit = NULL,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = NULL,
  reset_packages = NULL,
  reset_options = NULL,
  garbage_collection = NULL,
  crashes_error = NULL,
  launch_max = NULL,
  tls = crew::crew_tls(),
  processes = NULL,
  r_arguments = c("--no-save", "--no-restore"),
  options_metrics = crew::crew_options_metrics()
) {
  launcher <- crew_class_launcher$new(
    name = name,
    workers = workers,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    tls = tls,
    processes = processes,
    r_arguments = r_arguments,
    options_metrics = options_metrics
  )
  launcher$validate()
  launcher
}

launcher_empty_instances <- tibble::tibble(
  handle = list(),
  start = numeric(0L)
)

#' @title Launcher abstract class
#' @export
#' @family launcher
#' @description `R6` abstract class to build other subclasses
#'   which launch and manage workers.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' client$start()
#' launcher <- crew_launcher_local()
#' launcher$start(url = client$url, profile = client$profile)
#' launcher$launch()
#' task <- mirai::mirai("result", .compute = client$profile)
#' mirai::call_mirai(task)
#' task$data
#' client$terminate()
#' }
crew_class_launcher <- R6::R6Class(
  classname = "crew_class_launcher",
  cloneable = TRUE,
  portable = TRUE,
  private = list(
    .name = NULL,
    .workers = NULL,
    .seconds_interval = NULL,
    .seconds_timeout = NULL,
    .seconds_launch = NULL,
    .seconds_idle = NULL,
    .seconds_wall = NULL,
    .tasks_max = NULL,
    .tasks_timers = NULL,
    .tls = NULL,
    .r_arguments = NULL,
    .options_metrics = NULL,
    .url = NULL,
    .profile = NULL,
    .instances = launcher_empty_instances,
    .id = NULL,
    .throttle = NULL
  ),
  active = list(
    #' @field name See [crew_launcher()].
    name = function() {
      .subset2(private, ".name")
    },
    #' @field workers See [crew_launcher()].
    workers = function() {
      .subset2(private, ".workers")
    },
    #' @field seconds_interval See [crew_launcher()].
    seconds_interval = function() {
      .subset2(private, ".seconds_interval")
    },
    #' @field seconds_timeout See [crew_launcher()].
    seconds_timeout = function() {
      .subset2(private, ".seconds_timeout")
    },
    #' @field seconds_launch See [crew_launcher()].
    seconds_launch = function() {
      .subset2(private, ".seconds_launch")
    },
    #' @field seconds_idle See [crew_launcher()].
    seconds_idle = function() {
      .subset2(private, ".seconds_idle")
    },
    #' @field seconds_wall See [crew_launcher()].
    seconds_wall = function() {
      .subset2(private, ".seconds_wall")
    },
    #' @field tasks_max See [crew_launcher()].
    tasks_max = function() {
      .subset2(private, ".tasks_max")
    },
    #' @field tasks_timers See [crew_launcher()].
    tasks_timers = function() {
      .subset2(private, ".tasks_timers")
    },
    #' @field tls See [crew_launcher()].
    tls = function() {
      .subset2(private, ".tls")
    },
    #' @field r_arguments See [crew_launcher()].
    r_arguments = function() {
      .subset2(private, ".r_arguments")
    },
    #' @field options_metrics See [crew_launcher()].
    options_metrics = function() {
      .subset2(private, ".options_metrics")
    },
    #' @field url Websocket URL for worker connections.
    url = function() {
      .subset2(private, ".url")
    },
    #' @field profile `mirai` compute profile of the launcher.
    profile = function() {
      .subset2(private, ".profile")
    },
    #' @field instances Data frame of worker instance information.
    instances = function() {
      .subset2(private, ".instances")
    },
    #' @field id Integer worker ID from the last call to `settings()`.
    id = function() {
      .subset2(private, ".id")
    },
    #' @field throttle A [crew_throttle()] object to throttle scaling.
    throttle = function() {
      .subset2(private, ".throttle")
    }
  ),
  public = list(
    #' @description Launcher constructor.
    #' @return An `R6` object with the launcher.
    #' @param name See [crew_launcher()].
    #' @param workers See [crew_launcher()].
    #' @param seconds_interval See [crew_launcher()].
    #' @param seconds_timeout See [crew_launcher()].
    #' @param seconds_launch See [crew_launcher()].
    #' @param seconds_idle See [crew_launcher()].
    #' @param seconds_wall See [crew_launcher()].
    #' @param seconds_exit See [crew_launcher()].
    #' @param tasks_max See [crew_launcher()].
    #' @param tasks_timers See [crew_launcher()].
    #' @param reset_globals Deprecated. See [crew_launcher()].
    #' @param reset_packages Deprecated. See [crew_launcher()].
    #' @param reset_options Deprecated. See [crew_launcher()].
    #' @param garbage_collection Deprecated. See [crew_launcher()].
    #' @param crashes_error See [crew_launcher()].
    #' @param launch_max Deprecated.
    #' @param tls See [crew_launcher()].
    #' @param processes Deprecated on 2025-08-22 (`crew` version 1.2.1.9004).
    #' @param r_arguments See [crew_launcher()].
    #' @param options_metrics See [crew_launcher()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' client <- crew_client()
    #' client$start()
    #' launcher <- crew_launcher_local()
    #' launcher$start(url = client$url, profile = client$profile)
    #' launcher$launch()
    #' task <- mirai::mirai("result", .compute = client$profile)
    #' mirai::call_mirai(task)
    #' task$data
    #' client$terminate()
    #' }
    initialize = function(
      name = NULL,
      workers = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
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
      crashes_error = NULL,
      launch_max = NULL,
      tls = NULL,
      processes = NULL,
      r_arguments = NULL,
      options_metrics = NULL
    ) {
      crew_deprecate(
        name = "The reset_globals argument in crew launchers",
        date = "2025-05-30",
        version = "1.1.2.9004",
        alternative = "reset_globals argument of crew::crew_controller()",
        condition = "message",
        value = reset_globals
      )
      crew_deprecate(
        name = "The reset_packages argument in crew launchers",
        date = "2025-05-30",
        version = "1.1.2.9004",
        alternative = "reset_packages argument of crew::crew_controller()",
        condition = "message",
        value = reset_packages
      )
      crew_deprecate(
        name = "The reset_options argument in crew launchers",
        date = "2025-05-30",
        version = "1.1.2.9004",
        alternative = "reset_options argument of crew::crew_controller()",
        condition = "message",
        value = reset_options
      )
      crew_deprecate(
        name = "The garbage_collection argument in crew launchers",
        date = "2025-05-30",
        version = "1.1.2.9004",
        alternative = paste(
          "garbage_collection argument of",
          "crew::crew_controller()"
        ),
        condition = "message",
        value = garbage_collection
      )
      crew_deprecate(
        name = "launch_max",
        date = "2024-11-04",
        version = "0.10.1.9000",
        alternative = "none",
        condition = "warning",
        value = launch_max
      )
      crew_deprecate(
        name = "seconds_exit",
        date = "2023-09-21",
        version = "0.5.0.9002",
        alternative = "none (no longer necessary)",
        condition = "warning",
        value = seconds_exit
      )
      crew_deprecate(
        name = "crashes_error",
        date = "2025-01-13",
        version = "0.10.2.9002",
        alternative = "none",
        condition = "message",
        value = crashes_error
      )
      crew_deprecate(
        name = "processes",
        date = "2025-08-22",
        version = "1.2.1.9004",
        alternative = "none (no longer supported)",
        condition = "message",
        value = processes
      )
      private$.name <- name %|||% crew_random_name(n = 4L)
      private$.workers <- workers
      private$.seconds_interval <- seconds_interval
      private$.seconds_timeout <- seconds_timeout
      private$.seconds_launch <- seconds_launch
      private$.seconds_idle <- seconds_idle
      private$.seconds_wall <- seconds_wall
      private$.tasks_max <- tasks_max
      private$.tasks_timers <- tasks_timers
      private$.tls <- tls
      private$.r_arguments <- r_arguments
      private$.options_metrics <- options_metrics
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly).
    validate = function() {
      if (!is.null(private$.name)) {
        crew_assert(
          private$.name,
          is.character(.),
          length(.) == 1L,
          !anyNA(.),
          nzchar(.),
          message = c(
            "launcher name must be a nonempty non-missing character string"
          )
        )
      }
      crew_assert(
        is.function(self$launch_worker),
        message = "launch_worker() must be a function."
      )
      fields <- c("call", "name", "launcher", "worker")
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
      fields <- c(
        "seconds_interval",
        "seconds_timeout",
        "seconds_launch",
        "seconds_idle",
        "seconds_wall",
        "tasks_max",
        "tasks_timers",
        "workers"
      )
      for (field in fields) {
        crew_assert(
          self[[field]],
          is.numeric(.),
          . >= 0,
          length(.) == 1L,
          !anyNA(.),
          message = paste(field, "must be a non-missing numeric of length 1")
        )
      }
      if (!is.null(private$.url)) {
        crew_assert(
          private$.url,
          is.character(.),
          !anyNA(.),
          length(.) == 1L,
          nzchar(.)
        )
      }
      crew_assert(private$.instances, is.data.frame(.))
      if (!is.null(private$.r_arguments)) {
        crew_assert(
          is.character(private$.r_arguments),
          message = "r_arguments must be a character vector."
        )
      }
      crew_assert(
        inherits(private$.tls, "crew_class_tls"),
        message = "field 'tls' must be an object created by crew_tls()"
      )
      private$.tls$validate()
      if (!is.null(private$.throttle)) {
        crew_assert(
          inherits(private$.throttle, "crew_class_throttle"),
          message = "field 'throttle' must be an object from crew_throttle()"
        )
        private$.throttle$validate()
      }
      if (!is.null(private$.options_metrics)) {
        crew_options_metrics_validate(private$.options_metrics)
      }
      invisible()
    },
    #' @description Poll the throttle.
    #' @return `TRUE` to run whatever work comes next, `FALSE` to skip
    #'   until the appropriate time.
    poll = function() {
      private$.throttle$poll()
    },
    #' @description List of arguments for `mirai::daemon()`.
    #' @return List of arguments for `mirai::daemon()`.
    settings = function() {
      private$.id <- private$.id + 1L
      list(
        url = private$.url,
        dispatcher = TRUE,
        asyncdial = FALSE,
        autoexit = crew_terminate_signal(),
        cleanup = FALSE,
        output = TRUE,
        maxtasks = private$.tasks_max,
        idletime = private$.seconds_idle * 1000,
        walltime = private$.seconds_wall * 1000,
        timerstart = private$.tasks_timers,
        id = private$.id,
        tlscert = private$.tls$worker(profile = private$.profile),
        rs = mirai::nextstream(.compute = private$.profile)
      )
    },
    #' @description Create a call to [crew_worker()] to
    #'   help create custom launchers.
    #' @return Character string with a call to [crew_worker()].
    #' @param worker Character string, name of the worker.
    #' @param socket Deprecated on 2025-01-28 (`crew` version 1.0.0).
    #' @param launcher Deprecated on 2025-01-28 (`crew` version 1.0.0).
    #' @param instance Deprecated on 2025-01-28 (`crew` version 1.0.0).
    #' @examples
    #' launcher <- crew_launcher_local()
    #' launcher$start(url = "tcp://127.0.0.1:57000", profile = "profile")
    #' launcher$call(worker = "worker_name")
    #' launcher$terminate()
    call = function(
      worker,
      socket = NULL,
      launcher = NULL,
      instance = NULL
    ) {
      call <- substitute(
        crew::crew_worker(
          settings = settings,
          launcher = launcher,
          worker = worker,
          options_metrics = crew::crew_options_metrics(
            path = path,
            seconds_interval = seconds_interval
          )
        ),
        env = list(
          settings = self$settings(),
          launcher = private$.name,
          worker = worker,
          path = private$.options_metrics$path,
          seconds_interval = private$.options_metrics$seconds_interval %|||% 5
        )
      )
      out <- deparse_safe(expr = call, collapse = " ")
      gsub(pattern = "   *", replacement = " ", x = out)
    },
    #' @description Start the launcher.
    #' @param url Character string, websocket URL for worker connections.
    #' @param profile Character string, `mirai` compute profile.
    #' @param sockets Deprecated on 2025-01-28 (`crew` version 1.0.0).
    #' @return `NULL` (invisibly).
    start = function(url = NULL, profile = NULL, sockets = NULL) {
      crew::crew_deprecate(
        name = "sockets",
        date = "2025-01-28",
        version = "1.0.0",
        alternative = "url and profile",
        condition = "message",
        value = sockets,
        skip_cran = TRUE
      )
      url <- url %|||% sockets[1L]
      profile <- profile %|||% crew_random_name()
      private$.throttle <- crew_throttle(seconds_max = self$seconds_interval)
      private$.url <- url
      private$.profile <- profile
      private$.instances <- launcher_empty_instances
      private$.id <- 0L
      invisible()
    },
    #' @description Terminate the whole launcher, including all workers.
    #' @return `NULL` (invisibly).
    terminate = function() {
      private$.url <- NULL
      private$.profile <- NULL
      if (!is.null(private$.throttle)) {
        private$.throttle$reset()
      }
      self$terminate_workers()
      invisible()
    },
    #' @description Launch a worker.
    #' @return Handle of the launched worker.
    launch = function() {
      launcher <- private$.name
      worker <- crew_random_name(n = 4L)
      call <- self$call(worker = worker)
      name <- name_worker(launcher = launcher, worker = worker)
      handle <- self$launch_worker(
        call = call,
        name = name,
        launcher = launcher,
        worker = worker
      )
      private$.instances <- tibble::add_row(
        private$.instances,
        handle = list(handle) %||% crew_null,
        start = now()
      )
      invisible()
    },
    #' @description Auto-scale workers out to meet the demand of tasks.
    #' @return Invisibly returns `TRUE` if workers were launched and `FALSE`
    #'   otherwise
    #' @param status A `mirai` status list with worker and task information.
    #' @param throttle Deprecated, only used in the controller
    #'   as of 2025-01-16 (`crew` version 0.10.2.9003).
    scale = function(status, throttle = NULL) {
      elapsed <- (now() - private$.instances$start)
      launching <- sum(elapsed < private$.seconds_launch)
      supply <- status["connections"] + launching
      demand <- max(private$.workers, status["awaiting"] + status["executing"])
      activity <- FALSE
      for (index in seq_len(demand - supply)) {
        self$launch()
        activity <- TRUE
      }
      private$.throttle$update(activity = activity)
      invisible(activity)
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
    launch_worker = function(call, name, launcher, worker) {
      list(
        call = call,
        name = name,
        launcher = launcher,
        worker = worker,
        abstract = TRUE
      )
    },
    #' @description Abstract worker termination method.
    #' @details Launcher plugins will overwrite this method.
    #' @return A handle to mock worker termination.
    #' @param handle A handle object previously
    #'   returned by `launch_worker()` which allows the termination
    #'   of the worker.
    terminate_worker = function(handle) {
      list(handle = handle, abstract = TRUE)
    },
    #' @description Terminate all workers.
    #' @return `NULL` (invisibly).
    terminate_workers = function() {
      instances <- .subset2(self, "instances")
      for (index in seq_len(nrow(instances))) {
        self$terminate_worker(handle = instances$handle[[index]])
      }
      private$.instances <- launcher_empty_instances
      invisible()
    },
    #' @description Deprecated on 2025-01-28 (`crew` version 1.0.0).
    #' @param index Unused argument.
    #' @return The integer 1, for compatibility.
    crashes = function(index = NULL) {
      crew_deprecate(
        name = "crashes() method in crew launchers",
        date = "2025-01-28",
        version = "1.0.0",
        alternative = "none",
        condition = "message",
        value = "x",
        skip_cran = TRUE
      )
      1L
    },
    #' @description Deprecated on 2025-01-28 (`crew` version 1.0.0).
    #' @param name Name to set for the launcher.
    #' @return `NULL` (invisibly).
    set_name = function(name) {
      crew_deprecate(
        name = "crashes() method in crew launchers",
        date = "2025-01-28",
        version = "1.0.0",
        alternative = "none",
        condition = "message",
        value = "x",
        skip_cran = TRUE
      )
      private$.name <- name
      invisible()
    }
  )
)
