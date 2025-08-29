#' @title Create an abstract launcher.
#' @export
#' @family launcher
#' @description This function is useful for inheriting argument documentation
#'   in functions that create custom third-party launchers. See
#'   `@inheritParams crew::crew_launcher` in the source code file of
#'   [crew_launcher_local()].
#' @section Auto-scaling:
#'   `crew` launchers implement auto-scaling in the `scale()` method.
#'   When the task load increases, the number of workers increases in response
#'   to demand. When the task load decreases, the workers start to exit.
#'   This behavior happens dynamically over the course of a workflow,
#'   and it can be tuned with arguments `seconds_interval`, `seconds_wall`,
#'   and `tasks_max`.
#'
#'   `tasks_max` is special: it determines not only the number of tasks
#'   a worker runs before exiting, it also determines how often
#'   auto-scaling runs.
#'   If `tasks_max` is finite, then `crew` uses an aggressive
#'   deterministic exponential backoff algorithm to determine how frequently
#'   to auto-scale (see [crew_throttle()]).
#'   But if `tasks_max` is `Inf`, then `crew` only scales at equally-spaced
#'   time intervals of `seconds_interval` to allow enough pending
#'   tasks to accumulate for job arrays.
#'   This last part is important because auto-scaling
#'   too frequently could lead to hundreds of separate job arrays with only
#'   job per array (as opposed to the desired outcome of 1 or 2 arrays
#'   with many jobs each).
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
#'   such as checking `mirai::info()`.
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
#'   exiting. Also determines how often the controller auto-scales.
#'   See the Auto-scaling section for details.
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
#' @param processes Deprecated on 2025-08-27 (`crew` version 1.2.1.9009).
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
  seconds_interval = 0.25,
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

launcher_empty_launches <- tibble::tibble(
  handle = list(),
  start = numeric(0L),
  submitted = logical(0L),
  count = integer(0L),
  total = integer(0L)
)

# TODO: remove when the next crew.aws.batch release drops async$eval().
deprecated_async_eval <- function(
  command,
  substitute = TRUE,
  data = list(),
  packages = character(0L),
  library = NULL
) {
  crew_deprecate(
    name = "async$eval() in crew plugins",
    date = "2025-08-27",
    version = "1.2.1.9009",
    alternative = "none",
    condition = "message",
    value = "x"
  )
  command <- if_any(substitute, substitute(command), command)
  load_packages(packages = packages, library = library)
  eval(expr = command, envir = list2env(data, parent = globalenv()))
}

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
    .launches = launcher_empty_launches,
    .throttle = NULL,
    .failed = 0L
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
    #' @field launches Data frame tracking worker launches with one row
    #'   per launch. Each launch may create more than one worker.
    #'   Old superfluous rows are periodically discarded for efficiency.
    launches = function() {
      .subset2(private, ".launches")
    },
    #' @field throttle A [crew_throttle()] object to throttle scaling.
    throttle = function() {
      .subset2(private, ".throttle")
    },
    #' @field failed Number of failed worker launches
    #'   (launches that exceed `seconds_launch` seconds to dial in).
    failed = function() {
      .subset2(private, ".failed")
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
    #' @param processes Deprecated on 2025-08-27 (`crew` version 1.2.1.9009).
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
        date = "2025-08-27",
        version = "1.2.1.9009",
        alternative = "none",
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
      private$.throttle <- crew_throttle(
        seconds_max = seconds_interval,
        # Persistent workers should not auto-scale too frequently
        # because then job arrays would not be as effective (see help file).
        seconds_min = if_any(
          is.finite(tasks_max),
          # Should be too small to feel but should reach seconds_interval
          # when doubled enough times
          # (doublings = ceiling(log (1 / seconds_min) / log(2))).
          1e-6,
          # Should be enough time
          # to accumulate tasks but not enough time to feel.
          seconds_interval
        )
      )
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
      if (!is.null(self$launch_worker)) {
        crew_assert(
          is.function(self$launch_worker),
          message = "If supplied, launch_worker() must be a function."
        )
        fields <- "call"
        if (!is.null(self$launch_worker)) {
          crew_assert(
            fields %in% names(formals(self$launch_worker)),
            message = paste(
              "If supplied, launch_worker()",
              "must have the following arguments:",
              paste(sprintf("\"%s\"", fields), collapse = ", ")
            )
          )
        }
      }
      if (!is.null(self$launch_workers)) {
        crew_assert(
          is.function(self$launch_workers),
          message = "If supplied, launch_worker() must be a function."
        )
        fields <- c("call", "n")
        if (!is.null(self$launch_workers)) {
          crew_assert(
            fields %in% names(formals(self$launch_workers)),
            message = paste(
              "If supplied, launch_workers()",
              "must have the following arguments:",
              paste(sprintf("\"%s\"", fields), collapse = ", ")
            )
          )
        }
      }
      crew_deprecate(
        name = "terminate_worker()",
        date = "2025-08-26",
        version = "1.2.1.9004",
        alternative = paste(
          "none. crew plugins no longer need (or use) terminate_worker()",
          "because terminating the underlying network connection",
          "with controller$teriminate() is enough to shut down the workers.",
          "For 100% certainty that workers terminate correctly and do not",
          "keep running indefinitely, please set a finite seconds_idle",
          "value in the controller, and please watch worker processes",
          "using the appropriate monitor for your cmoputing platform",
          "(for example, crew_monitor_local() for workers",
          "created from crew_controller_local())."
        ),
        condition = "message",
        value = self$terminate_worker,
        skip_cran = FALSE,
        frequency = "once"
      )
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
      crew_assert(private$.launches, is.data.frame(.))
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
        tlscert = private$.tls$worker(profile = private$.profile),
        rs = mirai::nextstream(.compute = private$.profile)
      )
    },
    #' @description Create a call to [crew_worker()] to
    #'   help create custom launchers.
    #' @return Character string with a call to [crew_worker()].
    #' @param worker Deprecated on 2025-08-28 (`crew` version 1.2.1.9009).
    #' @examples
    #' launcher <- crew_launcher_local()
    #' launcher$start(url = "tcp://127.0.0.1:57000", profile = "profile")
    #' launcher$call()
    #' launcher$terminate()
    call = function(worker = NULL) {
      crew::crew_deprecate(
        name = "worker argument of launcher$call()",
        date = "2025-02-28",
        version = "1.2.1.9009",
        alternative = "none",
        condition = "message",
        value = worker,
        skip_cran = TRUE,
        frequency = "once"
      )
      call <- substitute(
        crew::crew_worker(
          settings = settings,
          controller = controller,
          options_metrics = crew::crew_options_metrics(
            path = path,
            seconds_interval = seconds_interval
          )
        ),
        env = list(
          settings = self$settings(),
          controller = private$.name,
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
      private$.url <- url
      private$.profile <- profile
      private$.launches <- launcher_empty_launches
      private$.failed <- 0L
      invisible()
    },
    #' @description Terminate the whole launcher, including all workers.
    #' @return `NULL` (invisibly).
    terminate = function() {
      private$.url <- NULL
      private$.profile <- NULL
      private$.failed <- 0L
      if (!is.null(private$.throttle)) {
        private$.throttle$reset()
      }
      invisible()
    },
    #' @description Launch a worker.
    #' @return Handle of the launched worker.
    #' @param n Positive integer, number of workers to launch.
    launch = function(n = 1L) {
      if (n < 1L) {
        return(invisible())
      }
      handle <- self$launch_workers(call = self$call(), n = n)
      launches <- private$.launches
      private$.launches <- tibble::add_row(
        private$.launches,
        handle = list(handle) %||% crew_null,
        start = now(),
        submitted = FALSE,
        count = n,
        total = n + (launches$total[nrow(launches)] %||% 0L)
      )
      invisible()
    },
    #' @description Abstract worker launch method.
    #' @details Launcher plugins will overwrite this method.
    #' @return A handle to mock the worker launch.
    #' @param call Character of length 1 with a namespaced call to
    #'   [crew_worker()] which will run in the worker and accept tasks.
    launch_worker = function(call) {
      list(call = call)
    },
    #' @description Launch multiple workers.
    #' @details Launcher plugins may overwrite this method
    #'   to launch multiple workers from a single system call.
    #' @return A handle to mock the worker launch.
    #' @param call Character of length 1 with a namespaced call to
    #'   [crew_worker()] which will run in each worker and accept tasks.
    #' @param n Positive integer, number of workers to launch.
    launch_workers = function(call, n) {
      # TODO: remove argument name compatibility layer after enough
      # releases of downstream plugins.
      args <- list(
        call = call,
        name = "name",
        launcher = private$.name,
        worker = "worker",
        instance = "instance"
      )
      args <- args[names(formals(self$launch_worker))]
      replicate(n, do.call(self$launch_worker, args), simplify = FALSE)
    },
    #' @description Auto-scale workers out to meet the demand of tasks.
    #' @return Invisibly returns `TRUE` if there was any relevant
    #'   auto-scaling activity (new worker launches or worker
    #'   connection/disconnection events) (`FALSE` otherwise).
    #' @param status A `mirai` status list with worker and task information.
    #' @param throttle Deprecated, only used in the controller
    #'   as of 2025-01-16 (`crew` version 0.10.2.9003).
    scale = function(status, throttle = NULL) {
      # Count the number of workers we still expect to be launching.
      launches <- private$.launches
      total <- launches$total[nrow(launches)]
      connections <- .subset(status, "connections")
      disconnections <- .subset(status, "cumulative") - connections
      failed <- private$.failed
      expected_launching <- total - connections - disconnections - failed
      # In case workers are submitted outside crew:
      expected_launching <- max(0L, expected_launching)
      # Among the workers we still expect to be launching,
      # count the subset which are actually truly still launching
      # (dubbed "already_launching" to reflect the logic later on).
      # These are the workers for whom startup period not yet expired.
      start <- rep.int(launches$start, launches$count)
      start <- utils::tail(start, expected_launching)
      is_already_launching <- (now() - start) < private$.seconds_launch
      already_launching <- sum(is_already_launching)
      # The workers with expired startup windows have failed.
      # We need to record those for future calls to scale().
      private$.failed <- failed + expected_launching - already_launching
      # Remove superfluous rows to minimize overhead from rep.int() above.
      still_relevant <- launches$total >= connections + disconnections
      private$.launches <- launches[still_relevant, , drop = FALSE]
      # Figure out how many workers to launch.
      # We want to ensure the number of active workers
      # meets the demand of unresolved tasks (up to a pre-determined cap).
      tasks <- .subset(status, "awaiting") + .subset(status, "executing")
      demand <- min(private$.workers, tasks)
      supply <- already_launching + connections
      should_launch <- max(0L, demand - supply)
      # Launch those workers.
      self$launch(n = should_launch)
      # Tune the launcher polling interval using exponential backoff.
      activity <- should_launch > 0L
      private$.throttle$update(activity = activity)
      invisible(activity)
    },
    #' @description Deprecated on 2025-08-26
    #'  (`crew` version 1.2.1.9004).
    #' @return `NULL` (invisibly).
    terminate_workers = function() {
      crew_deprecate(
        name = "terminate_worker()",
        date = "2025-08-26",
        version = "1.2.1.9004",
        alternative = "none",
        condition = "message",
        value = "x",
        skip_cran = TRUE,
        frequency = "once"
      )
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
    },
    #' @field async Deprecated on 2025-08-27 (`crew` version 1.2.1.9009).
    async = list(eval = deprecated_async_eval)
  )
)
