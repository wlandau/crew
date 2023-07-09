#' @title Create a controller object from a client and launcher.
#' @export
#' @family developer
#' @description This function is for developers of `crew` launcher plugins.
#'   Users should use a specific controller helper such as
#'   [crew_controller_local()].
#' @param client An `R6` client object created by [crew_client()].
#' @param launcher An `R6` launcher object created by one of the
#'   `crew_launcher_*()` functions such as [crew_launcher_local()].
#' @param auto_scale Deprecated. Use the `scale` argument of `push()`,
#'   `pop()`, and `wait()` instead.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' launcher <- crew_launcher_local()
#' controller <- crew_controller(client = client, launcher = launcher)
#' controller$start()
#' controller$push(name = "task", command = sqrt(4))
#' controller$wait()
#' controller$pop()
#' controller$terminate()
#' }
crew_controller <- function(
  client,
  launcher,
  auto_scale = NULL
) {
  if (!is.null(auto_scale)) {
    crew_deprecate(
      name = "auto_scale",
      date = "2023-05-18",
      version = "0.2.0",
      alternative = "use the scale argument of push(), pop(), and wait()"
    )
  }
  schedule <- crew_schedule(seconds_interval = client$seconds_interval)
  controller <- crew_class_controller$new(
    client = client,
    launcher = launcher,
    schedule = schedule
  )
  controller$launcher$name <- controller$client$name
  controller$validate()
  controller
}

#' @title Controller class
#' @export
#' @family class
#' @description `R6` class for controllers.
#' @details See [crew_controller()].
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' launcher <- crew_launcher_local()
#' controller <- crew_controller(client = client, launcher = launcher)
#' controller$start()
#' controller$push(name = "task", command = sqrt(4))
#' controller$wait()
#' controller$pop()
#' controller$terminate()
#' }
crew_class_controller <- R6::R6Class(
  classname = "crew_class_controller",
  cloneable = FALSE,
  public = list(
    #' @field client Router object.
    client = NULL,
    #' @field launcher Launcher object.
    launcher = NULL,
    #' @field schedule Schedule object.
    schedule = NULL,
    #' @field log Tibble with per-worker metadata about tasks.
    log = NULL,
    #' @field until Numeric of length 1, time point when
    #'   throttled auto-scaling unlocks.
    until = NULL,
    #' @field error Tibble of monads from the last call to
    #'   `map(error = "stop)`.
    error = NULL,
    #' @description `mirai` controller constructor.
    #' @return An `R6` controller object.
    #' @param client Router object. See [crew_controller()].
    #' @param launcher Launcher object. See [crew_controller()].
    #' @param schedule Schedule object from [crew_schedule()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' client <- crew_client()
    #' launcher <- crew_launcher_local()
    #' controller <- crew_controller(client = client, launcher = launcher)
    #' controller$start()
    #' controller$push(name = "task", command = sqrt(4))
    #' controller$wait()
    #' controller$pop()
    #' controller$terminate()
    #' }
    initialize = function(
      client = NULL,
      launcher = NULL,
      schedule = NULL
    ) {
      self$client <- client
      self$launcher <- launcher
      self$schedule <- schedule
      invisible()
    },
    #' @description Validate the client.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(inherits(self$client, "crew_class_client"))
      crew_assert(inherits(self$launcher, "crew_class_launcher"))
      crew_assert(inherits(self$schedule, "crew_class_schedule"))
      self$client$validate()
      self$launcher$validate()
      self$schedule$validate()
      crew_assert(
        identical(self$client$name, self$launcher$name),
        message = "client and launcher must have the same name"
      )
      crew_assert(self$log, is.null(.) || is.data.frame(.))
      invisible()
    },
    #' @description Check if the controller is empty.
    #' @details A controller is empty if it has no running tasks
    #'   or completed tasks waiting to be retrieved with `push()`.
    #' @return `TRUE` if the controller is empty, `FALSE` otherwise.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    empty = function(controllers = NULL) {
      .subset2(.subset2(self, "schedule"), "empty")()
    },
    #' @description Check if the controller is nonempty.
    #' @details A controller is empty if it has no running tasks
    #'   or completed tasks waiting to be retrieved with `push()`.
    #' @return `TRUE` if the controller is empty, `FALSE` otherwise.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    nonempty = function(controllers = NULL) {
      .subset2(.subset2(self, "schedule"), "nonempty")()
    },
    #' @description Check if the controller is saturated.
    #' @details A controller is saturated if the number of unresolved tasks
    #'   is greater than or equal to the maximum number of workers.
    #'   In other words, in a saturated controller, every available worker
    #'   has a task.
    #'   You can still push tasks to a saturated controller, but
    #'   tools that use `crew` such as `targets` may choose not to.
    #' @return `TRUE` if the controller is saturated, `FALSE` otherwise.
    #' @param collect Logical of length 1, whether to collect the results
    #'   of any newly resolved tasks before determining saturation.
    #' @param throttle Logical of length 1, whether to delay task collection
    #'   until the next request at least `self$client$seconds_interval`
    #'   seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    saturated = function(collect = TRUE, throttle = TRUE, controller = NULL) {
      if (collect) {
        .subset2(self, "collect")(throttle = throttle)
      }
      length(.subset2(.subset2(self, "schedule"), "pushed")) >=
        .subset2(.subset2(self, "client"), "workers")
    },
    #' @description Start the controller if it is not already started.
    #' @details Register the mirai client and register worker websockets
    #'   with the launcher.
    #' @return `NULL` (invisibly).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    start = function(controllers = NULL) {
      if (!isTRUE(.subset2(.subset2(self, "client"), "started"))) {
        self$client$start()
        workers <- self$client$workers
        self$launcher$start()
        self$schedule$start()
        self$log <- list(
          controller = rep(self$client$name, workers),
          worker = seq_len(workers),
          tasks = rep(0L, workers),
          seconds = rep(0, workers),
          errors = rep(0L, workers),
          warnings = rep(0L, workers)
        )
      }
      invisible()
    },
    #' @description Launch one or more workers.
    #' @return `NULL` (invisibly).
    #' @param n Number of workers to try to launch. The actual
    #'   number launched is capped so that no more than "`workers`"
    #'   workers running at a given time, where "`workers`"
    #'   is an argument of [crew_controller()]. The
    #'   actual cap is the "`workers`" argument minus the number of connected
    #'   workers minus the number of starting workers. A "connected"
    #'   worker has an active websocket connection to the `mirai` client,
    #'   and "starting" means that the worker was launched at most
    #'   `seconds_start` seconds ago, where `seconds_start` is
    #'   also an argument of [crew_controller()].
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    launch = function(n = 1L, controllers = NULL) {
      walk(x = self$launcher$done(), f = self$launcher$rotate)
      self$launcher$tally()
      walk(x = self$launcher$unlaunched(n = n), f = self$launcher$launch)
      invisible()
    },
    #' @description Auto-scale workers out to meet the demand of tasks.
    #' @details Methods `push()`, `pop()`, and `wait()` already invoke
    #'   `scale()` if the `scale` argument is `TRUE`.
    #'   For finer control of the number of workers launched,
    #'   call `launch()` on the controller with the exact desired
    #'   number of workers.
    #' @return `NULL` (invisibly).
    #' @param throttle Logical of length 1, whether to delay auto-scaling
    #'   until the next auto-scaling request at least
    #'  `self$client$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    scale = function(throttle = FALSE, controllers = NULL) {
      self$schedule$collect(throttle = FALSE)
      demand <- length(self$schedule$pushed)
      self$launcher$scale(demand = demand, throttle = throttle)
      invisible()
    },
    #' @description Push a task to the head of the task list.
    #' @return `NULL` (invisibly).
    #' @param command Language object with R code to run.
    #' @param data Named list of local data objects in the
    #'   evaluation environment.
    #' @param globals Named list of objects to temporarily assign to the
    #'   global environment for the task.
    #'   This list should
    #'   include any functions you previously defined in the global
    #'   environment which are required to run tasks.
    #'   See the `reset_globals` argument
    #'   of [crew_controller_local()].
    #' @param substitute Logical of length 1, whether to call
    #'   `base::substitute()` on the supplied value of the
    #'   `command` argument. If `TRUE` (default) then `command` is quoted
    #'   literally as you write it, e.g.
    #'   `push(command = your_function_call())`. If `FALSE`, then `crew`
    #'   assumes `command` is a language object and you are passing its
    #'   value, e.g. `push(command = quote(your_function_call()))`.
    #'   `substitute = TRUE` is appropriate for interactive use,
    #'   whereas `substitute = FALSE` is meant for automated R programs
    #'   that invoke `crew` controllers.
    #' @param seed Integer of length 1 with the pseudo-random number generator
    #'   seed to set for the evaluation of the task.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param seconds_timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param scale Logical, whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #'   By design, auto-scaling might not actually happen
    #'   if `throttle = TRUE`.
    #' @param throttle If `scale` is `TRUE`, whether to defer auto-scaling
    #'   until the next request at least
    #'   `self$client$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param name Optional name of the task.
    #' @param save_command Logical of length 1. If `TRUE`, the controller
    #'   deparses the command and returns it with the output on `pop()`.
    #'   If `FALSE` (default), the controller skips this step to
    #'   increase speed.
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    push = function(
      command,
      data = list(),
      globals = list(),
      substitute = TRUE,
      seed = as.integer(nanonext::random() / 2),
      packages = character(0),
      library = NULL,
      seconds_timeout = NULL,
      scale = TRUE,
      throttle = TRUE,
      name = NA_character_,
      save_command = FALSE,
      controller = NULL
    ) {
      .subset2(self, "start")()
      if (substitute) {
        command <- substitute(command)
      }
      if (save_command) {
        string <- deparse_safe(command)
      } else {
        string <- NA_character_
      }
      if (is.null(seconds_timeout)) {
        .timeout <- NULL
      } else {
        .timeout <- seconds_timeout * 1000
      }
      task <- mirai::mirai(
        .expr = expr_crew_eval,
        name = name,
        command = command,
        string = string,
        data = data,
        globals = globals,
        seed = seed,
        packages = packages,
        library = library,
        .timeout = .timeout,
        .compute = self$client$name
      )
      .subset2(.subset2(self, "schedule"), "push")(task = task)
      if (scale) {
        .subset2(self, "scale")(throttle = throttle)
      }
      invisible()
    },
    #' @description Quickly push a task to the head of the task list.
    #' @details Exists to support extensions to `crew` for `purrr`-like
    #'   functional programming. For developers only and not supported for
    #'   controller groups. Relative to `push()`, `shove()` skips user
    #'   options and guardrails for to aggressively optimize performance.
    #' @return `NULL` (invisibly).
    #' @param command Language object with R code to run.
    #' @param data Named list of local data objects in the
    #'   evaluation environment.
    #' @param globals Named list of objects to temporarily assign to the
    #'   global environment for the task.
    #'   This list should
    #'   include any functions you previously defined in the global
    #'   environment which are required to run tasks.
    #'   See the `reset_globals` argument
    #'   of [crew_controller_local()].
    #' @param seed Integer of length 1 with the pseudo-random number generator
    #'   seed to set for the evaluation of the task.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param .timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param scale Logical, whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #'   By design, auto-scaling might not actually happen
    #'   if `throttle = TRUE`.
    #' @param throttle If `scale` is `TRUE`, whether to defer auto-scaling
    #'   until the next request at least
    #'   `self$client$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param name Optional name of the task.
    #' @param string Optional character string with the deparsed command.
    shove = function(
      command,
      data = list(),
      globals = list(),
      seed = 0L,
      packages = character(0),
      library = NULL,
      .timeout = NULL,
      name = NA_character_,
      string = NA_character_
    ) {
      task <- mirai::mirai(
        .expr = expr_crew_eval,
        name = name,
        command = command,
        string = string,
        data = data,
        globals = globals,
        seed = seed,
        packages = packages,
        library = library,
        .timeout = .timeout,
        .compute = self$client$name
      )
      .subset2(.subset2(self, "schedule"), "push")(task = task)
      invisible()
    },
    #' @description Apply a single command to multiple inputs.
    #' @details The idea comes from functional programming: for example,
    #'   the `map()` function from the `purrr` package.
    #'   The controller must be started and empty before calling `map()`.
    #' @return A `tibble` of results and metadata, like the output of `pop()`
    #'   but with multiple rows aggregated together (one row per task).
    #' @param command Language object with R code to run.
    #' @param iterate Named list of vectors or lists to iterate over.
    #'   For example, to run function calls
    #'   `f(x = 1, y = "a")` and `f(x = 2, y = "b")`,
    #'   set `command` to `f(x, y)`, and set `iterate` to
    #'   `list(x = c(1, 2), y = c("a", "b"))`. The individual
    #'   function calls are evaluated as
    #'   `f(x = iterate$x[[1]], y = iterate$y[[1]])` and
    #'   `f(x = iterate$x[[2]], y = iterate$y[[2]])`.
    #'   All the elements of `iterate` must have the same length.
    #'   If there are any name conflicts between `iterate` and `data`,
    #'   `iterate` takes precedence.
    #' @param data Named list of constant local data objects in the
    #'   evaluation environment. Objects in this list are treated as single
    #'   values and are held constant for each iteration of the map.
    #' @param globals Named list of constant objects to temporarily
    #'   assign to the global environment for each task. This list should
    #'   include any functions you previously defined in the global
    #'   environment which are required to run tasks.
    #'   See the `reset_globals` argument of [crew_controller_local()].
    #'   Objects in this list are treated as single
    #'   values and are held constant for each iteration of the map.
    #' @param substitute Logical of length 1, whether to call
    #'   `base::substitute()` on the supplied value of the
    #'   `command` argument. If `TRUE` (default) then `command` is quoted
    #'   literally as you write it, e.g.
    #'   `push(command = your_function_call())`. If `FALSE`, then `crew`
    #'   assumes `command` is a language object and you are passing its
    #'   value, e.g. `push(command = quote(your_function_call()))`.
    #'   `substitute = TRUE` is appropriate for interactive use,
    #'   whereas `substitute = FALSE` is meant for automated R programs
    #'   that invoke `crew` controllers.
    #' @param seed Integer of length 1 with a pseudo-random number generator
    #'   seed. Task-specific task seeds are non-randomly derived
    #'   from this seed.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param seconds_interval Number of seconds to wait between intervals
    #'   polling the tasks for completion. Defaults to the `seconds_interval`
    #'   field in the client object
    #' @param seconds_timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param names Optional character of length 1, name of the element of
    #'   `iterate` with names for the tasks. If `names` is supplied,
    #'   then `iterate[[names]]` must be a character vector.
    #' @param save_command Logical of length 1, whether to store
    #'   a text string version of the R command in the output.
    #' @param error Character vector of length 1, choice of action if
    #'   a task has an error. Possible values:
    #'   * `"stop"`: throw an error in the main R session instead of returning
    #'     a value. In case of an error, the results from the last errored
    #'     `map()` are in the `error` field
    #'     of the controller, e.g. `controller_object$error`. To reduce
    #'     memory consumption, set `controller_object$error <- NULL` after
    #'     you are finished troubleshooting.
    #'   * `"warn"`: throw a warning. This allows the return value with
    #'     all the error messages and tracebacks to be generated.
    #'   * `"silent"`: do nothing special.
    #' @param verbose Logical of length 1, whether to print progress messages.
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    map = function(
      command,
      iterate,
      data = list(),
      globals = list(),
      substitute = TRUE,
      seed = as.integer(nanonext::random() / 2),
      packages = character(0),
      library = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      names = NULL,
      save_command = FALSE,
      error = "stop",
      verbose = interactive(),
      controller = NULL
    ) {
      crew_assert(substitute, isTRUE(.) || isFALSE(.))
      seconds_interval <- seconds_interval %|||% self$client$seconds_interval
      if (substitute) {
        command <- substitute(command)
      }
      crew_assert(save_command, isTRUE(.) || isFALSE(.))
      crew_assert(
        iterate,
        is.list(.),
        rlang::is_named(.),
        message = "the \"iterate\" argument of map() must be a named list"
      )
      crew_assert(
        length(iterate) > 0L,
        message = "the \"iterate\" argument must be a nonempty named list"
      )
      crew_assert(
        length(unique(map_dbl(iterate, length))) == 1L,
        message = "all elements of \"iterate\" must have the same length"
      )
      crew_assert(
        length(iterate[[1L]]) > 0L,
        message = "all elements of \"iterate\" must be nonempty"
      )
      crew_assert(
        data,
        is.list(.),
        rlang::is_named(.) || length(.) < 1L,
        message = "the \"data\" argument of map() must be a named list"
      )
      crew_assert(
        globals,
        is.list(.),
        rlang::is_named(.) || length(.) < 1L,
        message = "the \"globals\" argument of map() must be a named list"
      )
      crew_assert(
        seed,
        is.integer(.),
        length(.) == 1L,
        !anyNA(seed),
        message = "seed must be an integer of length 1"
      )
      crew_assert(
        packages,
        is.character(.),
        !anyNA(.),
        message = "packages must be a character vector with no element missing"
      )
      crew_assert(
        library %|||% "local",
        is.character(.),
        !anyNA(.),
        message = "library must be a NULL or a non-missing character vector"
      )
      crew_assert(
        seconds_interval,
        is.numeric(.),
        length(.) == 1L,
        !anyNA(.),
        . >= 0,
        message = "seconds_interval must be a nonnegative real number"
      )
      crew_assert(
        seconds_timeout %|||% 0,
        is.numeric(.),
        length(.) == 1L,
        !anyNA(.),
        . >= 0,
        message = "seconds_timeout must be NULL or a nonnegative real number"
      )
      crew_assert(
        names %|||% names(iterate)[[1L]],
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.),
        . %in% names(iterate),
        message = "names argument must be NULL or an element of names(iterate)"
      )
      crew_assert(
        error %in% c("stop", "warn", "silent"),
        message = "error argument must be \"stop\", \"warn\", or \"silent\""
      )
      string <- if_any(save_command, deparse_safe(command), NA_character_)
      .timeout <- if_any(
        is.null(seconds_timeout),
        NULL,
        seconds_timeout * 1000
      )
      names <- if_any(
        is.null(names),
        as.character(seq_along(iterate[[1L]])),
        iterate[[names]]
      )
      crew_assert(
        anyDuplicated(names) < 1L,
        message = "task names in map() must not have duplicates"
      )
      names_iterate <- names(iterate)
      self$start()
      old_schedule <- self$schedule
      self$schedule <- crew_schedule(
        seconds_interval = old_schedule$seconds_interval
      )
      self$schedule$start()
      for (index in seq_along(names)) {
        for (name in names_iterate) {
          data[[name]] <- .subset2(.subset2(iterate, name), index)
        }
        .subset2(self, "shove")(
          command = command,
          string = string,
          data = data,
          globals = globals,
          seed = seed - (as.integer(sign(seed)) * index),
          packages = packages,
          library = library,
          .timeout = .timeout,
          name = .subset(names, index)
        )
      }
      schedule <- self$schedule
      start <- nanonext::mclock()
      crew_retry(
        fun = ~{
          .subset2(self, "scale")(throttle = FALSE)
          summary <- .subset2(schedule, "summary")()
          pushed <- .subset2(summary, "pushed")
          collected <- .subset2(summary, "collected")
          controller_map_message_progress(pushed, collected, verbose)
          pushed < 1L
        },
        seconds_interval = seconds_interval,
        seconds_timeout = Inf
      )
      controller_map_message_complete(length(names), start, verbose)
      if_any(verbose, message(), NULL)
      results <- self$schedule$list()
      self$schedule <- old_schedule
      out <- lapply(results, monad_tibble)
      out <- tibble::new_tibble(data.table::rbindlist(out))
      out <- out[match(x = names, table = out$name),, drop = FALSE] # nolint
      out <- out[!is.na(out$name),, drop = FALSE] # nolint
      worker <- .subset2(out, "worker")
      tasks <- table(worker)
      seconds <- tapply(
        X = .subset2(out, "seconds"),
        INDEX = worker,
        FUN = sum
      )
      errors <- tapply(
        X = as.integer(!is.na(.subset2(out, "error"))),
        INDEX = worker,
        FUN = sum
      )
      warnings <- tapply(
        X = as.integer(!is.na(.subset2(out, "warnings"))),
        INDEX = worker,
        FUN = sum
      )
      index <- as.integer(names(tasks))
      log <- .subset2(self, "log")
      self$log$tasks[index] <- .subset2(log, "tasks")[index] + tasks
      self$log$seconds[index] <- .subset2(log, "seconds")[index] + seconds
      self$log$errors[index] <- .subset2(log, "errors")[index] + errors
      self$log$warnings[index] <- .subset2(log, "warnings")[index] + warnings
      error_messages <- .subset2(out, "error")
      if (!all(is.na(error_messages)) && !identical(error, "silent")) {
        message <- sprintf(
          "%s tasks encountered errors. First error message: \"%s\".",
          sum(!is.na(error_messages)),
          error_messages[min(which(!is.na(error_messages)))]
        )
        if (identical(error, "stop")) {
          self$error <- out
          message <- paste(
            message,
            "\nSee the \"error\" field of your controller object",
            "for all results, including warnings, tracebacks, all",
            "error messages, etc."
          )
          crew_error(message)
        } else {
          crew_warning(message)
        }
      }
      out
    },
    #' @description Check for done tasks and move the results to
    #'   the results list.
    #' @return `NULL` (invisibly). Removes elements from the `queue`
    #'   list as applicable and moves them to the `results` list.
    #' @param throttle whether to defer task collection
    #'   until the next task collection request at least
    #'   `seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    collect = function(throttle = FALSE, controllers = NULL) {
      .subset2(.subset2(self, "schedule"), "collect")(throttle = throttle)
    },
    #' @description Pop a completed task from the results data frame.
    #' @details If not task is currently completed and collected, `pop()`
    #'   will attempt to auto-scale workers as needed and collect
    #'   any newly completed results.
    #' @return If there is a completed task available to collect, the return
    #'   value is a one-row data frame with the results, warnings, and errors.
    #'   Otherwise, if there are no results available to collect,
    #'   the return value is `NULL`.
    #' @param scale Logical of length 1,
    #'   whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #'   Auto-scaling might not actually happen if `throttle` is `TRUE`.
    #'   Scaling up on `pop()` may be important
    #'   for transient or nearly transient workers that tend to drop off
    #'   quickly after doing little work.
    #' @param collect Logical of length 1,
    #'   whether to collect the results of completed tasks.
    #' @param throttle Whether to defer auto-scaling and task collection
    #'   until the next request at least
    #'   `self$client$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    pop = function(
      scale = TRUE,
      collect = TRUE,
      throttle = TRUE,
      controllers = NULL
    ) {
      if (collect) {
        .subset2(.subset2(self, "schedule"), "collect")(throttle = throttle)
      }
      if (scale) {
        .subset2(self, "scale")(throttle = throttle)
      }
      task <- .subset2(.subset2(self, "schedule"), "pop")()
      if (is.null(task)) {
        return(NULL)
      }
      out <- task$data
      # The contents of the if() statement below happen
      # if mirai cannot evaluate the command.
      # I cannot cover this in automated tests, but
      # I did test it by hand.
      # nocov start
      if (!is.list(out)) {
        out <- monad_init(
          error = paste(
            utils::capture.output(print(out), type = "output"),
            collapse = "\n"
          )
        )
      }
      # nocov end
      out <- monad_tibble(out)
      log <- .subset2(self, "log")
      # Same as above.
      # nocov start
      if (anyNA(.subset2(out, "launcher"))) {
        return(out)
      }
      # nocov end
      index <- .subset2(out, "worker")
      self$log$tasks[index] <- .subset2(log, "tasks")[index] + 1L
      self$log$seconds[index] <- .subset2(log, "seconds")[index] +
        .subset2(out, "seconds")
      self$log$errors[index] <- .subset2(log, "errors")[index] +
        !anyNA(.subset2(out, "error"))
      self$log$warnings[index] <- .subset2(log, "warnings")[index] +
        !anyNA(.subset2(out, "warnings"))
      out
    },
    #' @description Wait for tasks.
    #' @details The `wait()` method blocks the calling R session,
    #'   repeatedly auto-scales workers for tasks
    #'   that need them, and repeatedly collects results.
    #'   The function runs until it either times out or reaches
    #'   its stopping condition based on the `mode` argument.
    #' @return `NULL` (invisibly). Call `pop()` to get the result.
    #' @param mode If `mode` is `"all"`,
    #'   then the method waits for all tasks to complete. If `mode` is
    #'   `"one"`, then it waits until a one task is complete.
    #' @param seconds_interval Number of seconds to wait between polling
    #'   intervals waiting for tasks. Defaults to the `seconds_interval`
    #'   field of the client object.
    #' @param seconds_timeout Timeout length in seconds waiting for tasks.
    #' @param scale Logical, whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #'   Might not actually auto-scale on every iteration if `throttle`
    #'   is `TRUE`.
    #' @param throttle Whether to defer auto-scaling
    #'   and task collection until the next request at least
    #'   `self$client$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #'   Highly recommended
    #'   to keep `throttle = TRUE` in `wait()` to ensure efficiency
    #'   and robustness.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    wait = function(
      mode = "all",
      seconds_interval = NULL,
      seconds_timeout = Inf,
      scale = TRUE,
      throttle = TRUE,
      controllers = NULL
    ) {
      mode <- as.character(mode)
      crew_assert(mode, identical(., "all") || identical(., "one"))
      seconds_interval <- seconds_interval %|||% self$client$seconds_interval
      tryCatch(
        crew_retry(
          fun = ~{
            self$schedule$collect(throttle = throttle)
            if (scale) {
              self$scale(throttle = throttle)
            }
            self$schedule$collected_mode(mode = mode)
          },
          seconds_interval = seconds_interval,
          seconds_timeout = seconds_timeout
        ),
        crew_expire = function(condition) NULL
      )
      invisible()
    },
    #' @description Summarize the workers and tasks of the controller.
    #' @return A data frame of summary statistics on the workers and tasks.
    #'   It has one row per worker websocket and the following columns:
    #'   * `controller`: name of the controller.
    #'.  * `worker`: integer index of the worker.
    #'   * `tasks`: number of tasks which were completed by
    #'     a worker at the websocket and then returned by calling
    #'     `pop()` on the controller object.
    #'   * `seconds`: total number of runtime and seconds of
    #'     all the tasks that ran on a worker connected to this websocket
    #'     and then were retrieved by calling `pop()` on the controller
    #'     object.
    #'   * `errors`: total number of tasks which ran on a worker
    #'     at the website, encountered an error in R, and then retrieved
    #'     with `pop()`.
    #'   * `warnings`: total number of tasks which ran on a worker
    #'     at the website, encountered one or more warnings in R,
    #'     and then retrieved with `pop()`. Note: `warnings`
    #'     is actually the number of *tasks*, not the number of warnings.
    #'     (A task could throw more than one warning.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    summary = function(controllers = NULL) {
      out <- .subset2(self, "log")
      if (!is.null(out)) {
        out <- tibble::new_tibble(out)
      }
      out
    },
    #' @description Terminate the workers and the `mirai` client.
    #' @return `NULL` (invisibly).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    terminate = function(controllers = NULL) {
      # https://github.com/r-lib/covr/issues/315 # nolint
      terminate_launcher_first <- identical(Sys.getenv("R_COVR"), "true") ||
        identical(tolower(Sys.info()[["sysname"]]), "windows")
      # nocov start
      if (terminate_launcher_first) {
        self$launcher$terminate()
        self$client$terminate()
      } else {
        self$client$terminate()
        self$launcher$terminate()
      }
      # nocov end
      invisible()
    }
  )
)

expr_crew_eval <- quote(
  crew::crew_eval(
    name = name,
    command = command,
    string = string,
    data = data,
    globals = globals,
    seed = seed,
    packages = packages,
    library = library
  )
)

controller_map_message_progress <- function(pushed, collected, verbose) {
  if (!verbose) {
    return()
  }
  symbol <- sample(c("-", "\\", "|", "/"), size = 1L)
  total <- collected + pushed
  text <- sprintf(
    "\r%s of %s tasks done (%s%%) %s",
    collected,
    total,
    round(100 * collected / total),
    symbol
  )
  message(text, appendLF = FALSE)
}

controller_map_message_complete <- function(tasks, start, verbose) {
  if (!verbose) {
    return()
  }
  seconds <- (nanonext::mclock() - start) / 1000
  time <- units_time(seconds)
  text <- sprintf("\r%s tasks done in %s.", tasks, time)
  message(text)
}
