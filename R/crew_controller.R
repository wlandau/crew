#' @title Create a controller object from a client and launcher.
#' @export
#' @family controller
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
  crew_deprecate(
    name = "auto_scale",
    date = "2023-05-18",
    version = "0.2.0",
    alternative = "use the scale argument of push(), pop(), and wait()",
    value = auto_scale,
    frequency = "once"
  )
  controller <- crew_class_controller$new(client = client, launcher = launcher)
  controller$launcher$set_name(controller$client$name)
  controller$validate()
  controller
}

#' @title Controller class
#' @export
#' @family controller
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
  private = list(
    .client = NULL,
    .launcher = NULL,
    .tasks = NULL,
    .pushed = NULL,
    .popped = NULL,
    .log = NULL,
    .error = NULL,
    .shove = function(
      command,
      data = list(),
      globals = list(),
      seed = NULL,
      algorithm = NULL,
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
        algorithm = algorithm,
        packages = packages,
        library = library,
        .timeout = .timeout,
        .compute = private$.client$name
      )
      on.exit({
        private$.tasks[[length(.subset2(self, "tasks")) + 1L]] <- task
        private$.pushed <- .subset2(self, "pushed") + 1L
      })
      invisible()
    },
    .wait_all_once = function(seconds_interval) {
      if (self$unresolved() > 0L) {
        private$.client$relay$wait(seconds_timeout = seconds_interval)
      }
      self$unresolved() < 1L
    },
    .wait_one_once = function(seconds_interval) {
      if (self$unpopped() < 1L) {
        private$.client$relay$wait(seconds_timeout = seconds_interval)
      }
      self$unpopped() > 0L
    }
  ),
  active = list(
    #' @field client Router object.
    client = function() {
      .subset2(private, ".client")
    },
    #' @field launcher Launcher object.
    launcher = function() {
      .subset2(private, ".launcher")
    },
    #' @field tasks A list of `mirai::mirai()` task objects.
    tasks = function() {
      .subset2(private, ".tasks")
    },
    #' @field pushed Number of tasks pushed since the controller was started.
    pushed = function() {
      .subset2(private, ".pushed")
    },
    #' @field popped Number of tasks popped
    #' since the controller was started.
    popped = function() {
      .subset2(private, ".popped")
    },
    #' @field log Tibble with per-worker metadata about tasks.
    log = function() {
      .subset2(private, ".log")
    },
    #' @field error Tibble of task results (with one result per row)
    #'   from the last call to `map(error = "stop)`.
    error = function() {
      .subset2(private, ".error")
    }
  ),
  public = list(
    #' @description `mirai` controller constructor.
    #' @return An `R6` controller object.
    #' @param client Router object. See [crew_controller()].
    #' @param launcher Launcher object. See [crew_controller()].
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
      launcher = NULL
    ) {
      private$.client <- client
      private$.launcher <- launcher
      invisible()
    },
    #' @description Validate the client.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(inherits(private$.client, "crew_class_client"))
      crew_assert(inherits(private$.launcher, "crew_class_launcher"))
      private$.client$validate()
      private$.launcher$validate()
      crew_assert(private$.tasks, is.null(.) || is.list(.))
      crew_assert(
        identical(private$.client$name, private$.launcher$name),
        message = "client and launcher must have the same name"
      )
      crew_assert(private$.log, is.null(.) || is.list(.))
      invisible()
    },
    #' @description Check if the controller is empty.
    #' @details A controller is empty if it has no running tasks
    #'   or completed tasks waiting to be retrieved with `push()`.
    #' @return `TRUE` if the controller is empty, `FALSE` otherwise.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    empty = function(controllers = NULL) {
      length(.subset2(self, "tasks")) < 1L
    },
    #' @description Check if the controller is nonempty.
    #' @details A controller is empty if it has no running tasks
    #'   or completed tasks waiting to be retrieved with `push()`.
    #' @return `TRUE` if the controller is empty, `FALSE` otherwise.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    nonempty = function(controllers = NULL) {
      length(.subset2(self, "tasks")) > 0L
    },
    #' @description Number of resolved `mirai()` tasks.
    #' @details `resolved()` is cumulative: it counts all the resolved
    #'   tasks over the entire lifetime of the controller session.
    #' @return Non-negative integer of length 1,
    #'   number of resolved `mirai()` tasks.
    #'   The return value is 0 if the condition variable does not exist
    #'   (i.e. if the client is not running).
    resolved = function() {
      .subset2(.subset2(self, "client"), "resolved")()
    },
    #' @description Number of unresolved `mirai()` tasks.
    #' @return Non-negative integer of length 1,
    #'   number of unresolved `mirai()` tasks.
    unresolved = function() {
      .subset2(self, "pushed") - .subset2(self, "resolved")()
    },
    #' @description Number of resolved `mirai()` tasks available via `pop()`.
    #' @return Non-negative integer of length 1,
    #'   number of resolved `mirai()` tasks available via `pop()`.
    unpopped = function() {
      .subset2(self, "resolved")() - .subset2(self, "popped")
    },
    #' @description Check if the controller is saturated.
    #' @details A controller is saturated if the number of unresolved tasks
    #'   is greater than or equal to the maximum number of workers.
    #'   In other words, in a saturated controller, every available worker
    #'   has a task.
    #'   You can still push tasks to a saturated controller, but
    #'   tools that use `crew` such as `targets` may choose not to.
    #' @return `TRUE` if the controller is saturated, `FALSE` otherwise.
    #' @param collect Deprecated in version 0.5.0.9003 (2023-10-02). Not used.
    #' @param throttle Deprecated in version 0.5.0.9003 (2023-10-02). Not used.
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    saturated = function(collect = NULL, throttle = NULL, controller = NULL) {
      crew_deprecate(
        name = "collect",
        date = "2023-10-02",
        version = "0.5.0.9003",
        alternative = "none (no longer necessary)",
        condition = "message",
        value = collect,
        skip_cran = TRUE,
        frequency = "once"
      )
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
      .subset2(self, "unresolved")() >=
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
        private$.client$start()
        workers <- private$.client$workers
        private$.launcher$start()
        private$.tasks <- list()
        private$.pushed <- 0L
        private$.popped <- 0L
        private$.log <- list(
          controller = rep(private$.client$name, workers),
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
      private$.launcher$tally()
      private$.launcher$rotate()
      walk(
        x = private$.launcher$unlaunched(n = n),
        f = private$.launcher$launch
      )
      invisible()
    },
    #' @description Auto-scale workers out to meet the demand of tasks.
    #' @details Methods `push()`, `pop()`, and `wait()` already invoke
    #'   `scale()` if the `scale` argument is `TRUE`.
    #'   For finer control of the number of workers launched,
    #'   call `launch()` on the controller with the exact desired
    #'   number of workers.
    #' @return `NULL` (invisibly).
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    scale = function(throttle = TRUE, controllers = NULL) {
      private$.launcher$scale(demand = self$unresolved(), throttle = throttle)
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
    #'   seed to set for the evaluation of the task. Passed to the
    #'   `seed` argument of `set.seed()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param algorithm Integer of length 1 with the pseudo-random number
    #'   generator algorithm to set for the evaluation of the task.
    #'   Passed to the `kind` argument of `RNGkind()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   recommended widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param seconds_timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param scale Logical, whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load. Also
    #'   see the `throttle` argument.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
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
      seed = NULL,
      algorithm = NULL,
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
        algorithm = algorithm,
        packages = packages,
        library = library,
        .timeout = .timeout,
        .compute = private$.client$name
      )
      on.exit({
        private$.tasks[[length(.subset2(self, "tasks")) + 1L]] <- task
        private$.pushed <- .subset2(self, "pushed") + 1L
      })
      if (scale) {
        .subset2(self, "scale")(throttle = throttle)
      }
      invisible()
    },
    #' @description Apply a single command to multiple inputs.
    #' @details `map()` cannot be used unless all prior tasks are
    #'   completed and popped. You may need to wait and then pop them
    #'   manually. Alternatively, you can start over: either call
    #'   `terminate()` on the current controller object to reset it, or
    #'   create a new controller object entirely.
    #' @return A `tibble` of results and metadata: one row per task
    #'   and columns corresponding to the output of `pop()`.
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
    #' @param seed Integer of length 1 with the pseudo-random number generator
    #'   seed to set for the evaluation of the task. Passed to the
    #'   `seed` argument of `set.seed()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   recommended widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param algorithm Integer of length 1 with the pseudo-random number
    #'   generator algorithm to set for the evaluation of the task.
    #'   Passed to the `kind` argument of `RNGkind()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   recommended widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param seconds_interval Number of seconds to wait between intervals
    #'   polling the tasks for completion.
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
    #' @param warnings Logical of length 1, whether to throw a warning in the
    #'   interactive session if at least one task encounters an error.
    #' @param verbose Logical of length 1, whether to print progress messages.
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. See also the `throttle` argument.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    map = function(
      command,
      iterate,
      data = list(),
      globals = list(),
      substitute = TRUE,
      seed = NULL,
      algorithm = NULL,
      packages = character(0),
      library = NULL,
      seconds_interval = 0.5,
      seconds_timeout = NULL,
      names = NULL,
      save_command = FALSE,
      error = "stop",
      warnings = TRUE,
      verbose = interactive(),
      scale = TRUE,
      throttle = TRUE,
      controller = NULL
    ) {
      crew_assert(substitute, isTRUE(.) || isFALSE(.))
      if (substitute) {
        command <- substitute(command)
      }
      crew_assert(save_command, isTRUE(.) || isFALSE(.))
      crew_assert(
        iterate,
        is.list(.),
        rlang::is_named(.),
        message = "the 'iterate' arg of map() must be a nonempty named list"
      )
      crew_assert(
        length(iterate) > 0L,
        message = "the \"iterate\" arg of map() must be a nonempty named list"
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
        seed %|||% 1L,
        is.integer(.),
        length(.) == 1L,
        !anyNA(seed),
        message = "seed must be an integer of length 1"
      )
      crew_assert(
        algorithm %|||% "Mersenne-Twister",
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.),
        message = "algorithm must be a valid RNG algorithm name"
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
        warnings,
        isTRUE(.) || isFALSE(.),
        message = "'warn' argument must be TRUE or FALSE."
      )
      crew_assert(
        error %in% c("stop", "warn", "silent"),
        message = "'error' argument must be \"stop\", \"warn\", or \"silent\""
      )
      crew_assert(
        length(private$.tasks) < 1L,
        message = "cannot map() until all prior tasks are completed and popped"
      )
      crew_assert(scale, isTRUE(.) || isFALSE(.))
      crew_assert(throttle, isTRUE(.) || isFALSE(.))
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
      sign <- if_any(!is.null(seed) && seed > 0L, 1L, -1L)
      if (!is.null(seed)) {
        seed <- 1L
      }
      for (index in seq_along(names)) {
        for (name in names_iterate) {
          data[[name]] <- .subset2(.subset2(iterate, name), index)
        }
        if (is.null(seed)) {
          task_seed <- NULL
        } else {
          task_seed <- seed - (sign * index)
        }
        .subset2(private, ".shove")(
          command = command,
          string = string,
          data = data,
          globals = globals,
          seed = task_seed,
          algorithm = algorithm,
          packages = packages,
          library = library,
          .timeout = .timeout,
          name = .subset(names, index)
        )
      }
      tasks <- private$.tasks
      total <- length(tasks)
      relay <- .subset2(.subset2(self, "client"), "relay")
      start <- nanonext::mclock()
      pushed <- private$.pushed
      this_envir <- environment()
      progress_envir <- new.env(parent = this_envir)
      if (verbose) {
        cli::cli_progress_bar(
          total = total,
          type = "custom",
          format = paste(
            "{cli::pb_current}/{cli::pb_total}",
            "{cli::pb_bar}",
            "{cli::pb_percent}"
          ),
          format_done = "{cli::pb_total} tasks in {cli::pb_elapsed}",
          clear = FALSE,
          .envir = progress_envir
        )
      }
      crew_retry(
        fun = ~{
          if (scale) {
            .subset2(self, "scale")(throttle = throttle)
          }
          unresolved <- .subset2(self, "unresolved")()
          if (verbose) {
            cli::cli_progress_update(
              set = total - unresolved,
              .envir = progress_envir
            )
          }
          if (unresolved > 0L) {
            .subset2(relay, "wait")(seconds_timeout = seconds_interval)
          }
          .subset2(self, "unresolved")() < 1L
        },
        seconds_interval = 0,
        seconds_timeout = Inf,
        error = FALSE
      )
      if (verbose) {
        cli::cli_progress_done(.envir = progress_envir)
      }
      results <- map(tasks, ~.subset2(.x, "data"))
      out <- lapply(results, monad_tibble)
      out <- tibble::new_tibble(data.table::rbindlist(out, use.names = FALSE))
      out <- out[match(x = names, table = out$name),, drop = FALSE] # nolint
      out <- out[!is.na(out$name),, drop = FALSE] # nolint
      worker <- .subset2(out, "worker")
      tasks <- table(worker)
      seconds <- tapply(
        X = .subset2(out, "seconds"),
        INDEX = worker,
        FUN = sum
      )
      summary_errors <- tapply(
        X = as.integer(!is.na(.subset2(out, "error"))),
        INDEX = worker,
        FUN = sum
      )
      summary_warnings <- tapply(
        X = as.integer(!is.na(.subset2(out, "warnings"))),
        INDEX = worker,
        FUN = sum
      )
      index <- as.integer(names(tasks))
      log <- .subset2(self, "log")
      on.exit({
        private$.tasks <- list()
        private$.popped <- .subset2(self, "popped") + total
        private$.log$tasks[index] <- .subset2(log, "tasks")[index] +
          tasks
        private$.log$seconds[index] <- .subset2(log, "seconds")[index] +
          seconds
        private$.log$errors[index] <- .subset2(log, "errors")[index] +
          summary_errors
        private$.log$warnings[index] <- .subset2(log, "warnings")[index] +
          summary_warnings
      })
      warning_messages <- .subset2(out, "warnings")
      if (!all(is.na(warning_messages)) && isTRUE(warnings)) {
        message <- sprintf(
          paste(
            "%s tasks encountered warnings.",
            "Warning messages of first such task: \"%s\"."
          ),
          sum(!is.na(warning_messages)),
          warning_messages[min(which(!is.na(warning_messages)))]
        )
        crew_warning(message)
      }
      error_messages <- .subset2(out, "error")
      if (!all(is.na(error_messages)) && !identical(error, "silent")) {
        message <- sprintf(
          "%s tasks encountered errors. First error message: \"%s\".",
          sum(!is.na(error_messages)),
          error_messages[min(which(!is.na(error_messages)))]
        )
        if (identical(error, "stop")) {
          private$.error <- out
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
    #' @description Deprecated in version 0.5.0.9003 (2023-10-02).
    #' @return `NULL`.
    #' @param throttle Deprecated in version 0.5.0.9003 (2023-10-02).
    #' @param controllers Deprecated in version 0.5.0.9003 (2023-10-02).
    collect = function(throttle = NULL, controllers = NULL) {
      crew_deprecate(
        name = "collect()",
        date = "2023-10-02",
        version = "0.5.0.9003",
        alternative = "none (no longer necessary)",
        condition = "message",
        value = "collect",
        skip_cran = TRUE,
        frequency = "once"
      )
    },
    #' @description Pop a completed task from the results data frame.
    #' @details If not task is currently completed, `pop()`
    #'   will attempt to auto-scale workers as needed.
    #' @return If there is no task to collect, return `NULL`. Otherwise,
    #'   return a one-row `tibble` with the following columns.
    #'   * `name`: the task name if given.
    #'   * `command`: a character string with the R command if `save_command`
    #'     was set to `TRUE` in `push()`.
    #'   * `result`: a list containing the return value of the R command.
    #'   * `seconds`: number of seconds that the task ran.
    #'   * `seed`: the single integer originally supplied to `push()`,
    #'      `NA` otherwise. The pseudo-random number generator state
    #'      just prior to the task can be restored using
    #'      `set.seed(seed = seed, kind = algorithm)`, where `seed` and
    #'      `algorithm` are part of this output.
    #'   * `algorithm`: name of the pseudo-random number generator algorithm
    #'      originally supplied to `push()`,
    #'      `NA` otherwise. The pseudo-random number generator state
    #'      just prior to the task can be restored using
    #'      `set.seed(seed = seed, kind = algorithm)`, where `seed` and
    #'      `algorithm` are part of this output.
    #'   * `error`: the first 2048 characters of the error message if
    #'     the task threw an error, `NA` otherwise.
    #'   * `trace`: the first 2048 characters of the text of the traceback
    #'     if the task threw an error, `NA` otherwise.
    #'   * `warnings`: the first 2048 characters. of the text of
    #'     warning messages that the task may have generated, `NA` otherwise.
    #'   * `launcher`: name of the `crew` launcher where the task ran.
    #' @param scale Logical of length 1,
    #'   whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #'   Scaling up on `pop()` may be important
    #'   for transient or nearly transient workers that tend to drop off
    #'   quickly after doing little work.
    #'   See also the `throttle` argument.
    #' @param collect Deprecated in version 0.5.0.9003 (2023-10-02).
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    pop = function(
      scale = TRUE,
      collect = NULL,
      throttle = TRUE,
      controllers = NULL
    ) {
      crew_deprecate(
        name = "collect",
        date = "2023-10-02",
        version = "0.5.0.9003",
        alternative = "none (no longer necessary)",
        condition = "message",
        value = collect,
        skip_cran = TRUE,
        frequency = "once"
      )
      if (scale) {
        .subset2(self, "scale")(throttle = throttle)
      }
      tasks <- .subset2(self, "tasks")
      n_tasks <- length(tasks)
      if (n_tasks < 1L) {
        return(NULL)
      }
      task <- NULL
      index_delete <- NULL
      for (index in seq(n_tasks)) {
        object <- .subset2(tasks, index)
        if (!nanonext::unresolved(object)) {
          task <- object
          index_delete <- index
          break
        }
      }
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
      on.exit({
        private$.tasks[[index_delete]] <- NULL
        private$.popped <- .subset2(self, "popped") + 1L
      })
      # nocov start
      if (anyNA(.subset2(out, "launcher"))) {
        return(out)
      }
      # nocov end
      on.exit({
        index <- .subset2(out, "worker")
        private$.log$tasks[index] <- .subset2(log, "tasks")[index] + 1L
        private$.log$seconds[index] <- .subset2(log, "seconds")[index] +
          .subset2(out, "seconds")
        private$.log$errors[index] <- .subset2(log, "errors")[index] +
          !anyNA(.subset2(out, "error"))
        private$.log$warnings[index] <- .subset2(log, "warnings")[index] +
          !anyNA(.subset2(out, "warnings"))
      }, add = TRUE)
      out
    },
    #' @description Wait for tasks.
    #' @details The `wait()` method blocks the calling R session and
    #'   repeatedly auto-scales workers for tasks that need them.
    #'   The function runs until it either times out or the condition
    #'   in `mode` is met.
    #' @return A logical of length 1, invisibly. `TRUE` if the condition
    #'   in `mode` was met, `FALSE` otherwise.
    #' @param mode Character of length 1: `"all"` to wait for all tasks to
    #'   complete, `"one"` to wait for a single task to complete.
    #' @param seconds_interval Number of seconds to interrupt the wait
    #'   in order to scale up workers as needed.
    #' @param seconds_timeout Timeout length in seconds waiting for tasks.
    #' @param scale Logical, whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #'   See also the `throttle` argument.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    wait = function(
      mode = "all",
      seconds_interval = 0.5,
      seconds_timeout = Inf,
      scale = TRUE,
      throttle = TRUE,
      controllers = NULL
    ) {
      crew_assert(mode, identical(., "all") || identical(., "one"))
      mode_all <- identical(mode, "all")
      if (length(private$.tasks) < 1L) {
        return(mode_all)
      }
      envir <- new.env(parent = emptyenv())
      envir$result <- FALSE
      crew_retry(
        fun = ~{
          if (!envir$result && scale) {
            self$scale(throttle = throttle)
          }
          envir$result <- if_any(
            mode_all,
            private$.wait_all_once(seconds_interval = seconds_interval),
            private$.wait_one_once(seconds_interval = seconds_interval)
          )
          envir$result
        },
        seconds_interval = 0,
        seconds_timeout = seconds_timeout,
        error = FALSE
      )
      invisible(envir$result)
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
      private$.client$terminate()
      private$.launcher$terminate()
      private$.tasks <- list()
      private$.pushed <- NULL
      private$.popped <- NULL
      invisible()
    }
  )
)
