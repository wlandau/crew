#' @title Create a controller object from a client and launcher.
#' @export
#' @family controller
#' @description This function is for developers of `crew` launcher plugins.
#'   Users should use a specific controller helper such as
#'   [crew_controller_local()].
#' @inheritParams crew_eval
#' @param client An `R6` client object created by [crew_client()].
#' @param launcher An `R6` launcher object created by one of the
#'   `crew_launcher_*()` functions such as [crew_launcher_local()].
#' @param crashes_max In rare cases, a worker may exit unexpectedly
#'   before it completes its current task. If this happens, `pop()`
#'   returns a status of `"crash"` instead of `"error"` for the task.
#'   The controller does not automatically retry the task, but
#'   you can retry it manually by calling `push()` again and using the same
#'   task name as before. (However, `targets` pipelines running `crew`
#'   do automatically retry tasks whose workers crashed.)
#'
#'   `crashes_max` is a non-negative integer, and it sets the maximum number of
#'   allowable consecutive crashes for a given task.
#'   If a task's worker crashes more than `crashes_max` times in a row,
#'   then `pop()` throws an error when it tries to return the results
#'   of the task.
#' @param backup An optional `crew` controller object, or `NULL` to omit.
#'   If supplied, the `backup` controller runs any pushed tasks that have
#'   already reached `crashes_max` consecutive crashes.
#'   Using `backup`, you can create
#'   a chain of controllers with different levels of resources
#'   (such as worker memory and CPUs) so that a task that fails on
#'   one controller can retry using incrementally more powerful workers.
#'   All controllers in a backup chain should be part of the same
#'   controller group (see [crew_controller_group()]) so you can call the
#'   group-level `pop()` and `collect()` methods to make sure you get results
#'   regardless of which controller actually ended up running the task.
#'
#'   Limitations of `backup`:
#'     * `crashes_max` needs to be positive in order for `backup` to be used.
#'       Otherwise, every task would always skip the current controller and
#'       go to `backup`.
#'     * `backup` cannot be a controller group. It must be an ordinary
#'       controller.
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
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  crashes_max = 5L,
  backup = NULL,
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
  controller <- crew_class_controller$new(
    client = client,
    launcher = launcher,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection,
    crashes_max = crashes_max,
    backup = backup
  )
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
  portable = FALSE,
  private = list(
    .client = NULL,
    .launcher = NULL,
    .tasks = collections::dict(),
    .reset_globals = NULL,
    .reset_packages = NULL,
    .reset_options = NULL,
    .garbage_collection = NULL,
    .crashes_max = NULL,
    .crash_log = collections::dict(),
    .backup = NULL,
    .summary = NULL,
    .error = NULL,
    .loop = NULL,
    .queue_resolved = NULL,
    .queue_backlog = NULL,
    .register_started = function() {
      .tasks <<- collections::dict()
      .crash_log <<- collections::dict()
      .summary <<- list(
        controller = .launcher$name,
        seconds = 0,
        success = 0L,
        error = 0L,
        crash = 0L,
        cancel = 0L,
        warning = 0L
      )
      .queue_backlog <<- collections::queue()
      .queue_resolved <<- collections::queue()
    },
    .name_new_task = function(name) {
      tasks_has <- .subset2(.tasks, "has")
      if (is.null(name)) {
        while (is.null(name) || tasks_has(name)) {
          name <- name_task_tempfile()
        }
      }
      if (tasks_has(name)) {
        crew_error(
          message = paste(
            "crew task name",
            name,
            "already found in the task list. Before pushing a task",
            "of the same name, please wait for it to resolve, then",
            "use pop() or collect() to remove it from the controller."
          )
        )
      }
      name
    },
    .resolve = function(force) {
      if (!(force || .subset2(.queue_resolved, "size")() < 1L)) {
        return()
      }
      keys <- .subset2(.tasks, "keys")()
      get <- .subset2(.tasks, "get")
      is_unresolved <- lapply(
        keys,
        function(key) nanonext::.unresolved(get(key))
      )
      resolved <- as.character(keys)[!as.logical(is_unresolved)]
      .queue_resolved <<- collections::queue(items = resolved)
    },
    .scan_crash = function(name, task) {
      code <- .subset2(task, "code")
      if (code != code_crash) {
        if (.subset2(.crash_log, "has")(name)) {
          .subset2(.crash_log, "remove")(name)
        }
        return()
      }
      previous <- .subset2(.crash_log, "get")(name, default = 0L)
      count <- previous + 1L
      .subset2(.crash_log, "set")(key = name, value = count)
      if (count > .crashes_max) {
        .summary$crash <<- .summary$crash + 1L
        crew_error(
          message = paste(
            "the crew worker of task",
            shQuote(name),
            "crashed",
            count,
            sprintf(
              "consecutive time(s) in controller %s.",
              shQuote(.launcher$name)
            ),
            "For details and advice, please see the",
            "crashes_max argument of crew::crew_controller(), as well as",
            "https://wlandau.github.io/crew/articles/risks.html#crashes",
            "and https://wlandau.github.io/crew/articles/logging.html."
          )
        )
      }
    }
  ),
  active = list(
    #' @field profile Character string, compute profile of the
    #'   controller.
    profile = function() {
      .subset2(.client, "profile")
    },
    #' @field client Client object.
    client = function() {
      .client
    },
    #' @field launcher Launcher object.
    launcher = function() {
      .launcher
    },
    #' @field tasks A list of `mirai::mirai()` task objects.
    #'   The list of tasks is dynamically generated from an internal,
    #'   dictionary, so it is not as fast as a simple lookup.
    tasks = function() {
      .subset2(.tasks, "as_list")()
    },
    #' @field reset_globals See [crew_controller()].
    #' since the controller was started.
    reset_globals = function() {
      .reset_globals
    },
    #' @field reset_packages See [crew_controller()].
    #' since the controller was started.
    reset_packages = function() {
      .reset_packages
    },
    #' @field reset_options See [crew_controller()].
    #' since the controller was started.
    reset_options = function() {
      .reset_options
    },
    #' @field garbage_collection See [crew_controller()].
    #' since the controller was started.
    garbage_collection = function() {
      .garbage_collection
    },
    #' @field crashes_max See [crew_controller()].
    crashes_max = function() {
      .crashes_max
    },
    #' @field backup See [crew_controller()].
    backup = function() {
      .backup
    },
    #' @field error Tibble of task results (with one result per row)
    #'   from the last call to `map(error = "stop)`.
    error = function() {
      .error
    },
    #' @field loop `later` loop if asynchronous auto-scaling is running,
    #'   `NULL` otherwise.
    loop = function() {
      .loop
    },
    #' @field queue_resolved Queue of resolved tasks.
    queue_resolved = function() {
      .queue_resolved
    },
    #' @field queue_backlog Queue of explicitly backlogged tasks.
    queue_backlog = function() {
      .queue_backlog
    }
  ),
  public = list(
    #' @description `mirai` controller constructor.
    #' @return An `R6` controller object.
    #' @param client Client object. See [crew_controller()].
    #' @param launcher Launcher object. See [crew_controller()].
    #' @param reset_globals See [crew_controller()].
    #' @param reset_packages See [crew_controller()].
    #' @param reset_options See [crew_controller()].
    #' @param garbage_collection See [crew_controller()].
    #' @param crashes_max See [crew_controller()].
    #' @param backup See [crew_controller()].
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
      reset_globals = NULL,
      reset_packages = NULL,
      reset_options = NULL,
      garbage_collection = NULL,
      crashes_max = NULL,
      backup = NULL
    ) {
      .client <<- client
      .launcher <<- launcher
      .reset_globals <<- reset_globals
      .reset_packages <<- reset_packages
      .reset_options <<- reset_options
      .garbage_collection <<- garbage_collection
      .crashes_max <<- crashes_max
      .backup <<- backup
      invisible()
    },
    #' @description Validate the controller.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(inherits(.client, "crew_class_client"))
      crew_assert(inherits(.launcher, "crew_class_launcher"))
      .client$validate()
      .launcher$validate()
      fields <- c(
        "reset_globals",
        "reset_packages",
        "reset_options",
        "garbage_collection"
      )
      for (field in fields) {
        crew_assert(
          self[[field]],
          isTRUE(.) || isFALSE(.),
          message = paste(field, "must be a non-missing logical of length 1")
        )
      }
      crew_assert(
        .crashes_max,
        is.numeric(.),
        length(.) == 1L,
        is.finite(.),
        . >= 0L,
        message = c(
          "crashes_max must be a finite non-negative integer scalar."
        )
      )
      if (!is.null(.crash_log)) {
        crew_assert(is.environment(.crash_log))
      }
      if (!is.null(.backup)) {
        crew_assert(
          .crashes_max > 0L,
          message = "crashes_max must be positive if backup is not NULL."
        )
        crew_assert(
          .backup,
          inherits(., "crew_class_controller"),
          !inherits(., "crew_class_controller_group"),
          message = paste(
            "backup must be NULL or a crew controller, and",
            "it must not be a controller group."
          )
        )
      }
      crew_assert(.tasks, is.null(.) || is.environment(.))
      crew_assert(.summary, is.null(.) || is.list(.))
      crew_assert(.loop, is.null(.) || inherits(., "event_loop"))
      if (!is.null(.queue_resolved)) {
        crew_assert(is.environment(.queue_resolved))
      }
      if (!is.null(.queue_backlog)) {
        crew_assert(is.environment(.queue_backlog))
      }
      invisible()
    },
    #' @description Number of tasks in the controller.
    #' @return Non-negative integer, number of tasks in the controller.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    size = function(controllers = NULL) {
      .subset2(.tasks, "size")()
    },
    #' @description Check if the controller is empty.
    #' @details A controller is empty if it has no [mirai::mirai()]
    #'   task objects in the controller.
    #'   There may still be other tasks running on the workers
    #'   of an empty controller, but those tasks were not submitted with
    #'   `push()` or `collect()`,
    #'   and they are not part of the controller task queue.
    #' @return `TRUE` if the controller is empty, `FALSE` otherwise.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    empty = function(controllers = NULL) {
      .subset2(.tasks, "size")() < 1L
    },
    #' @description Check if the controller is nonempty.
    #' @details A controller is empty if it has no [mirai::mirai()]
    #'   task objects in the controller.
    #'   There may still be other tasks running on the workers
    #'   of an empty controller, but those tasks were not submitted with
    #'   `push()` or `collect()`,
    #'   and they are not part of the controller task queue.
    #' @return `TRUE` if the controller is empty, `FALSE` otherwise.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    nonempty = function(controllers = NULL) {
      .subset2(.tasks, "size")() > 0L
    },
    #' @description Cumulative number of resolved tasks.
    #' @details `resolved()` is cumulative: it counts all the resolved
    #'   tasks over the entire lifetime of the controller session.
    #' @return Non-negative integer of length 1,
    #'   number of resolved tasks.
    #'   The return value is 0 if the condition variable does not exist
    #'   (i.e. if the client is not running).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    resolved = function(controllers = NULL) {
      if (!started()) {
        return(0L)
      }
      as.integer(.subset(.subset2(.client, "status")(), "completed"))
    },
    #' @description Number of unresolved tasks.
    #' @return Non-negative integer of length 1,
    #'   number of unresolved tasks.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    unresolved = function(controllers = NULL) {
      if (!started()) {
        return(0L)
      }
      counts <- .subset2(.client, "status")()
      as.integer(.subset(counts, "awaiting") + .subset2(counts, "executing"))
    },
    #' @description Check if the controller is saturated.
    #' @details A controller is saturated if the number of uncollected tasks
    #'   is greater than or equal to the maximum number of workers.
    #'   You can still push tasks to a saturated controller, but
    #'   tools that use `crew` such as `targets` may choose not to
    #'   (for performance and user-friendliness).
    #' @return `TRUE` if the controller is saturated, `FALSE` otherwise.
    #' @param collect Deprecated in version 0.5.0.9003 (2023-10-02). Not used.
    #' @param throttle Deprecated in version 0.5.0.9003 (2023-10-02). Not used.
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    saturated = function(collect = NULL, throttle = NULL, controller = NULL) {
      # TODO: when mirai gains a threaded dispatcher, reinstate the
      # commented 4 lines of code below and also follow the TODO
      # comments in pop_backlog().
      #
      # "Saturated" really should mean that the number of *unresolved* tasks
      # is equal to the maximum number of workers.
      # To avoid potentially burdensome dispatcher queries, "saturated"
      # for the time being means that the number of *uncollected* tasks
      # is equal to the maximum number of workers.
      # This shortcut avoids a bottleneck in {targets},
      # but it isn't exactly what "saturated" should mean.
      #
      # counts <- .subset2(.client, "status")()
      # unresolved <- .subset(counts, "awaiting") +
      #   .subset(counts, "executing")
      # as.logical(unresolved >= .subset2(.launcher, "workers"))
      #
      # Here is the shortcut crew currently uses to reduce overhead
      # from repeated calls to saturated():
      as.logical(size() >= .subset2(.launcher, "workers"))
    },
    #' @description Start the controller if it is not already started.
    #' @details Register the mirai client and register worker websockets
    #'   with the launcher.
    #' @return `NULL` (invisibly).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    start = function(controllers = NULL) {
      if (!.subset2(.client, "started")) {
        .client$start()
        .launcher$start(url = .client$url, profile = .client$profile)
        .register_started()
      }
      invisible()
    },
    #' @description Check whether the controller is started.
    #' @details Actually checks whether the client is started.
    #' @return `TRUE` if the controller is started, `FALSE` otherwise.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    started = function(controllers = NULL) {
      .subset2(.client, "started")
    },
    #' @description Launch one or more workers.
    #' @return `NULL` (invisibly).
    #' @param n Number of workers to launch.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    launch = function(n = 1L, controllers = NULL) {
      start()
      .launcher$launch(n = n)
      invisible()
    },
    #' @description Auto-scale workers out to meet the demand of tasks.
    #' @details The `scale()` method launches new workers to
    #'   run tasks if needed.
    #' @return Invisibly returns `TRUE` if auto-scaling was attempted
    #'   (throttling can skip it) and there was any relevant
    #'   auto-scaling activity (new worker launches or worker
    #'   connection/disconnection events). `FALSE` otherwise.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last polling interval. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    scale = function(throttle = TRUE, controllers = NULL) {
      if (throttle && !.launcher$poll()) {
        return(invisible(FALSE))
      }
      start()
      activity <- .launcher$scale(.client$status(), throttle)
      invisible(activity)
    },
    #' @description Run worker auto-scaling in a `later` loop
    #'   in polling intervals determined by exponential backoff.
    #' @details Call `controller$descale()` to terminate the
    #'   auto-scaling loop.
    #' @param loop A `later` loop to run auto-scaling.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    #' @return `NULL` (invisibly).
    autoscale = function(loop = later::current_loop(), controllers = NULL) {
      # Tested in tests/interactive/test-autoscale.R
      # nocov start
      crew_assert(
        inherits(loop, "event_loop"),
        message = "loop must be a valid later loop"
      )
      if (inherits(.loop, "event_loop")) {
        return(invisible())
      }
      poll <- function() {
        if (!is.null(.loop)) {
          self$scale(throttle = TRUE) # necessary reference to self
          later::later(
            func = poll,
            delay = .launcher$throttle$seconds_interval,
            loop = .loop
          )
        }
      }
      self$start() # necessary reference to self
      .loop <<- loop
      poll()
      invisible()
      # nocov end
    },
    #' @description Terminate the auto-scaling loop started by
    #'   `controller$autoscale()`.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    #' @return `NULL` (invisibly).
    descale = function(controllers = NULL) {
      .loop <<- NULL
      invisible()
    },
    #' @description Report the number of consecutive crashes of a task.
    #' @details See the `crashes_max` argument of [crew_controller()].
    #' @return Non-negative integer, number of consecutive times the task
    #'   crashed.
    #' @param name Character string, name of the task to check.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    crashes = function(name, controllers = NULL) {
      .subset2(.crash_log, "get")(key = name, default = 0L)
    },
    #' @description Push a task to the head of the task list.
    #' @return Invisibly return the `mirai` object of the pushed task.
    #'   This allows you to interact with the task directly, e.g.
    #'   to create a promise object with `promises::as.promise()`.
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
    #'   within the last polling interval. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param name Character string, name of the task. If `NULL`, then
    #'   a random name is generated automatically.
    #'   The name of the task must not conflict with the name of another
    #'   task pushed to the controller. Any previous task with the same name
    #'   must first be popped before a new task with that name can be pushed.
    #' @param save_command Deprecated on 2025-01-22 (`crew` version
    #'   0.10.2.9004) and no longer used.
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
      name = NULL,
      save_command = NULL,
      controller = NULL
    ) {
      start()
      name <- .name_new_task(name)
      if (substitute) {
        command <- substitute(command)
      }
      if (is.null(seconds_timeout)) {
        .timeout <- NULL
      } else {
        .timeout <- seconds_timeout * 1000
      }
      if (!is.null(.backup)) {
        if ((.crashes_max > 0L) && (crashes(name = name) == .crashes_max)) {
          return(
            .subset2(.backup, "push")(
              command = command,
              data = data,
              globals = globals,
              substitute = FALSE,
              seed = seed,
              algorithm = algorithm,
              packages = packages,
              library = library,
              seconds_timeout = seconds_timeout,
              scale = scale,
              throttle = throttle,
              name = name,
              controller = controller
            )
          )
        }
      }
      task <- mirai::mirai(
        .expr = expr_crew_eval,
        .args = list(
          name = name,
          command = command,
          data = data,
          globals = globals,
          seed = seed,
          algorithm = algorithm,
          packages = packages,
          library = library,
          reset_globals = .reset_globals,
          reset_packages = .reset_packages,
          reset_options = .reset_options,
          garbage_collection = .garbage_collection
        ),
        .timeout = .timeout,
        .compute = .subset2(.client, "profile")
      )
      .subset2(.tasks, "set")(key = name, value = task)
      if (scale) {
        scale(throttle = throttle)
      }
      invisible(task)
    },
    #' @description Apply a single command to multiple inputs,
    #'   and return control to the user without
    #'   waiting for any task to complete.
    #' @details In contrast to `walk()`, `map()` blocks the local R session
    #'   and waits for all tasks to complete.
    #' @return Invisibly returns a list of `mirai` task objects for the
    #'   newly created tasks. The order of tasks in the list matches the
    #'   order of data in the `iterate` argument.
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
    #' @param seconds_timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param names Optional character of length 1, name of the element of
    #'   `iterate` with names for the tasks. If `names` is supplied,
    #'   then `iterate[[names]]` must be a character vector.
    #' @param save_command Deprecated on 2025-01-22 (`crew` version
    #'   0.10.2.9004). The command is always saved now.
    #' @param verbose Logical of length 1, whether to print to a progress bar
    #'   when pushing tasks.
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. See also the `throttle` argument.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last polling interval. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    walk = function(
      command,
      iterate,
      data = list(),
      globals = list(),
      substitute = TRUE,
      seed = NULL,
      algorithm = NULL,
      packages = character(0),
      library = NULL,
      seconds_timeout = NULL,
      names = NULL,
      save_command = NULL,
      verbose = interactive(),
      scale = TRUE,
      throttle = TRUE,
      controller = NULL
    ) {
      start()
      crew_deprecate(
        name = "save_command",
        date = "2025-01-22",
        version = "0.10.2.9004",
        alternative = "none (no longer needed)",
        condition = "warning",
        value = save_command
      )
      crew_assert(substitute, isTRUE(.) || isFALSE(.))
      if (substitute) {
        command <- substitute(command)
      }
      crew_assert(
        iterate,
        is.list(.),
        rlang::is_named(.),
        message = "the 'iterate' argument must be a nonempty named list"
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
        message = "the \"data\" argument must be a named list"
      )
      crew_assert(
        globals,
        is.list(.),
        rlang::is_named(.) || length(.) < 1L,
        message = "the \"globals\" argument must be a named list"
      )
      crew_assert(
        seed %|||% 1L,
        is.numeric(.),
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
        names %|||% names(iterate)[[1L]],
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.),
        . %in% names(iterate),
        message = "names argument must be NULL or an element of names(iterate)"
      )
      crew_assert(verbose, isTRUE(.) || isFALSE(.))
      crew_assert(scale, isTRUE(.) || isFALSE(.))
      crew_assert(throttle, isTRUE(.) || isFALSE(.))
      names <- if_any(
        is.null(names),
        paste(
          name_task_tempfile(),
          as.character(seq_along(iterate[[1L]])),
          sep = "_"
        ),
        as.character(iterate[[names]])
      )
      crew_assert(
        names,
        is.character(.),
        !anyNA(.),
        message = "task names in map() must be valid character strings."
      )
      crew_assert(
        anyDuplicated(names) < 1L,
        message = "task names in map() must not have duplicates"
      )
      names_iterate <- names(iterate)
      start()
      sign <- if_any(!is.null(seed) && seed > 0L, 1L, -1L)
      if (!is.null(seed)) {
        seed <- 1L
      }
      total <- length(names)
      tasks <- vector(mode = "list", length = total)
      names(tasks) <- names
      # covered in tests/local/test-map.R
      # nocov start
      if (verbose) {
        this_envir <- environment()
        progress_envir <- new.env(parent = this_envir)
        bar <- cli::cli_progress_bar(
          total = total,
          type = "custom",
          format = paste(
            "walk() {cli::pb_current}/{cli::pb_total}",
            "{cli::pb_bar}",
            "{cli::pb_elapsed}"
          ),
          format_done = "{cli::pb_total} tasks pushed in {cli::pb_elapsed}",
          clear = FALSE,
          .envir = progress_envir
        )
      }
      # nocov end
      index <- 1L
      while (index <= total) {
        # covered in tests/local/test-map.R
        # nocov start
        if (verbose) {
          cli::cli_progress_update(id = bar, .envir = progress_envir)
        }
        # nocov end
        for (name in names_iterate) {
          data[[name]] <- .subset2(.subset2(iterate, name), index)
        }
        if (is.null(seed)) {
          task_seed <- NULL
        } else {
          task_seed <- seed - (sign * index)
        }
        tasks[[index]] <- push(
          command = command,
          substitute = FALSE,
          data = data,
          globals = globals,
          seed = task_seed,
          algorithm = algorithm,
          packages = packages,
          library = library,
          seconds_timeout = seconds_timeout,
          scale = FALSE,
          name = .subset(names, index)
        )
        index <- index + 1L
      }
      # covered in tests/local/test-map.R
      # nocov start
      if (verbose) {
        cli::cli_progress_done(id = bar, .envir = progress_envir)
      }
      # nocov end
      if (scale) {
        scale(throttle = throttle)
      }
      invisible(tasks)
    },
    #' @description Apply a single command to multiple inputs,
    #'   wait for all tasks to complete,
    #'   and return the results of all tasks.
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
    #' @param seconds_interval Deprecated on 2025-01-17 (`crew` version
    #'   0.10.2.9003). Instead, the `seconds_interval` argument passed
    #'   to [crew_controller_group()] is used as `seconds_max`
    #'   in a [crew_throttle()] object which orchestrates exponential
    #'   backoff.
    #' @param seconds_timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param names Optional character string, name of the element of
    #'   `iterate` with names for the tasks. If `names` is supplied,
    #'   then `iterate[[names]]` must be a character vector.
    #' @param save_command Deprecated on 2025-01-22 (`crew` version
    #'   0.10.2.9004). The command is always saved now.
    #' @param error Character of length 1, choice of action if
    #'   a task was not successful. Possible values:
    #'   * `"stop"`: throw an error in the main R session instead of returning
    #'     a value. In case of an error, the results from the last errored
    #'     `map()` are in the `error` field
    #'     of the controller, e.g. `controller_object$error`. To reduce
    #'     memory consumption, set `controller_object$error <- NULL` after
    #'     you are finished troubleshooting.
    #'   * `"warn"`: throw a warning. This allows the return value with
    #'     all the error messages and tracebacks to be generated.
    #'   * `"silent"`: do nothing special.
    #'   NOTE: the only kinds of errors considered here are errors at the R
    #'   level. A crashed tasks will return a status of `"crash"` in the output
    #'   and not trigger an error in `map()` unless `crashes_max` is reached.
    #' @param warnings Logical of length 1, whether to throw a warning in the
    #'   interactive session if at least one task encounters an error.
    #' @param verbose Logical of length 1, whether to print to a progress bar
    #'   as tasks resolve.
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. See also the `throttle` argument.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last polling interval. `FALSE` to auto-scale
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
      seconds_interval = NULL,
      seconds_timeout = NULL,
      names = NULL,
      save_command = NULL,
      error = "stop",
      warnings = TRUE,
      verbose = interactive(),
      scale = TRUE,
      throttle = TRUE,
      controller = NULL
    ) {
      crew_deprecate(
        name = "seconds_interval",
        date = "2025-01-17",
        version = "0.10.2.9003",
        alternative = "none (no longer used)",
        condition = "warning",
        value = seconds_interval
      )
      crew_deprecate(
        name = "save_command",
        date = "2025-01-22",
        version = "0.10.2.9004",
        alternative = "none (no longer needed)",
        condition = "warning",
        value = save_command
      )
      crew_assert(
        .subset2(.tasks, "size")() < 1L,
        message = "cannot map() until all prior tasks are completed and popped"
      )
      crew_assert(substitute, isTRUE(.) || isFALSE(.))
      crew_assert(error %in% c("stop", "warn", "silent"))
      if (substitute) {
        command <- substitute(command)
      }
      tasks <- walk(
        command = command,
        iterate = iterate,
        data = data,
        globals = globals,
        substitute = FALSE,
        seed = seed,
        algorithm = algorithm,
        packages = packages,
        library = library,
        seconds_timeout = seconds_timeout,
        names = names,
        scale = scale,
        throttle = throttle
      )
      names <- names(tasks)
      total <- length(tasks)
      relay <- .subset2(.client, "relay")
      start <- nanonext::mclock()
      if (verbose) {
        this_envir <- environment()
        progress_envir <- new.env(parent = this_envir)
        bar <- cli::cli_progress_bar(
          total = total,
          type = "custom",
          format = paste(
            "map() {cli::pb_current}/{cli::pb_total}",
            "{cli::pb_bar}",
            "{cli::pb_elapsed}"
          ),
          format_done = "{cli::pb_total} tasks resolved in {cli::pb_elapsed}",
          clear = FALSE,
          .envir = progress_envir
        )
      }
      iterate <- function() {
        if (scale) {
          scale(throttle = throttle)
        }
        unresolved <- unresolved()
        if (verbose) {
          cli::cli_progress_update(
            set = total - unresolved,
            id = bar,
            .envir = progress_envir
          )
        }
        if (unresolved > 0L) {
          .subset2(relay, "wait")()
        }
        unresolved() < 1L
      }
      crew_retry(
        fun = iterate,
        seconds_interval = 0,
        seconds_timeout = Inf,
        error = FALSE,
        assertions = FALSE
      )
      if (verbose) {
        cli::cli_progress_done(id = bar, .envir = progress_envir)
      }
      out <- vector(mode = "list", length = total)
      controller_name <- .subset2(.launcher, "name")
      index <- 1L
      while (index <= total) {
        task <- .subset2(tasks, index)
        name <- .subset(names, index)
        monad <- as_monad(
          task = task,
          name = name,
          controller = controller_name
        )
        .scan_crash(name = name, task = monad)
        out[[index]] <- monad
        index <- index + 1L
      }
      out <- tibble::new_tibble(data.table::rbindlist(out, use.names = FALSE))
      out <- out[match(x = names, table = out$name), , drop = FALSE] # nolint
      out <- out[!is.na(out$name), , drop = FALSE] # nolint
      on.exit({
        .tasks <<- collections::dict()
        .summary$seconds <<- .subset2(.summary, "seconds") +
          sum(out$seconds)
        for (status in c("success", "error", "crash", "cancel")) {
          .summary[[status]] <<- .subset2(.summary, status) +
            sum(.subset2(out, "status") == status)
        }
        .summary$warning <<- .subset2(.summary, "warning") +
          sum(!is.na(out$warnings))
      })
      warning_messages <- out$warnings
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
      error_messages <- out$error
      if (!all(is.na(error_messages)) && !identical(error, "silent")) {
        message <- sprintf(
          "%s tasks were not successful. First error message: \"%s\".",
          sum(!is.na(error_messages)),
          error_messages[min(which(!is.na(error_messages)))]
        )
        if (identical(error, "stop")) {
          .error <<- out
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
    #' @description Pop a completed task from the results data frame.
    #' @details If not task is currently completed, `pop()`
    #'   will attempt to auto-scale workers as needed.
    #' @return If there is no task to collect, return `NULL`. Otherwise,
    #'   return a one-row `tibble` with the following columns.
    #'   * `name`: the task name.
    #'   * `command`: a character string with the R command.
    #'   * `result`: a list containing the return value of the R command.
    #'     `NA` if the task failed.
    #'   * `status`: a character string. `"success"` if the task succeeded,
    #'     `"cancel"` if the task was canceled with
    #'     the `cancel()` controller method,
    #'     `"crash"` if the worker running the task exited before
    #'     it could complete the task, or `"error"`
    #'      for any other kind of error.
    #'   * `error`: the first 2048 characters of the error message if
    #'     the task status is not `"success"`, `NA` otherwise.
    #'     Messages for crashes and cancellations are captured here
    #'     alongside ordinary R-level errors.
    #'   * `code`: an integer code denoting the specific exit status:
    #'     `0` for successful tasks, `-1` for tasks with an error in the R
    #'     command of the task, and another positive integer with an NNG
    #'     status code if there is an error at the NNG/`nanonext` level.
    #'     `nanonext::nng_error()` can interpret these codes.
    #'   * `trace`: the first 2048 characters of the text of the traceback
    #'     if the task threw an error, `NA` otherwise.
    #'   * `warnings`: the first 2048 characters. of the text of
    #'     warning messages that the task may have generated, `NA` otherwise.
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
    #'   * `controller`: name of the `crew` controller where the task ran.
    #'   * `worker`: name of the `crew` worker that ran the task.
    #' @param scale Logical of length 1,
    #'   whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #'   Scaling up on `pop()` may be important
    #'   for transient or nearly transient workers that tend to drop off
    #'   quickly after doing little work.
    #'   See also the `throttle` argument.
    #' @param collect Deprecated in version 0.5.0.9003 (2023-10-02).
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last polling interval. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param error `NULL` or character of length 1, choice of action if
    #'   the popped task threw an error. Possible values:
    #'   * `"stop"`: throw an error in the main R session instead of returning
    #'     a value.
    #'   * `"warn"`: throw a warning.
    #'   * `NULL` or `"silent"`: do not react to errors.
    #'   NOTE: the only kinds of errors considered here are errors at the R
    #'   level. A crashed tasks will return a status of `"crash"` in the output
    #'   and not trigger an error in `pop()` unless `crashes_max` is reached.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    pop = function(
      scale = TRUE,
      collect = NULL,
      throttle = TRUE,
      error = NULL,
      controllers = NULL
    ) {
      if (!.subset2(.client, "started")) {
        return(NULL)
      }
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
      if (!is.null(error)) {
        crew_assert(
          error,
          is.character(.),
          !anyNA(.),
          nzchar(.),
          length(.) == 1L,
          error %in% c("stop", "warn", "silent")
        )
      }
      if (scale) {
        scale(throttle = throttle)
      }
      if (empty()) {
        return(NULL)
      }
      .resolve(force = FALSE)
      if (.subset2(.queue_resolved, "size")() < 1L) {
        return(NULL)
      }
      name <- .subset2(.queue_resolved, "pop")()
      task <- .subset2(.tasks, "get")(name)
      .subset2(.tasks, "remove")(name)
      out <- as_monad(
        task = task,
        name = name,
        controller = .subset2(.launcher, "name")
      )
      suppressWarnings({
        .scan_crash(name = name, task = out)
        seconds <- .subset2(out, "seconds")
        if (!anyNA(seconds)) {
          .summary$seconds <<- .subset2(.summary, "seconds") + seconds
        }
      })
      status <- .subset2(out, "status")
      .summary[[status]] <<- .subset2(.summary, status) + 1L
      if (!anyNA(.subset2(out, "warnings"))) {
        # Tests definitely cover this line, but for some reason
        # the code coverage CI workflow does not detect it.
        .summary$warning <<- .subset2(.summary, "warning") + 1L # nocov
      }
      if (!is.null(error)) {
        has_error <- any(status != "success")
        throw_error <- has_error && all(error == "stop")
        throw_warning <- has_error && all(error == "warn")
        if_any(
          throw_error,
          crew_error(message = .subset2(out, "error")),
          NULL
        )
        if_any(
          throw_warning,
          crew_warning(message = .subset2(out, "error")),
          NULL
        )
      }
      out
    },
    #' @description Pop all available task results and return them in a tidy
    #'   `tibble`.
    #' @return A `tibble` of results and metadata of all resolved tasks,
    #'   with one row per task. Returns `NULL` if there are no tasks
    #'   to collect. See `pop()` for details on the columns of the
    #'   returned `tibble`.
    #' @param scale Logical of length 1,
    #'   whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last polling interval. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param error `NULL` or character of length 1, choice of action if
    #'   the popped task threw an error. Possible values:
    #'     * `"stop"`: throw an error in the main R session instead of
    #'       returning a value.
    #'     * `"warn"`: throw a warning.
    #'     * `NULL` or `"silent"`: do not react to errors.
    #'   NOTE: the only kinds of errors considered here are errors at the R
    #'   level. A crashed tasks will return a status of `"crash"` in the output
    #'   and not trigger an error in `collect()`
    #'   unless `crashes_max` is reached.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    collect = function(
      scale = TRUE,
      throttle = TRUE,
      error = NULL,
      controllers = NULL
    ) {
      if (!is.null(error)) {
        crew_assert(
          error,
          is.character(.),
          !anyNA(.),
          nzchar(.),
          length(.) == 1L,
          error %in% c("stop", "warn", "silent"),
          message = "invalid error argument to collect()"
        )
      }
      if (scale) {
        scale(throttle = throttle)
      }
      if (empty()) {
        return(NULL)
      }
      .resolve(force = TRUE)
      queue_pop <- .subset2(.queue_resolved, "pop")
      n <- .subset2(.queue_resolved, "size")()
      names <- as.character(replicate(n, queue_pop(), simplify = FALSE))
      if (!length(names)) {
        return(NULL)
      }
      controller_name <- .subset2(.launcher, "name")
      out <- lapply(names, function(name) {
        out <- as_monad(
          task = .subset2(.tasks, "get")(name),
          name = name,
          controller = controller_name
        )
        .scan_crash(name = name, task = out)
        out
      })
      out <- tibble::new_tibble(data.table::rbindlist(out, use.names = FALSE))
      lapply(names, .subset2(.tasks, "remove"))
      .summary$seconds <<- .subset2(.summary, "seconds") +
        sum(.subset2(out, "seconds"), na.rm = TRUE)
      for (status in c("success", "error", "crash", "cancel")) {
        .summary[[status]] <<- .subset2(.summary, status) +
          sum(.subset2(out, "status") == status)
      }
      .summary$warning <<- .subset2(.summary, "warning") +
        sum(!is.na(.subset2(out, "warnings")))
      errors <- .subset2(out, "error")
      errors <- errors[!is.na(errors)]
      if (!is.null(error) && length(errors)) {
        if (identical(error, "stop")) {
          crew_error(message = errors[1L])
        } else if (identical(error, "warn")) {
          crew_warning(message = errors[1L])
        }
      }
      out
    },
    #' @description Wait for tasks.
    #' @details The `wait()` method blocks the calling R session
    #'   until the condition in the `mode` argument is met.
    #'   During the wait, `wait()` iteratively auto-scales the workers.
    #' @return A logical of length 1, invisibly.
    #'   `wait(mode = "all")` returns `TRUE` if all tasks in the `mirai`
    #'   compute profile have resolved (`FALSE` otherwise).
    #'   `wait(mode = "one")` returns `TRUE` if the controller is ready
    #'   to pop or collect at least one resolved task (`FALSE` otherwise).
    #'   `wait(mode = "one")` assumes all
    #'   tasks were submitted through the controller and not by other means.
    #' @param mode Character string, name of the waiting condition.
    #'   `wait(mode = "all")` waits until all tasks in the `mirai`
    #'   compute profile resolve, and
    #'   `wait(mode = "one")` waits until at least one task is available
    #'   to `push()` or `collect()` from the controller.
    #'   The former still works if the controller is not the only
    #'   means of submitting tasks to the compute profile,
    #'   whereas the latter assumes only the controller submits tasks.
    #' @param seconds_interval Deprecated on 2025-01-17 (`crew` version
    #'   0.10.2.9003). Instead, the `seconds_interval` argument passed
    #'   to [crew_controller_group()] is used as `seconds_max`
    #'   in a [crew_throttle()] object which orchestrates exponential
    #'   backoff.
    #' @param seconds_timeout Timeout length in seconds waiting for tasks.
    #' @param scale Logical, whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #'   See also the `throttle` argument.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last polling interval. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
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
      crew_deprecate(
        name = "seconds_interval",
        date = "2025-01-17",
        version = "0.10.2.9003",
        alternative = "none (no longer used)",
        condition = "warning",
        value = seconds_interval
      )
      crew_assert(mode, identical(., "all") || identical(., "one"))
      if (size() < 1L || !started()) {
        return(identical(mode, "all"))
      }
      if (identical(mode, "all")) {
        wait_event <- function() {
          if (self$unresolved() > 0L) {
            .client$relay$wait()
          }
          self$unresolved() < 1L
        }
      } else {
        wait_event <- function() {
          if (size() - self$unresolved() < 1L) {
            .client$relay$wait()
          }
          self$size() - self$unresolved() > 0L
        }
      }
      envir <- new.env(parent = emptyenv())
      envir$result <- FALSE
      iterate <- function() {
        if (!envir$result && scale) {
          scale(throttle = throttle)
        }
        envir$result <- wait_event()
        envir$result
      }
      crew_retry(
        fun = iterate,
        seconds_interval = 0,
        seconds_timeout = seconds_timeout,
        error = FALSE,
        assertions = FALSE
      )
      invisible(envir$result)
    },
    #' @description Push the name of a task to the backlog.
    #' @details `pop_backlog()` pops the tasks that can be pushed
    #'   without saturating the controller.
    #' @param name Character of length 1 with the task name to push to
    #'   the backlog.
    #' @return `NULL` (invisibly).
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    push_backlog = function(name, controller = NULL) {
      .subset2(.queue_backlog, "push")(name)
      invisible()
    },
    #' @description Pop the task names from the head of the backlog which
    #'   can be pushed without saturating the controller.
    #' @return Character vector of task names which can be pushed to the
    #'   controller without saturating it. If the controller is saturated,
    #'   `character(0L)` is returned.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    pop_backlog = function(controllers = NULL) {
      # TODO: when mirai gains a threaded dispatcher and status() is
      # zero overhead, reinstate the following line of code:
      #
      # n <- .subset2(.launcher, "workers") - unresolved()
      #
      # In place of these 3 lines:
      max_workers <- .subset2(.launcher, "workers")
      uncollected_tasks <- .subset2(.tasks, "size")()
      n <- max(0, max_workers - uncollected_tasks)
      # The rest of pop_backlog() doesn't depend on the issue above:
      n <- min(n, .subset2(.queue_backlog, "size")())
      if (n < 1L) {
        return(character(0L))
      }
      backlog_pop <- .subset2(.queue_backlog, "pop")
      as.character(replicate(n, backlog_pop(), simplify = FALSE))
    },
    #' @description Summarize the collected tasks of the controller.
    #' @return A data frame of cumulative summary statistics on the tasks
    #'   collected through `pop()` and `collect()`.
    #'   It has one row and the following columns:
    #'   * `controller`: name of the controller.
    #'   * `seconds`: total number of runtime in seconds.
    #'   * `tasks`: total number of tasks collected.
    #'   * `success`: total number of collected tasks that did not crash
    #'     or error.
    #'   * `error`: total number of tasks with errors, either in the R code
    #'     of the task or an NNG-level error that is not a cancellation
    #'     or crash.
    #'   * `crash`: total number of crashed tasks (where the worker exited
    #'     unexpectedly while it was running the task).
    #'   * `cancel`: total number of tasks interrupted with the `cancel()`
    #'     controller method.
    #'   * `warning`: total number of tasks with one or more warnings.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    summary = function(controllers = NULL) {
      if (is.null(.summary)) {
        return(NULL)
      }
      out <- tibble::new_tibble(.summary)
      out$tasks <- out$success + out$error + out$crash + out$cancel
      columns <- c(
        "controller",
        "seconds",
        "tasks",
        "success",
        "error",
        "crash",
        "cancel",
        "warning"
      )
      out[, columns]
    },
    #' @description Cancel one or more tasks.
    #' @return `NULL` (invisibly).
    #' @param names Character vector of names of tasks to cancel.
    #'   Those names must have been manually supplied by `push()`.
    #' @param all `TRUE` to cancel all tasks, `FALSE` otherwise.
    #'   `all = TRUE` supersedes the `names` argument.
    cancel = function(names = character(0L), all = FALSE) {
      crew_assert(
        all,
        isTRUE(.) || isFALSE(.),
        message = "'all' must be TRUE or FALSE"
      )
      crew_assert(
        names,
        is.character(.),
        !anyNA(.),
        nzchar(.),
        message = paste(
          "'names' must be a character vector",
          "with no missing or empty strings"
        )
      )
      if (all) {
        mirai::stop_mirai(.subset2(.tasks, "as_list")())
      }
      names <- intersect(names, .subset2(.tasks, "keys")())
      for (name in names) {
        mirai::stop_mirai(.subset2(.tasks, "get")(name))
      }
      invisible()
    },
    #' @description Deprecated on 2025-08-26 in `crew` version 1.2.1.9005.
    #' @return The integer process ID of the current process.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    pids = function(controllers = NULL) {
      crew::crew_deprecate(
        name = "pids()",
        date = "2025-08-26",
        version = "1.2.1.9006",
        alternative = "none",
        condition = "warning",
        value = "x"
      )
      Sys.getpid()
    },
    #' @description Terminate the workers and the `mirai` client.
    #' @return `NULL` (invisibly).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    terminate = function(controllers = NULL) {
      .client$terminate()
      .launcher$terminate()
      .tasks <<- collections::dict()
      .crash_log <<- collections::dict()
      .loop <<- FALSE
      .queue_resolved <<- collections::queue()
      .queue_backlog <<- collections::queue()
      invisible()
    }
  )
)
