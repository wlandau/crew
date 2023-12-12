#' @title Create a controller group.
#' @export
#' @family controller_group
#' @description Create an `R6` object to submit tasks and launch workers
#'   through multiple `crew` controllers.
#' @param ... `R6` controller objects or lists of `R6` controller objects.
#'   Nested lists are allowed, but each element must be a control object
#'   or another list.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' persistent <- crew_controller_local(name = "persistent")
#' transient <- crew_controller_local(
#'   name = "transient",
#'   tasks_max = 1L
#' )
#' group <- crew_controller_group(persistent, transient)
#' group$start()
#' group$push(name = "task", command = sqrt(4), controller = "transient")
#' group$wait()
#' group$pop()
#' group$terminate()
#' }
crew_controller_group <- function(...) {
  controllers <- unlist(list(...), recursive = TRUE)
  names(controllers) <- map_chr(controllers, ~.x$client$name)
  walk(
    names(controllers),
    ~crew_assert(
      !isTRUE(controllers[[.x]]$client$started),
      message = paste(
        "controller",
        .x,
        "supplied to crew_controller_group()",
        "must not already be started."
      )
    )
  )
  relay <- crew_relay()
  relay$start()
  for (controller in controllers) {
    controller$client$relay$set_to(relay$condition)
  }
  out <- crew_class_controller_group$new(
    controllers = controllers,
    relay = relay
  )
  out$validate()
  out
}

#' @title Controller group class
#' @export
#' @family controller_group
#' @description `R6` class for controller groups.
#' @details See [crew_controller_group()].
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' persistent <- crew_controller_local(name = "persistent")
#' transient <- crew_controller_local(
#'   name = "transient",
#'   tasks_max = 1L
#' )
#' group <- crew_controller_group(persistent, transient)
#' group$start()
#' group$push(name = "task", command = sqrt(4), controller = "transient")
#' group$wait()
#' group$pop()
#' group$terminate()
#' }
crew_class_controller_group <- R6::R6Class(
  classname = "crew_class_controller_group",
  cloneable = FALSE,
  private = list(
    .controllers = NULL,
    .relay = NULL,
    .select_controllers = function(names) {
      if (is.null(names)) {
        return(private$.controllers)
      }
      message <- "'controllers' must be a valid nonempty character vector."
      crew_assert(
        names,
        length(.) > 0L,
        is.character(.),
        !anyNA(.),
        nzchar(.),
        message = message
      )
      invalid <- setdiff(names, names(private$.controllers))
      crew_assert(
        !length(invalid),
        message = sprintf(
          "controllers not found: %s",
          paste(invalid, collapse = ", ")
        )
      )
      private$.controllers[names]
    },
    .select_single_controller = function(name) {
      names <- names(private$.controllers)
      name <- name %|||% utils::head(names, n = 1L)
      crew_assert(
        name %in% names,
        message = sprintf("controller not found: %s", name)
      )
      private$.controllers[[name]]
    },
    .wait_one = function(
      controllers,
      seconds_interval,
      seconds_timeout,
      scale,
      throttle
    ) {
      if (sum(map_int(controllers, ~length(.x$tasks))) < 1L) {
        return(FALSE)
      }
      envir <- new.env(parent = emptyenv())
      envir$result <- FALSE
      crew_retry(
        fun = ~{
          if (scale) {
            walk(controllers, ~.x$scale(throttle = throttle))
          }
          for (controller in controllers) {
            if (controller$unpopped() > 0L) {
              envir$result <- TRUE
              return(TRUE)
            }
          }
          private$.relay$wait(seconds_timeout = seconds_interval)
          FALSE
        },
        seconds_interval = 0,
        seconds_timeout = seconds_timeout,
        error = FALSE
      )
      envir$result
    },
    .wait_all = function(
      controllers,
      seconds_interval,
      seconds_timeout,
      scale,
      throttle
    ) {
      for (controller in controllers) {
        out <- controller$wait(
          mode = "all",
          seconds_interval = seconds_interval,
          seconds_timeout = seconds_timeout,
          scale = scale,
          throttle = throttle
        )
        if (!out) {
          return(FALSE)
        }
      }
      TRUE
    }
  ),
  active = list(
    #' @field controllers List of `R6` controller objects.
    controllers = function() {
      .subset2(private, ".controllers")
    },
    #' @field relay Relay object for event-driven programming on a downstream
    #'   condition variable.
    relay = function() {
      .subset2(private, ".relay")
    }
  ),
  public = list(
    #' @description Multi-controller constructor.
    #' @return An `R6` object with the controller group object.
    #' @param controllers List of `R6` controller objects.
    #' @param relay Relay object for event-driven programming on a downstream
    #'   condition variable.
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' persistent <- crew_controller_local(name = "persistent")
    #' transient <- crew_controller_local(
    #'   name = "transient",
    #'   tasks_max = 1L
    #' )
    #' group <- crew_controller_group(persistent, transient)
    #' group$start()
    #' group$push(name = "task", command = sqrt(4), controller = "transient")
    #' group$wait()
    #' group$pop()
    #' group$terminate()
    #' }
    initialize = function(
      controllers = NULL,
      relay = NULL
    ) {
      private$.controllers <- controllers
      private$.relay <- relay
      invisible()
    },
    #' @description Validate the client.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(
        map_lgl(private$.controllers, ~inherits(.x, "crew_class_controller")),
        message = "All objects in a controller group must be controllers."
      )
      out <- unname(map_chr(private$.controllers, ~.x$client$name))
      exp <- names(private$.controllers)
      crew_assert(identical(out, exp), message = "bad controller names")
      crew_assert(inherits(private$.relay, "crew_class_relay"))
      private$.relay$validate()
      invisible()
    },
    #' @description See if the controllers are empty.
    #' @details A controller is empty if it has no running tasks
    #'   or completed tasks waiting to be retrieved with `push()`.
    #' @return `TRUE` if all the selected controllers are empty,
    #'   `FALSE` otherwise.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    empty = function(controllers = NULL) {
      control <- private$.select_controllers(controllers)
      all(map_lgl(control, ~.x$empty()))
    },
    #' @description Check if a controller is saturated.
    #' @details A controller is saturated if the number of unresolved tasks
    #'   is greater than or equal to the maximum number of workers.
    #'   In other words, in a saturated controller, every available worker
    #'   has a task.
    #'   You can still push tasks to a saturated controller, but
    #'   tools that use `crew` such as `targets` may choose not to.
    #' @return `TRUE` if all the selected controllers are saturated,
    #'   `FALSE` otherwise.
    #' @param collect Deprecated in version 0.5.0.9003 (2023-10-02). Not used.
    #' @param throttle Deprecated in version 0.5.0.9003 (2023-10-02). Not used.
    #' @param controller Character vector of length 1 with the controller name.
    #'   Set to `NULL` to select the default controller that `push()`
    #'   would choose.
    saturated = function(collect = NULL, throttle = NULL, controller = NULL) {
      control <- private$.select_single_controller(name = controller)
      control$saturated()
    },
    #' @description Start one or more controllers.
    #' @return `NULL` (invisibly).
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    start = function(controllers = NULL) {
      control <- private$.select_controllers(controllers)
      walk(control, ~.x$start())
    },
    #' @description Launch one or more workers on one or more controllers.
    #' @return `NULL` (invisibly).
    #' @param n Number of workers to launch in each controller selected.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    launch = function(n = 1L, controllers = NULL) {
      control <- private$.select_controllers(controllers)
      walk(control, ~.x$launch(n = n))
    },
    #' @description Automatically scale up the number of workers if needed
    #'   in one or more controller objects.
    #' @details See the `scale()` method in individual controller classes.
    #' @return `NULL` (invisibly).
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    scale = function(throttle = TRUE, controllers = NULL) {
      control <- private$.select_controllers(controllers)
      walk(control, ~.x$scale(throttle = throttle))
    },
    #' @description Push a task to the head of the task list.
    #' @return `NULL` (invisibly).
    #' @param command Language object with R code to run.
    #' @param data Named list of local data objects in the
    #'   evaluation environment.
    #' @param globals Named list of objects to temporarily assign to the
    #'   global environment for the task. See the `reset_globals`
    #'   argument of [crew_controller_local()].
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
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. See the `scale` argument of the `push()` method of
    #'   ordinary single controllers.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param name Optional name of the task. Replaced with a random name
    #'   if `NULL` or in conflict with an existing name in the task list.
    #' @param save_command Logical of length 1. If `TRUE`, the controller
    #'   deparses the command and returns it with the output on `pop()`.
    #'   If `FALSE` (default), the controller skips this step to
    #'   increase speed.
    #' @param controller Character of length 1,
    #'   name of the controller to submit the task.
    #'   If `NULL`, the controller defaults to the
    #'   first controller in the list.
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
      save_command = FALSE,
      controller = NULL
    ) {
      if (substitute) {
        command <- substitute(command)
      }
      control <- .subset2(
        private,
        ".select_single_controller"
      )(name = controller)
      .subset2(control, "push")(
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
        save_command = save_command
      )
    },
    #' @description Apply a single command to multiple inputs.
    #' @details The idea comes from functional programming: for example,
    #'   the `map()` function from the `purrr` package.
    #' @return A `tibble` of results and metadata: one row per task and
    #'   columns corresponding to the output of `pop()`.
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
    #' @param controller Character of length 1,
    #'   name of the controller to submit the task.
    #'   If `NULL`, the controller defaults to the
    #'   first controller in the list.
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
      control <- private$.select_single_controller(name = controller)
      control$map(
        command = command,
        iterate = iterate,
        data = data,
        globals = globals,
        substitute = FALSE,
        seed = seed,
        algorithm = algorithm,
        packages = packages,
        library = library,
        seconds_interval = seconds_interval,
        seconds_timeout = seconds_timeout,
        names = names,
        save_command = save_command,
        error = error,
        warnings = warnings,
        verbose = verbose,
        scale = scale,
        throttle = throttle
      )
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
    #' @return If there is no task to collect, return `NULL`. Otherwise,
    #'   return a one-row `tibble` with the same columns as `pop()`
    #'   for ordinary controllers.
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. See the `scale` argument of the `pop()` method of
    #'   ordinary single controllers.
    #' @param collect Deprecated in version 0.5.0.9003 (2023-10-02). Not used.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    pop = function(
      scale = TRUE,
      collect = NULL,
      throttle = TRUE,
      controllers = NULL
    ) {
      control <- private$.select_controllers(controllers)
      for (controller in control) {
        out <- controller$pop(scale = scale, throttle = throttle)
        if (!is.null(out)) {
          return(out)
        }
      }
      NULL
    },
    #' @description Wait for tasks.
    #' @details The `wait()` method blocks the calling R session and
    #'   repeatedly auto-scales workers for tasks that need them.
    #'   The function runs until it either times out or the condition
    #'   in `mode` is met.
    #' @return A logical of length 1, invisibly. `TRUE` if the condition
    #'   in `mode` was met, `FALSE` otherwise.
    #' @param mode Character of length 1: `"all"` to wait for
    #'   all tasks in all controllers to complete, `"one"` to wait for
    #'   a single task in a single controller to complete. In this scheme,
    #'   the timeout limit is applied to each controller sequentially,
    #'   and a timeout is treated the same as a completed controller.
    #' @param seconds_interval Number of seconds to interrupt the wait
    #'   in order to scale up workers as needed.
    #' @param seconds_timeout Timeout length in seconds waiting for
    #'   results to become available.
    #' @param scale Logical of length 1, whether to call `scale_later()`
    #'   on each selected controller to schedule auto-scaling.
    #'   See the `scale` argument of the `wait()` method of
    #'   ordinary single controllers.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    wait = function(
      mode = "all",
      seconds_interval = 0.5,
      seconds_timeout = Inf,
      scale = TRUE,
      throttle = TRUE,
      controllers = NULL
    ) {
      mode <- as.character(mode)
      crew_assert(mode, identical(., "all") || identical(., "one"))
      control <- private$.select_controllers(controllers)
      out <- if_any(
        identical(mode, "one"),
        private$.wait_one(
          controllers = control,
          seconds_interval = seconds_interval,
          seconds_timeout = seconds_timeout,
          scale = scale,
          throttle = throttle
        ),
        private$.wait_all(
          controllers = control,
          seconds_interval = seconds_interval,
          seconds_timeout = seconds_timeout,
          scale = scale,
          throttle = throttle
        )
      )
      invisible(out)
    },
    #' @description Summarize the workers of one or more controllers.
    #' @return A data frame of aggregated worker summary statistics
    #'   of all the selected controllers. It has one row per worker,
    #'   and the rows are grouped by controller.
    #'   See the documentation of the `summary()` method of the controller
    #'   class for specific information about the columns in the output.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    summary = function(controllers = NULL) {
      control <- private$.select_controllers(controllers)
      out <- map(control, ~.x$summary())
      if (all(map_lgl(out, is.null))) {
        return(NULL) # nocov
      }
      tibble::as_tibble(do.call(what = rbind, args = out))
    },
    #' @description Terminate the workers and disconnect the client
    #'   for one or more controllers.
    #' @return `NULL` (invisibly).
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    terminate = function(controllers = NULL) {
      walk(private$.select_controllers(controllers), ~.x$terminate())
    }
  )
)
