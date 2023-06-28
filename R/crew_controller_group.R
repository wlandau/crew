#' @title Create a controller group.
#' @export
#' @family user
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
  out <- crew_class_controller_group$new(controllers = controllers)
  out$validate()
  out
}

#' @title Controller group class
#' @export
#' @family class
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
    select_controllers = function(names) {
      if (is.null(names)) {
        return(self$controllers)
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
      invalid <- setdiff(names, names(self$controllers))
      crew_assert(
        !length(invalid),
        message = sprintf(
          "controllers not found: %s",
          paste(invalid, collapse = ", ")
        )
      )
      self$controllers[names]
    },
    select_single_controller = function(name) {
      names <- names(self$controllers)
      name <- name %|||% utils::head(names, n = 1L)
      crew_assert(
        name %in% names,
        message = sprintf("controller not found: %s", name)
      )
      self$controllers[[name]]
    }
  ),
  public = list(
    #' @field controllers List of `R6` controller objects.
    controllers = NULL,
    #' @description Multi-controller constructor.
    #' @return An `R6` object with the controller group object.
    #' @param controllers List of `R6` controller objects.
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
      controllers = NULL
    ) {
      self$controllers <- controllers
      invisible()
    },
    #' @description Validate the client.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(
        map_lgl(self$controllers, ~inherits(.x, "crew_class_controller")),
        message = "All objects in a controller group must be controllers."
      )
      out <- unname(map_chr(self$controllers, ~.x$client$name))
      exp <- names(self$controllers)
      crew_assert(identical(out, exp), message = "bad controller names")
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
      control <- private$select_controllers(controllers)
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
    #' @param collect Logical of length 1, whether to collect the results
    #'   of any newly resolved tasks before determining saturation.
    #' @param throttle Logical of length 1, whether to delay task collection
    #'   until the next request at least `self$client$seconds_interval`
    #'   seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param controller Character vector of length 1 with the controller name.
    #'   Set to `NULL` to select the default controller that `push()`
    #'   would choose.
    saturated = function(collect = TRUE, throttle = TRUE, controller = NULL) {
      control <- private$select_single_controller(name = controller)
      control$saturated(collect = collect, throttle = throttle)
    },
    #' @description Start one or more controllers.
    #' @return `NULL` (invisibly).
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    start = function(controllers = NULL) {
      control <- private$select_controllers(controllers)
      walk(control, ~.x$start())
    },
    #' @description Launch one or more workers on one or more controllers.
    #' @return `NULL` (invisibly).
    #' @param n Number of workers to launch in each controller selected.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    launch = function(n = 1L, controllers = NULL) {
      control <- private$select_controllers(controllers)
      walk(control, ~.x$launch(n = n))
    },
    #' @description Automatically scale up the number of workers if needed
    #'   in one or more controller objects.
    #' @details See the `scale()` method in individual controller classes.
    #' @return `NULL` (invisibly).
    #' @param throttle Logical of length 1, whether to delay auto-scaling
    #'   until the next auto-scaling request at least
    #'  `self$client$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    scale = function(throttle = FALSE, controllers = NULL) {
      control <- private$select_controllers(controllers)
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
    #'   seed to temporarily set for the evaluation of the task.
    #'   At the end of the task, the seed is restored.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param seconds_timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand.
    #'   If `TRUE`, then `collect()` runs first
    #'   so demand can be properly assessed before scaling and the number
    #'   of workers is not too high.
    #' @param throttle If `scale` is `TRUE`, whether to defer auto-scaling
    #'   until the next auto-scaling request at least
    #'   `self$client$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
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
      seed = sample.int(n = 1e9L, size = 1L),
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
        "select_single_controller"
      )(name = controller)
      .subset2(control, "push")(
        command = command,
        data = data,
        globals = globals,
        substitute = FALSE,
        seed = seed,
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
    #'   field of the client object.
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
    #'     a value.
    #'   * `"warn"`: throw a warning. This allows the return value with
    #'     all the error messages and tracebacks to be generated.
    #'   * `"silent"`: do nothing special.
    #' @param verbose Logical of length 1, whether to print progress messages.
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
      if (substitute) {
        command <- substitute(command)
      }
      control <- private$select_single_controller(name = controller)
      control$map(
        command = command,
        iterate = iterate,
        data = data,
        globals = globals,
        substitute = FALSE,
        seed = seed,
        packages = packages,
        library = library,
        seconds_interval = seconds_interval,
        seconds_timeout = seconds_timeout,
        names = names,
        save_command = save_command,
        error = error
      )
    },
    #' @description Check for done tasks and move the results to
    #'   the results list.
    #' @return `NULL` (invisibly). Removes elements from the `queue`
    #'   list as applicable and moves them to the `results` list.
    #' @param throttle whether to defer task collection
    #'   until the next task collection request at least
    #'   `self$client$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    collect = function(throttle = FALSE, controllers = NULL) {
      control <- private$select_controllers(controllers)
      walk(control, ~.x$collect(throttle = throttle))
    },
    #' @description Pop a completed task from the results data frame.
    #' @return If there is a completed task available to collect, the return
    #'   value is a one-row data frame with the results, warnings, and errors.
    #'   Otherwise, if there are no results available to collect,
    #'   the return value is `NULL`.
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand.
    #'   If `TRUE`, then `collect()` runs first
    #'   so demand can be properly assessed before scaling and the number
    #'   of workers is not too high. Scaling up on `pop()` may be important
    #'   for transient or nearly transient workers that tend to drop off
    #'   quickly after doing little work.
    #' @param collect Logical of length 1. If `scale` is `FALSE`,
    #'   whether to call `collect()`
    #'   to pick up the results of completed tasks. This task collection
    #'   step always happens (with throttling) when `scale` is `TRUE`.
    #' @param throttle If `scale` is `TRUE`, whether to defer auto-scaling
    #'   until the next auto-scaling request at least
    #'   `self$client$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    pop = function(
      scale = TRUE,
      collect = TRUE,
      throttle = TRUE,
      controllers = NULL
    ) {
      control <- private$select_controllers(controllers)
      for (controller in control) {
        out <- controller$pop(
          scale = scale,
          collect = collect,
          throttle = throttle
        )
        if (!is.null(out)) {
          return(out)
        }
      }
      NULL
    },
    #' @description Wait for tasks.
    #' @details The `wait()` method blocks the calling R session,
    #'   repeatedly auto-scales workers for tasks
    #'   that need them, and repeatedly collects results.
    #'   The function runs until it either times out or one of the
    #'   controllers reaches the stopping condition
    #'   based on the `mode` argument.
    #' @return `NULL` (invisibly). Call `pop()` to get the result.
    #' @param mode If `mode` is `"all"`,
    #'   then the method waits for all tasks to complete. If `mode` is
    #'   `"one"`, then it waits until a one task is complete.
    #' @param seconds_interval Number of seconds to wait between
    #'   polling intervals while checking for results.
    #'   Defaults to a fixed value because the internal `seconds_interval`
    #'   field may be different from controller to controller.
    #' @param seconds_timeout Timeout length in seconds waiting for
    #'   results to become available.
    #' @param scale Logical of length 1, whether to call `scale_later()`
    #'   on each selected controller to schedule auto-scaling.
    #'   By design, auto-scaling might not actually happen on
    #'   every iteration if `throttle` is `TRUE`.
    #' @param throttle If `scale` is `TRUE`, whether to defer auto-scaling
    #'   and task collection until the next request at least
    #'   `self$client$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency. Highly recommended to keep
    #'   the default `throttle = TRUE` for `wait()`.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    wait = function(
      mode = "all",
      seconds_interval = 0.01,
      seconds_timeout = Inf,
      scale = TRUE,
      throttle = TRUE,
      controllers = NULL
    ) {
      mode <- as.character(mode)
      crew_assert(mode, identical(., "all") || identical(., "one"))
      control <- private$select_controllers(controllers)
      tryCatch(
        crew_retry(
          fun = ~{
            empty <- TRUE
            for (controller in control) {
              controller$schedule$collect(throttle = throttle)
              if (scale) {
                controller$scale(throttle = throttle)
              }
              done <- controller$schedule$collected_mode(mode = mode)
              empty <- controller$schedule$empty()
              if (done && !empty) {
                return(TRUE)
              }
              empty <- empty && controller$schedule$empty()
            }
            empty
          },
          seconds_interval = seconds_interval,
          seconds_timeout = seconds_timeout
        ),
        crew_expire = function(condition) NULL
      )
      invisible()
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
      control <- private$select_controllers(controllers)
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
      walk(private$select_controllers(controllers), ~.x$terminate())
    }
  )
)
