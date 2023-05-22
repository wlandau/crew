#' @title Create a controller group.
#' @export
#' @keywords internal
#' @family controllers
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
  names(controllers) <- map_chr(controllers, ~.x$router$name)
  out <- crew_class_controller_group$new(controllers = controllers)
  out$validate()
  out
}

#' @title Controller group class
#' @export
#' @family controllers
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
    #' @description Validate the router.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(
        map_lgl(self$controllers, ~inherits(.x, "crew_class_controller")),
        message = "All objects in a controller group must be controllers."
      )
      out <- unname(map_chr(self$controllers, ~.x$router$name))
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
    #'   until the next request at least `self$router$seconds_interval`
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
    #'  `self$router$seconds_interval` seconds from the original request.
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
    #'   `self$router$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param name Optional name of the task. Replaced with a random name
    #'   if `NULL` or in conflict with an existing name in the task list.
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
      controller = NULL
    ) {
      if (substitute) {
        command <- substitute(command)
      }
      control <- private$select_single_controller(name = controller)
      control$push(
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
        name = name
      )
    },
    #' @description Check for done tasks and move the results to
    #'   the results list.
    #' @return `NULL` (invisibly). Removes elements from the `queue`
    #'   list as applicable and moves them to the `results` list.
    #' @param throttle whether to defer task collection
    #'   until the next task collection request at least
    #'   `self$router$seconds_interval` seconds from the original request.
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
    #'   `self$router$seconds_interval` seconds from the original request.
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
    #' @param seconds_timeout Timeout length in seconds waiting for
    #'   results to become available.
    #' @param scale Logical of length 1, whether to call `scale_later()`
    #'   on each selected controller to schedule auto-scaling.
    #'   By design, auto-scaling might not actually happen on
    #'   every iteration if `throttle` is `TRUE`.
    #' @param throttle If `scale` is `TRUE`, whether to defer auto-scaling
    #'   and task collection until the next request at least
    #'   `self$router$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #'   Highly recommended to keep
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
              if_any(
                scale,
                controller$scale(throttle = throttle),
                controller$collect(throttle = throttle)
              )
              empty_queue <- length(controller$queue) < 1L
              empty_results <- length(controller$results) < 1L
              done <- if_any(
                identical(mode, "all"),
                empty_queue && (!empty_results),
                empty_queue || (!empty_results)
              )
              if (done) {
                return(TRUE)
              }
              empty <- empty && empty_queue && empty_results
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
    #'   of all the selected controllers. It has one row per worker
    #'   websocket, and the rows are grouped by controller.
    #'   See the documentation of the `summary()` method of the controller
    #'   class for specific information about the columns in the output.
    #' @param columns Tidyselect expression to select a subset of columns.
    #'   Examples include `columns = contains("worker")` and
    #'   `columns = starts_with("tasks")`.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    summary = function(
      columns = tidyselect::everything(),
      controllers = NULL
    ) {
      control <- private$select_controllers(controllers)
      columns <- rlang::enquo(columns)
      out <- map(
        control,
        ~do.call(what = .x$summary, args = list(columns = columns))
      )
      if (all(map_lgl(out, is.null))) {
        return(NULL)
      }
      tibble::as_tibble(do.call(what = rbind, args = out))
    },
    #' @description Terminate the workers and disconnect the router
    #'   for one or more controllers.
    #' @return `NULL` (invisibly).
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    terminate = function(controllers = NULL) {
      walk(private$select_controllers(controllers), ~.x$terminate())
    }
  )
)
