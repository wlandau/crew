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
#' crew_session_start()
#' persistent <- crew_controller_callr(name = "persistent")
#' transient <- crew_controller_callr(
#'   name = "transient",
#'   max_tasks = 1L
#' )
#' group <- crew_controller_group(persistent, transient)
#' group$connect()
#' group$push(name = "task", command = sqrt(4), controller = "transient")
#' group$wait()
#' group$pop()
#' group$controllers[["persistent"]]$launcher$running() # 0
#' group$controllers[["transient"]]$launcher$running() # 0
#' group$terminate()
#' crew_session_terminate()
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
#' crew_session_start()
#' persistent <- crew_controller_callr(name = "persistent")
#' transient <- crew_controller_callr(
#'   name = "transient",
#'   max_tasks = 1L
#' )
#' group <- crew_controller_group(persistent, transient)
#' group$connect()
#' group$push(name = "task", command = sqrt(4), controller = "transient")
#' group$wait()
#' group$pop()
#' group$controllers[["persistent"]]$launcher$running() # 0
#' group$controllers[["transient"]]$launcher$running() # 0
#' group$terminate()
#' crew_session_terminate()
#' }
crew_class_controller_group <- R6::R6Class(
  classname = "crew_class_controller_group",
  private = list(
    select_controllers = function(names) {
      if (is.null(names)) {
        return(self$controllers)
      }
      message <- "'controllers' must be a valid nonempty character vector."
      true(
        names,
        length(.) > 0L,
        is.character(.),
        !anyNA(.),
        nzchar(.),
        message = message
      )
      invalid <- setdiff(names, names(self$controllers))
      true(
        !length(invalid),
        message = sprintf(
          "controllers not found: %s",
          paste(invalid, collapse = ", ")
        )
      )
      self$controllers[names]
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
    #' crew_session_start()
    #' persistent <- crew_controller_callr(name = "persistent")
    #' transient <- crew_controller_callr(
    #'   name = "transient",
    #'   max_tasks = 1L
    #' )
    #' group <- crew_controller_group(persistent, transient)
    #' group$connect()
    #' group$push(name = "task", command = sqrt(4), controller = "transient")
    #' group$wait()
    #' group$pop()
    #' group$controllers[["persistent"]]$launcher$running() # 0
    #' group$controllers[["transient"]]$launcher$running() # 0
    #' group$terminate()
    #' crew_session_terminate()
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
      true(
        map_lgl(self$controllers, ~inherits(.x, "crew_class_controller")),
        message = "All objects in a controller group must be controllers."
      )
      out <- unname(map_chr(self$controllers, ~.x$router$name))
      exp <- names(self$controllers)
      true(identical(out, exp), message = "bad controller names")
      invisible()
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
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    scale = function(controllers = NULL) {
      control <- private$select_controllers(controllers)
      walk(control, ~.x$scale())
    },
    #' @description Check for done tasks and move the results to
    #'   the results list.
    #' @return `NULL` (invisibly). Removes elements from the `queue`
    #'   list as applicable and moves them to the `results` list.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    collect = function(controllers = NULL) {
      control <- private$select_controllers(controllers)
      walk(control, ~.x$collect())
    },
    #' @description Push a task to the head of the task list.
    #' @return `NULL` (invisibly).
    #' @param command Language object with R code to run.
    #' @param data Named list of local data objects in the
    #'   evaluation environment.
    #' @param globals Named list of objects to temporarily assign to the
    #'   global environment for the task. At the end of the task,
    #'   these values are reset to their previous values.
    #' @param seed Integer of length 1 with the pseudo-random number generator
    #'   seed to temporarily set for the evaluation of the task.
    #'   At the end of the task, the seed is restored.
    #' @param garbage_collection Logical, whether to run garbage collection
    #'   with `gc()` before running the task.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param name Optional name of the task. Replaced with a random name
    #'   if `NULL` or in conflict with an existing name in the task list.
    #' @param seconds_timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. If `TRUE`, then `clean()` and `collect()` run first
    #'   so demand can be properly assessed before scaling and the number
    #'   of workers is not too high.
    #' @param controller Character of length 1,
    #'   name of the controller to submit the task.
    #'   If `NULL`, the controller defaults to the
    #'   first controller in the list.
    push = function(
      command,
      data = list(),
      globals = list(),
      seed = sample.int(n = 1e9L, size = 1L),
      garbage_collection = FALSE,
      packages = character(0),
      library = NULL,
      seconds_timeout = NULL,
      name = NULL,
      scale = TRUE,
      controller = NULL
    ) {
      controller <- controller %|||%
        utils::head(names(self$controllers), n = 1L)
      true(
        length(controller) == 1L,
        message = "controller argument of push() must have length 1."
      )
      true(
        controller %in% names(self$controllers),
        message = sprintf("controller \"%s\" not found", controller)
      )
      args <- list(
        command = substitute(command),
        data = data,
        globals = globals,
        seed = seed,
        garbage_collection = garbage_collection,
        packages = packages,
        library = library,
        seconds_timeout = seconds_timeout,
        name = name,
        scale = scale
      )
      do.call(what = self$controllers[[controller]]$push, args = args)
    },
    #' @description Pop a completed task from the results data frame.
    #' @return If there is a completed task available to collect, the return
    #'   value is a one-row data frame with the results, warnings, and errors.
    #'   Otherwise, if there are no results available to collect,
    #'   the return value is `NULL`.
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. If `TRUE`, then `clean()` and `collect()` run first
    #'   so demand can be properly assessed before scaling and the number
    #'   of workers is not too high. Scaling up on `pop()` may be important
    #'   for transient or nearly transient workers that tend to drop off
    #'   quickly after doing little work.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    pop = function(scale = TRUE, controllers = NULL) {
      control <- private$select_controllers(controllers)
      for (controller in control) {
        out <- controller$pop(scale = scale)
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
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    wait = function(
      mode = "all",
      seconds_interval = 0.001,
      seconds_timeout = Inf,
      controllers = NULL
    ) {
      mode <- as.character(mode)
      true(mode, identical(., "all") || identical(., "one"))
      control <- private$select_controllers(controllers)
      tryCatch(
        crew_wait(
          fun = ~{
            for (controller in control) {
              controller$collect()
              controller$clean()
              controller$scale()
              done <- length(controller$results) > 0L
              if (identical(mode, "all")) {
                done <- done && (length(controller$queue) < 1L)
              }
              if (done) {
                return(TRUE)
              }
            }
            FALSE
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
