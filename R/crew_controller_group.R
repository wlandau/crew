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
    controller_names = function(names = NULL) {
      names <- as.character(names %|||% names(self$controllers))
      true(
        names,
        is.character(.), !anyNA(.), nzchar(.),
        message = "invalid controller names."
      )
      true(
        all(names %in% names(self$controllers)),
        message = "bad controller names."
      )
      true(length(names) > 0L, message = "no controllers selected.")
      names
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
    #'   If `NULL`, it defaults to all controllers in the list.
    start = function(controllers = NULL) {
      control <- self$controllers[private$controller_names(controllers)]
      walk(control, ~.x$start())
    },
    #' @description Launch one or more workers on one or more controllers.
    #' @return `NULL` (invisibly).
    #' @param n Number of workers to launch in each controller selected.
    #' @param controllers Character vector of controller names.
    #'   If `NULL`, it defaults to all controllers in the list.
    launch = function(n = 1L, controllers = NULL) {
      control <- self$controllers[private$controller_names(controllers)]
      walk(control, ~.x$launch(n = n))
    },
    #' @description Check for done tasks and move the results to
    #'   the results list.
    #' @return `NULL` (invisibly). Removes elements from the `queue`
    #'   list as applicable and moves them to the `results` list.
    #' @param controllers Character vector of controller names.
    #'   If `NULL`, it defaults to all controllers in the list.
    collect = function(controllers = NULL) {
      control <- self$controllers[private$controller_names(controllers)]
      walk(control, ~.x$collect())
    },
    #' @description Automatically scale up the number of workers if needed
    #'   in one or more controller objects.
    #' @details See the `scale()` method in individual controller classes.
    #' @return `NULL` (invisibly).
    #' @param controllers Character vector of controller names.
    #'   If `NULL`, it defaults to all controllers in the list.
    scale = function(controllers = NULL) {
      control <- self$controllers[private$controller_names(controllers)]
      walk(control, ~.x$scale())
    },
    #' @description Push a task to the head of the task list.
    #' @return `NULL` (invisibly).
    #' @param command R code with the task to run.
    #' @param args A list of objects referenced in `expr`.
    #' @param timeout Time in milliseconds for the task.
    #' @param name Optional name of the task. Replaced with a random name
    #'   if `NULL` or in conflict with an existing name in the task list.
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. If `TRUE`, then `collect()` runs first so demand can be
    #'   properly assessed before scaling.
    #' @param controller Characte of length 1,
    #'   name of the controller to submit the task.
    #'   If `NULL`, the controller defaults to the
    #'   first controller in the list.
    push = function(
      command,
      args = list(),
      timeout = NULL,
      name = NULL,
      scale = TRUE,
      controller = NULL
    ) {
      controller <- utils::head(private$controller_names(controller), n = 1L)
      true(length(controller) == 1L)
      args <- list(
        command = substitute(command),
        args = args,
        timeout = timeout,
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
    #' @param controllers Names of the controllers (in order) to look for
    #'   completed tasks.
    pop = function(scale = TRUE, controllers = NULL) {
      control <- self$controllers[private$controller_names(controllers)]
      for (controller in control) {
        out <- controller$pop(scale = scale)
        if (!is.null(out)) {
          return(out)
        }
      }
      NULL
    },
    #' @description Wait for a result to be ready.
    #' @return `NULL` (invisibly). Call `pop()` to get the result.
    #' @param timeout Timeout length in seconds waiting for
    #'   results to become available.
    #' @param wait Number of seconds to wait between polling intervals
    #'   while checking for results.
    #' @param controllers Names of the controllers to wait for.
    wait = function(timeout = Inf, wait = 0.1, controllers = NULL) {
      control <- self$controllers[private$controller_names(controllers)]
      crew_wait(
        fun = ~{
          for (controller in control) {
            controller$collect()
            if (length(controller$results) > 0L) {
              return(TRUE)
            }
          }
          FALSE
        },
        timeout = timeout,
        wait = wait
      )
      invisible()
    },
    #' @description Summarize the workers of one or more controllers.
    #' @return A data frame of summary statistics on the workers and tasks.
    #' @param columns Tidyselect expression to select a subset of columns.
    #'   Examples include `columns = contains("worker")` and
    #'   `columns = starts_with("tasks")`.
    #' @param controllers Names of the controllers to summarize.
    summary = function(
      columns = tidyselect::everything(),
      controllers = NULL
    ) {
      control <- self$controllers[private$controller_names(controllers)]
      columns <- rlang::enquo(columns)
      out <- map(
        control,
        ~do.call(what = .x$summary, args = list(columns = columns))
      )
      if (all(map_lgl(out, is.null))) {
        return(NULL)
      }
      out <- tibble::as_tibble(do.call(what = rbind, args = out))
    },
    #' @description Terminate the workers and disconnect the router
    #'   for one or more controllers.
    #' @return `NULL` (invisibly).
    #' @param controllers Names of the controllers to terminate.
    terminate = function(controllers = NULL) {
      control <- self$controllers[private$controller_names(controllers)]
      walk(control, ~.x$terminate())
    }
  )
)
