#' @title Create a `mirai` controller.
#' @export
#' @keywords internal
#' @family mirai
#' @description Create an `R6` object to submit tasks and launch workers
#'   on `mirai`-based infrastructure.
#' @param router An `R6` router object created by [crew_mirai_router()].
#' @param launcher An `R6` launcher object created by one of the
#'   `crew_mirai_launcher_*()` functions such as [crew_mirai_launcher_callr()].
#' @param scale_method Character of length 1, name of the method for
#'   automatically scaling workers to meet demand. `NULL` to default to
#'   `"demand"`. Possible values include the following:
#'   * `"demand"`: just after pushing a new task in `push()`, launch
#'     `min(n, max(0, t - w))` workers, where `n` is the maximum number of
#'     workers, `t` is the number of queued tasks, and `w` is the current
#'     number of workers already running. In other words, scale up the
#'     number of workers to meet the current demand.
#'   * `"single"`: just after pushing a new task in `push()`, launch
#'     a single worker if demand `min(n, max(0, t - w))` is greater than 0.
#'   * `"none"`: do not auto-scale at all.
crew_mirai_controller <- function(
  router,
  launcher,
  scale_method = "demand"
) {
  scale_method <- scale_method %|||% scale_methods_mirai
  controller <- crew_class_mirai_controller$new(
    router = router,
    launcher = launcher,
    scale_method = scale_method
  )
  controller$validate()
  controller
}

#' @title `mirai` controller class
#' @export
#' @family mirai
#' @description `R6` class for `mirai` controllers.
#' @details See [crew_mirai_controller()].
crew_class_mirai_controller <- R6::R6Class(
  classname = "crew_class_mirai_controller",
  public = list(
    #' @field router Router object.
    router = NULL,
    #' @field launcher Launcher object.
    launcher = NULL,
    #' @field scale_method Scaling method. See [crew_mirai_controller()].
    scale_method = NULL,
    #' @field queue List of tasks in the queue.
    queue = list(),
    #' @field results List of finished tasks
    results = list(),
    #' @description `mirai` controller constructor.
    #' @return An `R6` object with the controller object.
    #' @param router Router object. See [crew_mirai_controller()].
    #' @param launcher Launcher object. See [crew_mirai_controller()].
    #' @param scale_method Scaling method. See [crew_mirai_controller()].
    initialize = function(
      router = NULL,
      launcher = NULL,
      scale_method = NULL
    ) {
      self$router <- router
      self$launcher <- launcher
      self$scale_method <- scale_method
      invisible()
    },
    #' @description Validate the router.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(is.list(self$queue))
      true(is.list(self$results))
      true(is_router(self$router))
      true(is_launcher(self$launcher))
      self$router$validate()
      self$launcher$validate()
      invisible()
    },
    #' @description Connect the router and register the TCP sockets
    #'   with the launcher.
    #' @return `NULL` (invisibly).
    connect = function() {
      self$router$connect()
      self$launcher$populate(sockets = self$router$sockets_listening())
      invisible()
    },
    #' @description Launch one or more workers.
    #' @details The actual number of newly launched workers
    #'   is capped at the max set when the controller
    #'   was created. Unless you are launching an array of
    #'   persistent workers or have `scale_method = "none"`,
    #'   this method does not usually need to be called
    #'   directly because `push(scale = TRUE)` already launches
    #'   workers depending on `scale_method`.
    #' @return `NULL` (invisibly).
    #' @param n Maximum number of workers to launch.
    launch = function(n = 1L) {
      self$launcher$launch(n = n)
    },
    #' @description Check for done tasks and move the results to
    #'   the results list.
    #' @return `NULL` (invisibly). Removes elements from the `queue`
    #'   list as applicable and moves them to the `results` list.
    collect = function() {
      done <- integer(0L)
      for (index in seq_along(self$queue)) {
        task <- self$queue[[index]]
        result <- task$handle[[1]]$data
        if (!mirai::unresolved(result)) {
          # Happens if mirai cannot evaluate the command.
          # I cannot cover this in automated tests, but
          # I did test it by hand.
          # nocov start
          if (!inherits(result, "crew_monad")) {
            result <- monad_init(
              command = task$command,
              error = utils::capture.output(print(result), type = "output")
            )
          }
          # nocov end
          result$name <- task$name
          self$results[[length(self$results) + 1L]] <- result
          done <- c(done, index)
        }
      }
      self$queue[done] <- NULL
      invisible()
    },
    #' @description Automatically scale up the number of workers if needed.
    #' @details This method is called during `push()`, and the method for
    #'   scaling up workers is governed by the `scale_method`
    #'   argument of [crew_mirai_controller()]. It is not meant to be called
    #'   manually. If called manually, it is recommended to call `collect()`
    #'   first so `scale()` can accurately assess the demand.
    #'   For finer control of the number of workers launched,
    #'   call `launch()` on the controller with the exact desired
    #'   number of workers.
    #' @return `NULL` (invisibly).
    scale = function() {
      demand <- max(0L, length(self$queue) - self$launcher$running())
      n <- switch(
        self$scale_method,
        demand = demand,
        single = min(1L, demand),
      ) %|||% 0L
      self$launcher$launch(n = n)
      invisible()
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
    push = function(
      command,
      args = list(),
      timeout = NULL,
      name = NULL,
      scale = TRUE
    ) {
      true(scale, isTRUE(.) || isFALSE(.))
      message <- "router must be connected to push tasks to the queue."
      true(self$router$is_connected(), message = message)
      while (is.null(name) || name %in% self$queue$name) name <- random_name()
      command <- substitute(command)
      expr <- rlang::call2("quote", command)
      expr <- rlang::call2(quote(crew::crew_eval), expr)
      mirai_args <- list(
        .expr = expr,
        .args = args,
        .timeout = timeout,
        .compute = self$router$name
      )
      handle <- do.call(what = mirai::mirai, args = mirai_args)
      command <- deparse_safe(command)
      task <- tibble::tibble(
        name = name,
        command = command,
        handle = list(handle)
      )
      self$queue[[length(self$queue) + 1L]] <- task
      if (scale) {
        self$collect()
        self$scale()
      }
      invisible()
    },
    #' @description Pop a completed task from the results data frame.
    #' @return If there is a completed task available to collect, the return
    #'   value is a one-row data frame with the results, warnings, and errors.
    #'   Otherwise, if there are no results available to collect,
    #'   the return value is `NULL`.
    #' @param collect Whether to run the `collect()` method to collect all
    #'   available results before calling `pop()`.
    pop = function(collect = TRUE) {
      true(collect, isTRUE(.) || isFALSE(.))
      if (collect) {
        self$collect()
      }
      out <- NULL
      if (length(self$results) > 0L) {
        out <- self$results[[1]]
        self$results[[1]] <- NULL
      }
      out
    },
    #' @description Wait for a result to be ready.
    #' @return `NULL` (invisibly). Call `pop()` to get the result.
    #' @param timeout Timeout length in seconds waiting for
    #'   results to become available.
    #' @param wait Number of seconds to wait between polling intervals
    #'   while checking for results.
    wait = function(timeout = Inf, wait = 0.1) {
      crew_wait(
        fun = ~{
          self$collect()
          length(self$results) > 0L
        },
        timeout = timeout,
        wait = wait
      )
      invisible()
    },
    #' @description Terminate the workers and disconnect the router.
    #' @return `NULL` (invisibly).
    terminate = function() {
      self$launcher$terminate()
      self$router$disconnect()
      invisible()
    }
  )
)

scale_methods_mirai <- c("demand", "single", "none")

#' @export
#' @keywords internal
is_controller.crew_class_mirai_controller <- function(x) {
  TRUE
}
