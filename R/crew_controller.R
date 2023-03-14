#' @title Create a controller.
#' @export
#' @keywords internal
#' @family controllers
#' @description Create an `R6` object to submit tasks and launch workers.
#' @param router An `R6` router object created by [crew_router()].
#' @param launcher An `R6` launcher object created by one of the
#'   `crew_launcher_*()` functions such as [crew_launcher_callr()].
#' @param auto_scale Character of length 1, name of the method for
#'   automatically scaling workers to meet demand. `NULL` to default to
#'   `"demand"`. Possible values include the following:
#'   * `"demand"`: just after pushing a new task in `push()`, launch
#'     `min(n, max(0, t - w))` workers, where `n` is the maximum number of
#'     workers, `t` is the number of queued tasks, and `w` is the current
#'     number of workers already running. In other words, scale up the
#'     number of workers to meet the current demand.
#'     If you trust tasks not to crash workers, this is a good choice.
#'     But if you think a task may always crash a worker
#'     (e.g. segmentation fault or maxed out memory) then
#'     this could be somewhat risky because `mirai` resubmits
#'     failed tasks behind the scenes and `crew` responds by
#'     re-launching workers. If you are worried about this scenario,
#'     choose `auto_scale = "single"` instead, which will only launch
#'     up to one worker whenever a task is pushed.
#'   * `"single"`: just after pushing a new task in `push()`, launch
#'     a single worker if demand `min(n, max(0, t - w))` is greater than 0.
#'   * `"none"`: do not auto-scale at all.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_session_start()
#' router <- crew_router()
#' launcher <- crew_launcher_callr()
#' controller <- crew_controller(router = router, launcher = launcher)
#' controller$start()
#' controller$push(name = "task", command = sqrt(4))
#' controller$wait()
#' controller$pop()
#' controller$terminate()
#' crew_session_terminate()
#' }
crew_controller <- function(
  router,
  launcher,
  auto_scale = "demand"
) {
  auto_scale <- auto_scale %|||% c("demand", "single", "none")
  controller <- crew_class_controller$new(
    router = router,
    launcher = launcher,
    auto_scale = auto_scale
  )
  controller$validate()
  controller
}

#' @title Controller class
#' @export
#' @family controllers
#' @description `R6` class for controllers.
#' @details See [crew_controller()].
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_session_start()
#' router <- crew_router()
#' launcher <- crew_launcher_callr()
#' controller <- crew_controller(router = router, launcher = launcher)
#' controller$start()
#' controller$push(name = "task", command = sqrt(4))
#' controller$wait()
#' controller$pop()
#' controller$terminate()
#' crew_session_terminate()
#' }
crew_class_controller <- R6::R6Class(
  classname = "crew_class_controller",
  public = list(
    #' @field router Router object.
    router = NULL,
    #' @field launcher Launcher object.
    launcher = NULL,
    #' @field auto_scale Scaling method. See [crew_controller()].
    auto_scale = NULL,
    #' @field queue List of tasks in the queue.
    queue = list(),
    #' @field results List of finished tasks
    results = list(),
    #' @description `mirai` controller constructor.
    #' @return An `R6` object with the controller object.
    #' @param router Router object. See [crew_controller()].
    #' @param launcher Launcher object. See [crew_controller()].
    #' @param auto_scale Scaling method. See [crew_controller()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' crew_session_start()
    #' router <- crew_router()
    #' launcher <- crew_launcher_callr()
    #' controller <- crew_controller(router = router, launcher = launcher)
    #' controller$start()
    #' controller$push(name = "task", command = sqrt(4))
    #' controller$wait()
    #' controller$pop()
    #' controller$terminate()
    #' crew_session_terminate()
    #' }
    initialize = function(
      router = NULL,
      launcher = NULL,
      auto_scale = NULL
    ) {
      self$router <- router
      self$launcher <- launcher
      self$auto_scale <- auto_scale
      invisible()
    },
    #' @description Validate the router.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(is.list(self$queue))
      true(is.list(self$results))
      true(inherits(self$router, "crew_class_router"))
      true(inherits(self$launcher, "crew_class_launcher"))
      self$router$validate()
      self$launcher$validate()
      invisible()
    },
    #' @description Start the controller.
    #' @details Register the mirai client and register worker websockets
    #'   with the launcher.
    #' @return `NULL` (invisibly).
    start = function() {
      self$router$listen()
      self$launcher$populate(sockets = self$router$sockets)
      invisible()
    },
    #' @description Force terminate workers whose startup time has elapsed
    #'   and are not connected to the `mirai` client.
    #' @return `NULL` (invisibly).
    clean = function() {
      inactive <- setdiff(self$router$sockets, self$launcher$active())
      self$launcher$terminate(sockets = inactive)
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
    launch = function(n = 1L) {
      inactive <- setdiff(self$router$sockets, self$launcher$active())
      sockets <- utils::head(inactive, n = n)
      if (length(sockets) > 0L) {
        self$launcher$launch(sockets = sockets)
      }
      invisible()
    },
    #' @description Run auto-scaling.
    #' @details This method is called during `push()`, and the method for
    #'   scaling up workers is governed by the `auto_scale`
    #'   argument of [crew_controller()]. It is not meant to be called
    #'   manually. If called manually, it is recommended to call `collect()`
    #'   first so `scale()` can accurately assess the demand.
    #'   For finer control of the number of workers launched,
    #'   call `launch()` on the controller with the exact desired
    #'   number of workers.
    #' @return `NULL` (invisibly).
    scale = function() {
      demand <- controller_demand(
        tasks = length(self$queue),
        workers = length(self$launcher$active())
      )
      n_new_workers <- controller_n_new_workers(
        demand = demand,
        auto_scale = self$auto_scale
      )
      if (n_new_workers > 0L) {
        self$launch(n = n_new_workers)
      }
      invisible()
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
    #' @description Push a task to the head of the task list.
    #' @return `NULL` (invisibly).
    #' @param command R code with the task to run.
    #' @param args A list of objects referenced in `expr`.
    #' @param timeout Time in milliseconds for the task.
    #' @param name Optional name of the task. Replaced with a random name
    #'   if `NULL` or in conflict with an existing name in the task list.
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. If `TRUE`, then `clean()` and `collect()` run first
    #'   so demand can be properly assessed before scaling and the number
    #'   of workers is not too high.
    push = function(
      command,
      args = list(),
      timeout = NULL,
      name = NULL,
      scale = TRUE
    ) {
      true(scale, isTRUE(.) || isFALSE(.))
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
      task <- list(
        name = name,
        command = command,
        handle = list(handle)
      )
      self$queue[[length(self$queue) + 1L]] <- task
      if (scale) {
        self$clean()
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
      if (isTRUE(collect)) {
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
    #' @description Terminate the workers and the `mirai` client.
    #' @return `NULL` (invisibly).
    terminate = function() {
      self$router$terminate()
      self$launcher$terminate()
      invisible()
    }
  )
)

controller_demand <- function(tasks, workers) {
  max(0L, tasks - workers)
}

controller_n_new_workers <- function(demand, auto_scale) {
  switch(
    auto_scale,
    demand = demand,
    single = min(1L, demand),
    none = 0L
  ) %|||% 0L
}
