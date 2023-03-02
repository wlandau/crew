#' @title Create a `mirai` controller.
#' @export
#' @family mirai
#' @description Create an `R6` object to submit tasks and launch workers
#'   on `mirai`-based infrastructure.
#' @param router An `R6` router object created by [crew_mirai_router()].
#' @param launcher An `R6` launcher object created by one of the
#'   `crew_mirai_launcher_*()` functions.
#' @examples
#' # TBD
crew_mirai_controller <- function(router, launcher) {
  controller <- crew_class_mirai_controller$new(
    router = router,
    launcher = launcher
  )
  controller$validate()
  controller
}

#' @title `mirai` controller class
#' @export
#' @family mirai-classes
#' @description `R6` class for `mirai` controllers.
#' @details See [crew_mirai_controller()].
crew_class_mirai_controller <- R6::R6Class(
  classname = "crew_class_mirai_controller",
  public = list(
    #' @field router Router object.
    router = NULL,
    #' @field launcher Launcher object.
    launcher = NULL,
    #' @field tasks Data frame of tasks in the queue.
    tasks = tibble::tibble(
      name = character(0),
      command = character(0),
      handle = list()
    ),
    #' @description `mirai` controller constructor.
    #' @return An `R6` object with the controller object.
    #' @param router Router object. See [crew_mirai_controller()].
    #' @param launcher Launcher object. See [crew_mirai_controller()].
    initialize = function(
      router = NULL,
      launcher = NULL
    ) {
      self$router <- router
      self$launcher <- launcher
      invisible()
    },
    #' @description Disconnect at garbage collection time.
    #' @return `NULL` (invisibly).
    finalize = function() {
      self$router$disconnect()
    },
    #' @description Validate the router.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(!is.null(self$router))
      true(!is.null(self$launcher))
      self$router$validate()
      self$launcher$validate()
      invisible()
    },
    #' @description Push a task to the head of the task list.
    #' @return `NULL` (invisibly).
    #' @param command R code with the task to run.
    #' @param args A list of objects referenced in `expr`.
    #' @param timeout Time in milliseconds for the task.
    #' @param name Optional name of the task. Replaced with a random name
    #'   if `NULL` or in conflict with an existing name in the task list.
    push = function(command, args = list(), timeout = NULL, name = NULL) {
      message <- "router must be connected to push tasks."
      true(self$connected(), message = message)
      while (is.null(name) || name %in% self$tasks$name) name <- random_name()
      monad <- as.call(list(quote(crew::crew_eval), substitute(command)))
      handle <- mirai::mirai(.expr = monad, .args = args, .timeout = timeout)
      tibble::add_row(
        self$tasks,
        name = name,
        command = deparse_safe(command),
        handle = handle,
        .before = 1L
      )
      invisible()
    },
    #' @description Pop a completed task from as near as possible to the tail
    #'   of the task list.
    #' @return If there is a completed task available to collect, the return
    #'   value is a list object with the results, warnings, and errors.
    #'   Otherwise, if there are no tasks or if all tasks are still running,
    #'   the return value is `NULL`.
    pop = function() {
      for (index in rev(seq_len(nrow(self$tasks)))) {
        task <- self$tasks[[index]]
        data <- task$handle$data
        if (!inherits(data, "unresolvedValue")) {
          if (inherits(data, "crew_monad")) {
            out <- monad_mutate(monad = data, name = task$name)
          } else {
            out <- monad_init(
              name = task$name,
              command = task$command,
              error = utils::capture.output(print(data), type = "output")
            )
          }
          return(out)
        }
      }
      NULL
    }
  )
)
