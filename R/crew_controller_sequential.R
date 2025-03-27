#' @title Create a sequential controller.
#' @export
#' @family sequential controllers
#' @description The sequential controller runs tasks on the same R process
#'   where the controller object exists. Tasks run sequentially
#'   rather than in parallel.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' controller <- crew_controller_sequential()
#' controller$push(name = "task", command = sqrt(4))
#' controller$pop()
#' }
crew_controller_sequential <- function() {
  controller <- crew_class_controller_sequential$new(
    client = crew_client(),
    launcher = crew_launcher(),
    crashes_max = 0L
  )
  controller$validate()
  controller
}

#' @title Sequential controller class
#' @export
#' @family sequential controllers
#' @description `R6` class for sequential controllers.
#' @details See [crew_controller_sequential()].
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' controller <- crew_controller_sequential()
#' controller$push(name = "task", command = sqrt(4))
#' controller$pop()
#' }
crew_class_controller_sequential <- R6::R6Class(
  classname = "crew_class_controller_sequential",
  inherit = crew_class_controller,
  cloneable = FALSE,
  public = list(
    #' @description Start the controller if it is not already started.
    #' @details For the sequential controller, there is nothing to do
    #'   except register the client as started.
    #' @return `NULL` (invisibly).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    start = function(controllers = NULL) {
      if (!.subset2(.subset2(self, "client"), "started")) {
        private$.client$set_started()
        private$.register_started()
      }
      invisible()
    },
    #' @description Does nothing for the sequential controller.
    #' @return `NULL` (invisibly).
    #' @param n Number of workers to launch.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    launch = function(n = 1L, controllers = NULL) {
      invisible()
    },
    #' @description Does nothing for the sequential controller.
    #' @return Invisibly returns `FALSE`.
    #' @param throttle Not applicable to the sequential controller.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    scale = function(throttle = TRUE, controllers = NULL) {
      invisible(FALSE)
    },
    #' @description Not applicable to the sequential controller.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    #' @return `NULL` (invisibly).
    autoscale = function(controllers = NULL) {
      invisible()
    },
    #' @description Not applicable to the sequential controller.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    #' @return `NULL` (invisibly).
    descale = function(controllers = NULL) {
      invisible()
    },
    #' @description Push a task to the head of the task list.
    #' @return Invisibly returns a `mirai`-like list where the `data`
    #'   element is the result of the task.
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
    #'   If `algorithm` and `seed` are both `NULL` for the sequential
    #'   controller, then the random number generator defaults to the
    #'   current RNG of the local R session where the sequential
    #'   controller lives.
    #' @param algorithm Integer of length 1 with the pseudo-random number
    #'   generator algorithm to set for the evaluation of the task.
    #'   Passed to the `kind` argument of `RNGkind()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL` for the sequential
    #'   controller, then the random number generator defaults to the
    #'   current RNG of the local R session where the sequential
    #'   controller lives.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param seconds_timeout Not used in the sequential controller..
    #' @param scale Not used in the sequential controller.
    #' @param throttle Not used in the sequential controller.
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
      .subset2(self, "start")()
      name <- private$.name_new_task(name)
      if (substitute) {
        command <- substitute(command)
      }
      task <- structure(
        list(
          data = crew::crew_eval(
            name = name,
            command = command,
            data = data,
            globals = globals,
            seed = seed,
            algorithm = algorithm,
            packages = packages,
            library = library
          )
        ),
        class = "mirai"
      )
      .subset2(private, ".push_task")(name, task)
      client <- .subset2(private, ".client")
      nanonext::cv_signal(.subset2(client, "condition"))
      invisible(task)
    },
    #' @description Not applicable to the sequential controller.
    #' @return Always returns `TRUE` (invisibly)
    #'   for the sequential controller.
    #' @param mode Not applicable to the sequential controller.
    #' @param seconds_interval Not applicable to the sequential controller.
    #' @param seconds_timeout Not applicable to the sequential controller.
    #' @param scale Not applicable to the sequential controller.
    #' @param throttle Not applicable to the sequential controller.
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
      invisible(TRUE)
    },
    #' @description Not applicable to the sequential controller.
    #' @param name Character of length 1 with the task name to push to
    #'   the backlog.
    #' @return `NULL` (invisibly).
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    push_backlog = function(name, controller = NULL) {
      invisible()
    },
    #' @description  Not applicable to the sequential controller.
    #' @return Always `character(0L)` for the sequential controller.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    pop_backlog = function(controllers = NULL) {
      character(0L)
    },
    #' @description Not applicable to the sequential controller.
    #' @param names Not applicable to the sequential controller.
    #' @param all Not applicable to the sequential controller.
    cancel = function(names = character(0L), all = FALSE) {
      invisible()
    }
  )
)
