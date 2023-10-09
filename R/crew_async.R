#' @title Local asynchronous client object.
#' @export
#' @family developer
#' @description Create an `R6` object to manage local asynchronous quick
#'   tasks with error detection.
#' @details [crew_async()] objects are created inside launchers to allow
#'   launcher plugins to run local tasks asynchronously, such as
#'   calls to cloud APIs to launch serious remote workers.
#' @return An `R6` async client object.
#' @param workers Number of local `mirai` daemons to run asynchronous tasks.
#'   If `NULL`, then tasks will be evaluated synchronously.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' x <- crew_async()
#' x$start()
#' out <- x$eval(1 + 1)
#' mirai::call_mirai(out)
#' out$data # 2
#' x$errors() # 0
#' x$terminate()
#' }
crew_async <- function(workers = NULL) {
  async <- crew_class_async$new(workers = workers)
  async$validate()
  async
}

#' @title `R6` async class.
#' @export
#' @family class
#' @description `R6` class for async configuration.
#' @details See [crew_async()].
#' @examples
#' crew_async(mode = "automatic")
crew_class_async <- R6::R6Class(
  classname = "crew_class_async",
  cloneable = FALSE,
  public = list(
    #' @field workers See [crew_async()].
    workers = NULL,
    #' @field instance Character of length 1, name of the current instance.
    instance = NULL,
    #' @field url Local URL of the socket for error detection.
    url = NULL,
    #' @field socket `nanonext` socket for error detection.
    socket = NULL,
    #' @field condition `nanonext` condition variable for error detection.
    condition = NULL,
    #' @description TLS configuration constructor.
    #' @return An `R6` object with TLS configuration.
    #' @param workers Argument passed from [crew_async()].
    initialize = function(workers = NULL) {
      self$workers <- workers
    },
    #' @description Validate the object.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(
        self$workers %|||% 57L,
        is.numeric(.),
        length(.) == 1L,
        !anyNA(.)
      )
      for (field in c("instance", "url")) {
        crew_assert(
          self[[field]] %|||% "x",
          is.character(.),
          length(.) == 1L,
          !anyNA(.),
          nzchar(.)
        )
      }
      if (!is.null(self$socket)) {
        crew_assert(inherits(self$socket, "nanoSocket"))
      }
      if (!is.null(self$condition)) {
        crew_assert(inherits(self$condition, "conditionVariable"))
      }
      invisible()
    },
    #' @description Start the local workers and error handling socket.
    #' @details Does not create workers or an error handling socket
    #'   if `workers` is `NULL` or the object is already started.
    #' @return `NULL` (invisibly).
    start = function() {
      if (is.null(self$workers) || !is.null(self$instance)) {
        return(invisible())
      }
      self$instance <- crew::crew_random_name()
      mirai::daemons(
        n = self$workers,
        dispatcher = FALSE,
        .compute = self$instance
      )
      self$url <- gsub(
        pattern = "/[^/]*$",
        paste0("/", nanonext::random(n = 12)),
        mirai::status(.compute = self$instance)$daemons[1L],
      )
      self$socket <- nanonext::socket(protocol = "req", listen = self$url)
      self$reset()
      invisible()
    },
    #' @description Report the number of task errors signaled.
    #' @return Integer of length 1, number of task errors signaled.
    errors = function() {
      events <- if_any(
        is.null(self$condition),
        0L,
        nanonext::cv_value(self$condition)
      )
      as.integer((events / 2L) + (events %% 2L))
    },
    #' @description Reset the number of task errors signaled.
    #' @details Re-creates the condition variable.
    #' @return `NULL` (invisibly).
    reset = function() {
      self$condition <- nanonext::cv()
      nanonext::pipe_notify(socket = self$socket, cv = self$condition)
    },
    #' @description Run a local asynchronous task using a local
    #'   compute profile.
    #' @details Used for launcher plugins with asynchronous launches and
    #'   terminations. If `processes` is `NULL`, the task will run locally.
    #'   Otherwise, the task will run on a local process in the local
    #'   `mirai` compute profile.
    #' @return If the `processes` field is `NULL`, a list with an object named
    #'   `data` containing the result of evaluating `expr` synchronously.
    #'   Otherwise, the task is evaluated asynchronously, and the result
    #'   is a `mirai` task object. Either way, the `data` element
    #'   of the return value will contain the result of the task.
    #' @param command R code to run.
    #' @param substitute Logical of length 1, whether to substitute `command`.
    #'   If `FALSE`, then `command` must be an expression object
    #'   or language object.
    #' @param args Named list of data objects required to run `command`.
    #' @param packages Character vector of packages to load.
    #' @param library Character vector of library paths to load the packages
    #'   from.
    eval = function(
      command,
      substitute = TRUE,
      args = list(),
      packages = character(0L),
      library = NULL
    ) {
      command <- if_any(substitute, substitute(command), command)
      if_any(
        is.null(self$workers),
        list(
          data = crew_eval_async(
            command = command,
            args = args,
            packages = packages,
            library = library
          )
        ),
        mirai::mirai(
          .expr = expr_crew_eval_async,
          command = command,
          args = args,
          packages = packages,
          library = library,
          url = self$url,
          .compute = self$instance
        )
      )
    },
    #' @description Start the local workers and error handling socket.
    #' @details Does not terminate workers or an error handling socket
    #'   if `workers` is `NULL` or the object is already terminated.
    #' @return `NULL` (invisibly).
    terminate = function() {
      if (is.null(self$workers) || is.null(self$instance)) {
        return(invisible())
      }
      mirai::daemons(n = 0L, .compute = self$instance)
      close(self$socket)
      self$instance <- NULL
      invisible()
    }
  )
)

#' @title Run an asynchronous task in the crew launcher.
#' @description Called internally, not for users.
#' @export
#' @family utilities
#' @keywords internal
#' @return The result of running `command`.
#' @param command Language object with R code to run.
#' @param args Named list of objects that `command` depends on.
#' @param packages Character vector of packages to load.
#' @param library Character vector of library paths to load the packages from.
#' @param url Local URL for error detection.
crew_eval_async <- function(
  command,
  args = list(),
  packages = character(0L),
  library = NULL,
  url = NULL
) {
  withCallingHandlers(
    {
      load_packages(packages = packages, library = library)
      eval(expr = command, envir = list2env(args, parent = globalenv()))
    },
    error = function(condition) {
      if (!is.null(url)) {
        socket <- nanonext::socket(protocol = "rep", dial = url)
        close(socket)
      }
    }
  )
}

expr_crew_eval_async <- quote(
  crew::crew_eval_async(
    command = command,
    args = args,
    packages = packages,
    library = library,
    url = url
  )
)
