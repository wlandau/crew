#' @title Worker class.
#' @export
#' @aliases worker
#' @description `R6` class for a worker.
class_worker <- R6::R6Class(
  classname = "worker",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field name Character of length 1, worker name.
    name = NULL,
    #' @field crew `R6` crew object to which the worker belongs.
    crew = NULL,
    #' @field timeout Positive numeric of length 1,
    #'   number of seconds of idling for the worker to time out.
    timeout = NULL,
    #' @field wait_input Positive numeric of length 1,
    #'   number of seconds for a worker to wait between iterations of
    #'   polling input.
    wait_input = NULL,
    #' @field tags Character vector of optional user-defined tags
    #'   to select subsets of eligible workers for job submission and
    #'   retrieval.
    tags = NULL,
    #' @field assigned Logical of length 1, whether the worker
    #'   already has a job to do.
    assigned = NULL,
    #' @description Worker constructor.
    #' @param name Character of length 1, worker name.
    #' @param crew `R6` crew object to which the worker belongs.
    #' @param timeout Positive numeric of length 1, number of seconds
    #'   that a worker can idle before timing out.
    #' @param wait_input Positive numeric of length 1, number of seconds
    #'   that the worker waits between checking if a job exists.
    #' @param tags Character vector of optional user-defined tags
    #'   to mark eligible groups of workers in scheduling operations.
    initialize = function(
      name = basename(tempfile(pattern = "worker_")),
      crew = NULL,
      timeout = 60,
      wait_input = 0.1,
      tags = character(0)
    ) {
      self$name <- name
      self$crew <- crew
      self$timeout <- timeout
      self$wait_input <- wait_input
      self$tags <- unique(tags)
      self$assigned <- FALSE
    },
    #' @description Check if this worker is ready to accept a job.
    sendable = function() {
      !self$assigned
    },
    #' @description Send a job.
    #' @param fun Function to run in the job. Should be completely
    #'   self-contained in the body and arguments, without relying
    #'   on the closure or global variables in the environment.
    #' @param args Named list of arguments to `fun`.
    send = function(fun, args = list()) {
      if (!self$sendable()) {
        crew_error(sprintf("worker %s is busy.", self$name))
      }
      self$assigned <- TRUE
      crew_assert(is.function(fun))
      crew_assert(is.list(args))
      crew_assert_named(args)
      data <- list(fun = deparse(fun), args = args)
      self$crew$store$write_input(name = self$name, data = data)
      self$launch()
      invisible()
    },
    #' @description `TRUE` if a worker is receivable with a job and the
    #'   main process can receive the output of the job. `FALSE` otherwise.
    receivable = function() {
      self$crew$store$exists_output(name = self$name)
    },
    #' @description Collect the results of a job.
    receive = function() {
      out <- self$crew$store$read_output(name = self$name)
      self$crew$store$delete_output(name = self$name)
      self$assigned <- FALSE
      out
    },
    #' @description Gracefully shut down the worker.
    shutdown = function() {
      self$send(fun = function() rlang::abort(class = "crew_shutdown"))
      invisible()
    },
    #' @description Check if the worker has one or more
    #'   of the tags in the argument.
    #' @param tags Character vector of tags to check.
    tagged = function(tags) {
      crew_assert(is.character(tags))
      any(self$tags %in% tags)
    },
    #' @description Worker validator.
    validate = function() {
      crew_assert_chr_scalar(self$name, "worker has invalid name.")
      crew_assert(
        inherits(self$crew, "crew"),
        paste("invalid crew object in worker", self$name)
      )
      crew_assert(
        identical(self$crew$workers[[self$name]], self),
        paste(
          "worker",
          self$name,
          "falsely claims it is part of crew",
          self$crew$name
        )
      )
      crew_assert_pos_dbl_scalar(
        self$timeout,
        "worker timeout must be a positive number."
      )
      crew_assert_pos_dbl_scalar(
        self$wait_input,
        "worker timeout must be a positive number."
      )
      crew_assert_lgl_scalar(self$assigned)
      crew_assert(is.character(self$tags))
      funs <- c(
        "up",
        "launch",
        "send",
        "sendable",
        "receive",
        "receivable",
        "shutdown",
        "validate"
      )
      for (fun in funs) {
        crew_assert(is.function(self[[fun]]), paste(fun, "method undefined"))
      }
    }
  )
)
