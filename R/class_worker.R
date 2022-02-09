#' @title Worker class.
#' @export
#' @aliases worker
#' @description `R6` class for a worker.
#' @details A worker object is an interface to manage a single
#'   high-performance computing worker. Supported methods
#'   send jobs, receive output, and poll, etc.
#'   This particular worker class is the parent abstract class
#'   and is not usable on its own. Subclasses like
#'   [class_worker_callr] and [class_worker_future]
#'   are concrete and usable. In addition, worker needs a crew in
#'   order to be valid. So it is recommended to create a worker
#'   of a given subclass through the `recruit()` method of the crew.
#'   See the examples.
#' @examples
#' crew <- class_crew$new(worker_classes = list(class_worker_callr))
#' crew$recruit(workers = 1)
#' worker <- crew$workers[[1]]
#' worker$send(fun = function(arg) paste("job", arg), args = list(arg = 1))
#' while (!worker$receivable()) Sys.sleep(0.1)
#' job <- worker$receive()
#' print(job$value)
#' print(job$error)
#' worker$shutdown()
#' processx::supervisor_kill()
class_worker <- R6::R6Class(
  classname = "worker",
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    send_job = function(
      fun = function() TRUE,
      args = list(),
      timeout = 60,
      wait = 1,
      class = "default"
    ) {
      crew_assert(is.function(fun))
      crew_assert(is.list(args))
      crew_assert_named(args)
      crew_assert_nonnegative_dbl_scalar(timeout)
      crew_assert_nonnegative_dbl_scalar(wait)
      if (!self$sendable()) {
        crew_error(sprintf("worker %s is busy.", self$name))
      }
      crew_assert(is.function(fun))
      crew_assert(is.list(args))
      crew_assert_named(args)
      data <- structure(list(fun = deparse(fun), args = args), class = class)
      self$crew$store$write_input(
        name = self$name,
        data = data,
        timeout = timeout,
        wait = wait
      )
      self$assigned <- TRUE
      self$launch()
      invisible()
    }
  ),
  public = list(
    #' @field name Character of length 1, worker name.
    name = NULL,
    #' @field crew `R6` crew object to which the worker belongs.
    crew = NULL,
    #' @field timeout Positive numeric of length 1,
    #'   number of seconds of idling for the worker to time out.
    timeout = NULL,
    #' @field wait Positive numeric of length 1,
    #'   number of seconds in a polling interval, e.g. checking
    #'   if an input job exists.
    wait = NULL,
    #' @field tags Character vector of optional user-defined tags
    #'   to select subsets of eligible workers for job submission and
    #'   retrieval.
    tags = NULL,
    #' @field assigned Logical of length 1, whether the worker
    #'   already has a job to do.
    assigned = NULL,
    #' @description Worker constructor.
    #' @return The `new()` method calls the constructor
    #'   and returns a new worker object.
    #' @param name Character of length 1, worker name.
    #' @param crew `R6` crew object to which the worker belongs.
    #' @param timeout Positive numeric of length 1, number of seconds
    #'   that a worker can idle before timing out.
    #' @param wait Positive numeric of length 1, number of seconds
    #'   in a polling interval, e.g. checking if an input job exists.
    #' @param tags Character vector of optional user-defined tags
    #'   to mark eligible groups of workers in scheduling operations.
    initialize = function(
      name = basename(tempfile(pattern = "worker_")),
      crew = NULL,
      timeout = 60,
      wait = 0.1,
      tags = character(0)
    ) {
      self$name <- name
      self$crew <- crew
      self$timeout <- timeout
      self$wait <- wait
      self$tags <- unique(tags)
      self$assigned <- FALSE
    },
    #' @description Check if the worker has one or more
    #'   of the tags in the argument.
    #' @param tags Character vector of tags to check.
    #' @return `TRUE` if the worker has any of the tags in the `tags` argument.
    #'   `FALSE` otherwise.
    tagged = function(tags) {
      crew_assert(is.character(tags))
      any(self$tags %in% tags)
    },
    #' @description Check if this worker is ready to accept a job.
    #' @return `TRUE` if the worker is ready for a job
    #'   (or not properly blocked) and `FALSE` otherwise.
    sendable = function() {
      !self$assigned
    },
    #' @description Send a job.
    #' @return `NULL` (invisibly).
    #' @param fun Function to run in the job. Should be completely
    #'   self-contained in the body and arguments, without relying
    #'   on the closure or global variables in the environment.
    #' @param args Named list of arguments to `fun`.
    #' @param timeout Number of seconds to wait for job data to
    #'   send successfully.
    #' @param wait Number of seconds to wait between iterations checking
    #'   if the job data was sent successfully.
    send = function(
      fun = function() TRUE,
      args = list(),
      timeout = 60,
      wait = 1
    ) {
      private$send_job(
        fun = fun,
        args = args,
        timeout = timeout,
        wait = wait
      )
    },
    #' @description Check if job output is available to collect.
    #' @return `TRUE` if a job output can be collected from the worker,
    #'   `FALSE` otherwise.
    receivable = function() {
      self$crew$store$exists_output(name = self$name)
    },
    #' @description Collect the results of a job and free up the worker.
    #' @details Once collected and returned. the job output is deleted from
    #'   the data store and no longer available to receive.
    #'   `receive()` also marks the worker as "sendable" again (unblocked)
    #'   which makes the worker ready for another job (`send()` method).
    #' @return A named list of job output. The `value` element has the
    #'   actual result of the job function, if successful. Other elements
    #'   have job metadata such as the error message (if any), traceback,
    #'   warnings, and runtime in seconds.
    #' @param timeout Number of seconds to wait for the file deletion to
    #'   succeed.
    #' @param wait Number of seconds to wait between iterations checking
    #'   that the worker files were successfully removed from the data store.
    receive = function(timeout = 60, wait = 1) {
      out <- self$crew$store$read_output(name = self$name)
      self$clear(timeout = timeout, wait = wait)
      self$assigned <- FALSE
      out
    },
    #' @description Clear worker input and output.
    #' @details Deletes worker input and output files.
    #' @return `NULL` (invisibly)
    #' @param timeout Number of seconds to wait for the file deletion to
    #'   succeed.
    #' @param wait Number of seconds to wait between iterations checking
    #'   that the worker files were successfully removed from the data store.
    clear = function(timeout = 60, wait = 1) {
      self$crew$store$delete_input(self$name, timeout = timeout, wait = wait)
      self$crew$store$delete_output(self$name, timeout = timeout, wait = wait)
      invisible()
    },
    #' @description Gracefully shut down the worker.
    #' @details The underlying worker process should promptly
    #'   shut down if successful. A new `send()` or `launch()`
    #'   call will re-launch the worker.
    #'
    #'   The default shutdown method
    #'   sends a special shutdown job through the data store.
    #'   This is not always reliable, e.g. if a worker freezes.
    #'   Subclasses of `class_worker` should write their own
    #'   shutdown methods that leverage the backend technology.
    #'   to achieve more reliable shutdowns.
    #' @return `NULL` (invisibly).
    shutdown = function() {
      private$send_job(class = "shutdown")
      invisible()
    },
    #' @description Check if a worker is stuck.
    #' @details A worker is stuck if it is down, not sendable,
    #'   and not receivable.
    #' @return Logical of length 1, whether the worker is stuck.
    stuck = function() {
      !self$sendable() && !self$receivable() && !self$up()
    },
    #' @description Relaunch the worker if it is stuck.
    #' @return `NULL` (invisibly).
    restart = function() {
      if (self$stuck()) {
        self$launch()
      }
      invisible()
    },
    #' @description Worker validator.
    #' @return `NULL` (invisibly).
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
        self$wait,
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
      invisible()
    }
  )
)
