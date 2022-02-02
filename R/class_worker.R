#' @title Worker class.
#' @export
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
    #' @description Worker constructor.
    #' @param name Character of length 1, worker name.
    #' @param crew `R6` crew object to which the worker belongs.
    #' @param timeout Positive numeric of length 1, number of seconds
    #'   that a worker can idle before timing out.
    #' @param wait_input Positive numeric of length 1, number of seconds
    #'   that the worker waits between checking if a job exists.
    initialize = function(
      name = basename(tempfile(pattern = "worker_")),
      crew = NULL,
      timeout = 60,
      wait_input = 0.1
    ) {
      self$name <- name
      self$crew <- crew
      self$timeout <- timeout
      self$wait_input <- wait_input
    },
    #' @description Worker validator.
    validate = function() {
      crew_assert_chr_scalar(self$name, "worker has invalid name.")
      crew_assert(
        inherits(self$crew, "crew"),
        paste("invalid crew object in worker", self$name)
      )
      crew_assert(
        identical(self$crew$worker_list[[self$name]], self),
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
    }
  )
)
