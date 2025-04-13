#' @title Create a `crew` queue object.
#' @export
#' @family queue
#' @description Create an `R6` `crew` queue object.
#' @details A `crew` queue is a classical first-in-first-out data structure
#'   that extends itself in chunks (of size `step`) to avoid
#'   overhead.
#'   `crew` uses queues to efficiently track the names of resolved
#'   tasks and backlogged tasks.
#' @return A queue object.
#' @param step Positive integer with the number of elements to extend the
#'   queue on each call to the `extend()` method.
crew_queue <- function(step = 1e3L) {
  queue <- crew_class_queue$new(step = step)
  queue$validate()
  queue
}

#' @title `R6` queue class
#' @export
#' @family queue
#' @description `R6` class for a queue.
#' @details See [crew_queue()].
#' @examples
#' crew_queue()
crew_class_queue <- R6::R6Class(
  classname = "crew_class_queue",
  cloneable = FALSE,
  private = list(
    .data = NULL,
    .head = NULL,
    .tail = NULL,
    .step = NULL
  ),
  active = list(
    #' @field data Character vector of elements.
    data = function() {
      .subset2(private, ".data")
    },
    #' @field head Non-negative integer pointing to the location of the
    #'   next element to pop.
    head = function() {
      .subset2(private, ".head")
    },
    #' @field tail Non-negative integer pointing to the tail of the queue.
    tail = function() {
      .subset2(private, ".tail")
    },
    #' @field step See [crew_queue()].
    step = function() {
      .subset2(private, ".step")
    }
  ),
  public = list(
    #' @description Create a queue object.
    #' @return A queue object.
    #' @param step See [crew_queue()].
    initialize = function(step = NULL) {
      private$.data <- character(0L)
      private$.head <- 1L
      private$.tail <- 0L
      private$.step <- step
    },
    #' @description Validate the queue.
    #' @return `NULL` (invisibly). Called for its side effects.
    validate = function() {
      crew_assert(
        private$.data,
        is.character(.),
        message = "data in crew queue must be a character vector"
      )
      for (field in c(".head", ".tail", ".step")) {
        crew_assert(
          private[[field]],
          is.integer(.),
          length(.) == 1L,
          !anyNA(.),
          is.finite(.),
          . >= 0L,
          message = paste("invalid crew queue", field)
        )
      }
    },
    #' @description Reset the queue.
    #' @return `NULL` (invisibly). Called for its side effects.
    reset = function() {
      .subset2(self, "set")(data = character(0L))
    },
    #' @description Set the data in the queue.
    #' @return `NULL` (invisibly). Called for its side effects.
    #' @param data Character vector of data to set.
    set = function(data = character(0L)) {
      private$.data <- data
      private$.head <- 1L
      invisible()
    },
    #' @description Pop an element off the queue.
    #' @return Character string, an element popped off the queue.
    #'   `NULL` if there are no more elements available to pop.
    pop = function() {
      data <- .subset2(private, ".data")
      head <- .subset2(private, ".head")
      if (head > length(data)) {
        return(NULL)
      }
      out <- data[head]
      private$.head <- head + 1L
      out
    },
    #' @description Remove and return all available elements off the queue.
    #' @return Character vector, elements collected from the queue.
    #'   `NULL` if there are no more elements available to collect.
    collect = function() {
      head <- .subset2(private, ".head")
      data <- .subset2(private, ".data")
      if (head > length(data)) {
        out <- NULL
      } else if (head > 1L) {
        out <- data[-seq_len(head - 1L)]
      } else {
        out <- data
      }
      .subset2(self, "reset")()
      out
    },
    #' @description Report if the queue is empty.
    #' @return `TRUE` if the queue is empty, `FALSE` otherwise.
    empty = function() {
      .subset2(private, ".head") > length(.subset2(private, ".data"))
    },
    #' @description Report if the queue is nonempty.
    #' @return `TRUE` if the queue is nonempty, `FALSE` otherwise.
    nonempty = function() {
      .subset2(private, ".head") <= length(.subset2(private, ".data"))
    },
    #' @description List the data already popped.
    #' @details `set()`, `reset()`, and `collect()` remove this data.
    #' @return Character vector of data already popped.
    popped = function() {
      .subset2(private, ".data")[seq_len(.subset2(private, ".head") - 1L)]
    }
  )
)
