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
      private$.step <- step
      self$reset()
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
    #' @description Check if the queue is empty.
    #' @return `TRUE` if the queue is empty, `FALSE` otherwise.
    empty = function() {
      head <- .subset2(private, ".head")
      tail <- .subset2(private, ".tail")
      tail <= 0L || head > tail
    },
    #' @description Check if the queue is empty.
    #' @return `TRUE` if the queue is nonempty, `FALSE` otherwise.
    nonempty = function() {
      head <- .subset2(private, ".head")
      tail <- .subset2(private, ".tail")
      tail > 0L && head <= tail
    },
    #' @description Reset the queue.
    #' @return `NULL` (invisibly). Called for its side effects.
    reset = function() {
      private$.data <- character(0L)
      private$.head <- 1L
      private$.tail <- 0L
      invisible()
    },
    #' @description Remove popped elements from the data in the queue.
    #' @return `NULL` (invisibly).
    clean = function() {
      head <- .subset2(private, ".head")
      if (head > 1L) {
        private$.data <- .subset2(private, ".data")[-seq(head - 1L)]
        private$.tail <- tail - head + 1L
        private$.head <- 1L
      }
      invisible()
    },
    #' @description Set the data in the queue.
    #' @return `NULL` (invisibly). Called for its side effects.
    #' @param data Character vector of data to set.
    set = function(data = character(0L)) {
      private$.data <- data
      private$.head <- 1L
      private$.tail <- length(data)
      invisible()
    },
    #' @description Extend the queue data by `step` elements.
    #' @param n Positive integer, number of elements to extend the queue data.
    #' @return `NULL` (invisibly).
    extend = function(n) {
      .subset2(self, "clean")()
      n <- max(n, .subset2(private, ".step"))
      private$.data <- c(.subset2(private, ".data"), rep(NA_character_, n))
      invisible()
    },
    #' @description Append new elements to the queue.
    #' @return `NULL` (invisibly).
    #' @param x Character vector of new data to append.
    push = function(x) {
      data <- .subset2(private, ".data")
      tail <- .subset2(private, ".tail")
      n <- length(x)
      if (length(data) - tail < n) {
        .subset2(self, "extend")(n)
      }
      tail <- .subset2(private, ".tail")
      private$.data[seq_along(x) + tail] <- x
      private$.tail <- tail + n
      invisible()
    },
    #' @description Pop an element off the queue.
    #' @return Character string, an element popped off the queue.
    #'   `NULL` if there are no more elements available to pop.
    pop = function() {
      if (.subset2(self, "empty")()) {
        return(NULL)
      }
      head <- .subset2(private, ".head")
      out <- .subset(.subset2(private, ".data"), head)
      private$.head <- head + 1L
      out
    },
    #' @description Remove and return all available elements off the queue.
    #' @return Character vector, elements collected from the queue.
    #'   `NULL` if there are no more elements available to collect.
    collect = function() {
      on.exit(.subset2(self, "reset")())
      if (.subset2(self, "empty")()) {
        return(NULL)
      }
      data <- .subset2(private, ".data")
      head <- .subset2(private, ".head")
      tail <- .subset2(private, ".tail")
      if (head > tail) {
        return(NULL)
      }
      data[seq(from = head, to = tail)]
    }
  )
)
