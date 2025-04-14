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
#' @param data Character vector of initial queue data.
#' @param step Positive integer with the number of elements to extend the
#'   queue on each call to the `extend()` method.
crew_queue <- function(data = character(0L), step = 1e3L) {
  queue <- crew_class_queue$new(data = data, step = step)
  queue$validate()
  queue
}

#' @title `R6` queue class
#' @export
#' @family queue
#' @description `R6` class for a queue.
#' @details See the Details section of [crew_queue()].
#'   The `R6` `crew` queue class is not portable (for efficiency),
#'   so other packages should not inherit from it.
#'   The reason for non-portability is efficiency: elements can be
#'   directly accessed without `self$` or `private$`, and they can be
#'   directly modified with `<<-`.
#'   This is especially important for `push()` because
#'   `envir$vector[slice] <- x` copies the entire vector in memory,
#'   which has O(n^2) complexity and is extremely slow for large vectors.
#' @examples
#' crew_queue()
crew_class_queue <- R6::R6Class(
  classname = "crew_class_queue",
  cloneable = FALSE,
  portable = FALSE,
  private = list(
    .data = NULL,
    .head = NULL,
    .tail = NULL,
    .step = NULL
  ),
  active = list(
    #' @field data See [crew_queue()].
    data = function() {
      .data
    },
    #' @field head Non-negative integer pointing to the location of the
    #'   next element to pop.
    head = function() {
      .head
    },
    #' @field tail Non-negative integer pointing to the tail of the queue.
    tail = function() {
      .tail
    },
    #' @field step See [crew_queue()].
    step = function() {
      .step
    }
  ),
  public = list(
    #' @description Create a queue object.
    #' @return A queue object.
    #' @param data See [crew_queue()].
    #' @param step See [crew_queue()].
    initialize = function(data = NULL, step = NULL) {
      .data <<- data
      .head <<- 1L
      .tail <<- length(data)
      .step <<- as.integer(step)
    },
    #' @description Validate the queue.
    #' @return `NULL` (invisibly). Called for its side effects.
    validate = function() {
      crew_assert(
        .data,
        is.character(.),
        message = "data in crew queue must be a character vector"
      )
      for (field in c("head", "tail", "step")) {
        crew_assert(
          self[[field]],
          is.integer(.),
          length(.) == 1L,
          !anyNA(.),
          is.finite(.),
          . >= 0L,
          message = paste("invalid crew queue field", field)
        )
      }
    },
    #' @description Check if the queue is empty.
    #' @return `TRUE` if the queue is empty, `FALSE` otherwise.
    empty = function() {
      .tail <= 0L || .head > .tail
    },
    #' @description Check if the queue is empty.
    #' @return `TRUE` if the queue is nonempty, `FALSE` otherwise.
    nonempty = function() {
      .tail > 0L && .head <= .tail
    },
    #' @description List available data.
    #' @return Character vector of available data.
    list = function() {
      if (empty()) {
        return(character(0L))
      }
      .data[seq(from = .head, to = .tail)]
    },
    #' @description Reset the queue.
    #' @return `NULL` (invisibly). Called for its side effects.
    reset = function() {
      .data <<- character(0L)
      .head <<- 1L
      .tail <<- 0L
      invisible()
    },
    #' @description Remove popped elements from the data in the queue.
    #' @return `NULL` (invisibly).
    clean = function() {
      if (.head > 1L) {
        .data <<- .data[-seq(.head - 1L)]
        .tail <<- .tail - .head + 1L
        .head <<- 1L
      }
      invisible()
    },
    #' @description Set the data in the queue.
    #' @return `NULL` (invisibly). Called for its side effects.
    #' @param data Character vector of data to set.
    set = function(data = character(0L)) {
      .data <<- data
      .head <<- 1L
      .tail <<- length(data)
      invisible()
    },
    #' @description Extend the queue data by `step` elements.
    #' @param n Positive integer, number of elements to extend the queue data.
    #' @return `NULL` (invisibly).
    extend = function(n) {
      clean()
      .data <<- c(.data, rep(NA_character_, max(n, .step)))
      invisible()
    },
    #' @description Append new elements to the queue.
    #' @details `push()` is the reason the queue class is not portable.
    #'   According to R6 documentation,
    #'   members of non-portable classes
    #'   can be accessed without `self$` or `private$`,
    #'   and assignment can be done with `<<-`.
    #'   In the case of `push()`, this prevents each assignment from
    #'   deep-copying the entire contents of the vector.
    #' @return `NULL` (invisibly).
    #' @param x Character vector of new data to append.
    push = function(x) {
      n <- length(x)
      if (length(.data) - .tail < n) {
        extend(n)
      }
      .data[seq_len(n) + .tail] <<- x
      .tail <<- .tail + n
      invisible()
    },
    #' @description Pop one or more elements off the queue.
    #' @param n Positive integer, maximum number of elements to pop.
    #'   Fewer than `n` are popped if fewer than `n` are available.
    #' @return Character vector of elements popped off the queue.
    #'   `NULL` if there are no more elements available to pop.
    pop = function(n = 1L) {
      if (empty()) {
        return(NULL)
      }
      n <- min(n, .tail - .head + 1L)
      slice <- seq.int(from = .head, length.out = n)
      out <- .subset(.data, slice)
      private$.head <<- .head + n
      out
    },
    #' @description Remove and return all available elements off the queue.
    #' @return Character vector, elements collected from the queue.
    #'   `NULL` if there are no more elements available to collect.
    collect = function() {
      if (empty()) {
        reset()
        return(NULL)
      }
      out <- .data[seq(from = .head, to = .tail)]
      reset()
      out
    }
  )
)
