#' @title Create a `crew` queue object.
#' @export
#' @family queue
#' @description Create an `R6` `crew` queue object for resolved task names.
#' @details A `crew` queue object efficiently tracks the names of resolved
#'   tasks so the controller can pop them efficiently.
crew_queue <- function() {
  queue <- crew_class_queue$new()
  queue$validate()
  queue
}

#' @title `R6` queue class
#' @export
#' @family queue
#' @description `R6` class for a queue of resolved task names.
#' @details See [crew_queue()].
#' @examples
#' crew_queue()
crew_class_queue <- R6::R6Class(
  classname = "crew_class_queue",
  cloneable = FALSE,
  private = list(
    .names = character(0L),
    .head = 1L
  ),
  active = list(
    #' @field names Names of resolved tasks.
    names = function() {
      .subset2(private, ".names")
    },
    #' @field head Non-negative integer pointing to the location of the
    #'   next name to pop.
    head = function() {
      .subset2(private, ".head")
    }
  ),
  public = list(
    #' @description Validate the queue.
    #' @return `NULL` (invisibly). Called for its side effects.
    validate = function() {
      crew_assert(
        private$.names,
        is.character(.),
        !anyNA(.),
        message = "invalid names field of crew queue"
      )
      crew_assert(
        private$.head,
        is.integer(.),
        length(.) == 1L,
        !anyNA(.),
        is.finite(.),
        . >= 1L,
        . <= length(private$.names) + 1L,
        message = "invalid head field of crew queue"
      )
    },
    #' @description Reset the queue.
    #' @return `NULL` (invisibly). Called for its side effects.
    reset = function() {
      .subset2(self, "set")(names = character(0L))
    },
    #' @description Set the names in the queue.
    #' @return `NULL` (invisibly). Called for its side effects.
    #' @param names Character vector of names to set.
    set = function(names = character(0L)) {
      private$.names <- names
      private$.head <- 1L
      invisible()
    },
    #' @description Pop a name off the queue.
    #' @return Character string, a name popped off the queue.
    #'   `NULL` if there are no more names available to pop.
    pop = function() {
      names <- .subset2(private, ".names")
      head <- .subset2(private, ".head")
      if (head > length(names)) {
        return(NULL)
      }
      out <- names[head]
      private$.head <- head + 1L
      out
    },
    #' @description Remove and return all available names off the queue.
    #' @return Character vector, names collected from the queue.
    #'   `NULL` if there are no more names available to collect.
    collect = function() {
      head <- .subset2(private, ".head")
      names <- .subset2(private, ".names")
      if (head > length(names)) {
        out <- NULL
      } else if (head > 1L) {
        out <- names[-seq_len(head - 1L)]
      } else {
        out <- names
      }
      .subset2(self, "reset")()
      out
    },
    #' @description Report if the queue is empty.
    #' @return `TRUE` if the queue is empty, `FALSE` otherwise.
    empty = function() {
      .subset2(private, ".head") > length(.subset2(private, ".names"))
    },
    #' @description Report if the queue is nonempty.
    #' @return `TRUE` if the queue is nonempty, `FALSE` otherwise.
    nonempty = function() {
      .subset2(private, ".head") <= length(.subset2(private, ".names"))
    },
    #' @description List the names already popped.
    #' @details `set()`, `reset()`, and `collect()` remove these names.
    #' @return Character vector of names already popped.
    popped = function() {
      .subset2(private, ".names")[seq_len(.subset2(private, ".head") - 1L)]
    }
  )
)
