#' @title Crew class.
#' @export
#' @aliases crew
#' @description `R6` class for a crew.
class_crew <- R6::R6Class(
  classname = "crew",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field name Character of length 1, crew name.
    name = NULL,
    #' @field store `R6` store object.
    store = NULL,
    #' @field worker_classes Named list of `R6ClassGenerator`
    #'   objects with worker definitions. The first one
    #'   is the default.
    worker_classes = NULL,
    #' @field workers Named list of worker objects.
    workers = NULL,
    #' @description Crew constructor.
    #' @param name Character of length 1, crew name.
    #' @param store `R6` store object.
    #' @param worker_classes `R6ClassGenerator`
    #'   object with the worker definition.
    #' @param workers Named list of `R6` worker objects.
    initialize = function(
      name = basename(tempfile(pattern = "crew_")),
      store = class_store_local$new(),
      worker_classes = list(default = class_worker),
      workers = list()
    ) {
      self$name <- name
      self$store <- store
      self$worker_classes <- worker_classes
      self$workers <- workers
    },
    #' @description Crew validator.
    validate = function() {
      crew_assert_chr_scalar(self$name, "crew has invalid name.")
      crew_assert(
        inherits(self$store, "store"),
        paste("invalid store in crew", self$name)
      )
      self$store$validate()
      crew_assert(length(self$worker_classes) > 0L)
      crew_assert(is.list(self$worker_classes))
      crew_assert_named(self$worker_classes)
      lapply(self$worker_classes, function(x) {
        crew_assert(inherits(x, "R6ClassGenerator"))
      })
      crew_assert(is.list(self$workers))
      crew_assert_named(self$workers)
      lapply(
        self$workers, function(x) {
          crew_assert(
            inherits(x, "worker"),
            paste(
              "In crew",
              self$crew,
              "not all elements of workers are workers."
            )
          )
        }
      )
      lapply(self$workers, function(x) x$validate())
      crew_assert(
        inherits(self$store, "store"),
        paste(
          "store field in crew",
          self$name,
          "must be an R6 store object."
        )
      )
      self$store$validate()
    }
  )
)
