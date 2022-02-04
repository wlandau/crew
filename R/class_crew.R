#' @title Crew class.
#' @export
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
    #' @field worker_definition `R6ClassGenerator`
    #'   object with the worker definition.
    worker_definition = NULL,
    #' @field worker_list Named list of worker objects.
    worker_list = NULL,
    #' @description Crew constructor.
    #' @param name Character of length 1, crew name.
    #' @param store `R6` store object.
    #' @param worker_definition `R6ClassGenerator`
    #'   object with the worker definition.
    #' @param worker_list Named list of `R6` worker objects.
    initialize = function(
      name = basename(tempfile(pattern = "crew_")),
      store = class_store_local$new(),
      worker_definition = class_worker,
      worker_list = list()
    ) {
      self$name <- name
      self$store <- store
      self$worker_definition <- worker_definition
      self$worker_list <- worker_list
    },
    #' @description Crew validator.
    validate = function() {
      crew_assert_chr_scalar(self$name, "crew has invalid name.")
      crew_assert(
        inherits(self$store, "store"),
        paste("invalid store in crew", self$name)
      )
      self$store$validate()
      crew_assert(
        inherits(self$worker_definition, "R6ClassGenerator"),
        paste(
          "worker_definition field in crew",
          self$name,
          "must be an R6ClassGenerator object (produced by R6::R6Class())",
          "with a valid worker definition."
        )
      )
      crew_assert(
        is.list(self$worker_list),
        paste(
          "worker_list of crew",
          self$name,
          "is not a list."
        )
      )
      worker_names <- names(self$worker_list)
      worker_names <- worker_names[nzchar(worker_names)]
      crew_assert(
        length(unique(worker_names)) == length(self$worker_list),
        paste(
          "all workers in crew",
          self$name,
          "must have nonempty unique names."
        )
      )
      lapply(
        self$worker_list, function(x) {
          crew_assert(
            inherits(x, "worker"),
            paste(
              "In crew",
              self$crew,
              "not all elements of worker_list are workers."
            )
          )
        }
      )
      lapply(self$worker_list, function(x) x$validate())
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
