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
    #' @field max_workers Positive integer,
    #'   maximum number of workers in the crew.
    max_workers = NULL,
    #' @description Crew constructor.
    #' @param name Character of length 1, crew name.
    #' @param store `R6` store object.
    #' @param worker_classes `R6ClassGenerator`
    #'   object with the worker definition.
    #' @param workers Named list of `R6` worker objects.
    #' @param max_workers Positive integer,
    #'   maximum number of workers in the crew.
    initialize = function(
      name = basename(tempfile(pattern = "crew_")),
      store = class_store_local$new(),
      worker_classes = list(default = class_worker),
      workers = list(),
      max_workers = 1
    ) {
      self$name <- name
      self$store <- store
      self$worker_classes <- worker_classes
      self$workers <- workers
      self$max_workers <- max_workers
    },
    #' @description create worker objects from one or more worker definitions.
    #'   Does not actually launch the new worker objects.
    #' @param workers Number of new worker objects to create.
    #' @param class Name of the worker class to use for launching workers.
    #' @param ... Named arguments to the worker constructor.
    recruit = function(
      workers = 1L,
      class = names(self$worker_classes)[[1]],
      ...
    ) {
      crew_assert_pos_dbl_scalar(workers)
      crew_assert_chr_scalar(class)
      crew_asset(class in names(self$worker_classes))
      
    },
    #' @description Launch all down workers in the crew.
    launch = function() {
      
    },
    #' @description Determine if any worker is unassigned
    #'   and ready to accept a new job.
    sendable = function() {
      
    },
    #' @description Send a job to an available worker. Assumes `sendable()`
    #'   on the crew returns `TRUE`.
    #' @param fun Function to run in the job.
    #' @param args Named list of arguments to `fun()`
    send = function(fun, args = list()) {
      crew_assert(is.function(fun))
      crew_assert(is.list(args))
      crew_assert_named(args)
      
    },
    #' @description Determine if any worker in the crew is done
    #'   with its current job and the job output is available for collection.
    receivable = function() {
      
    },
    #' @description Choose a receivable worker and collect its job output.
    receive = function() {
      
    },
    #' @description Shut down one or more workers. Workers must be sendable
    #'   if the worker shutdown method uses `send()`.
    #' @param workers Positive integer of length 1,
    #'   number of workers to shut down.
    shutdown = function(workers = Inf) {
      crew_assert_pos_dbl_scalar(workers)
      
    },
    #' @description  delete one or more worker objects from the crew.
    #'   Dismissed workers must be down and sendable,
    #'   so not all requested dismissals actually happen.
    #' @param workers Positive integer of length 1,
    #'   number of workers to shut down.
    dismiss = function(workers = Inf) {
      crew_assert_pos_dbl_scalar(workers)
      
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
      crew_assert_pos_dbl_scalar(self$max_workers)
    }
  )
)
