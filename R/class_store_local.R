#' @title Local store class.
#' @export
#' @description `R6` class for a local store.
class_store <- R6::R6Class(
  classname = "store_local",
  inherit = "store",
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    write_data = function(name, dir, data) {
      crew_assert(is.character(name) & length(name) == 1L & nzchar(name))
      dir_create(self$dir_temp)
      dir_create(dir)
      path_temp <- file.path(self$dir_temp, name)
      path_input <- file.path(dir, name)
      qs::qsave(x = output, file = path_temp)
      file.rename(from = path_temp, to = path_output)
      invisible()
    },
    read_data <- function(name, dir) {
      crew_assert(is.character(name) & length(name) == 1L & nzchar(name))
      path_input <- file.path(dir, name)
      if (!all(file.exists(path_input))) {
        crew_error(
          paste(
            "input file",
            path_input,
            "of worker",
            name,
            "does not exist."
          )
        )
      }
      qs::qread(file = path_input)
    },
    exists_data <- function(name, dir) {
      crew_assert(is.character(name) & length(name) == 1L & nzchar(name))
      all(file.exists(file.path(dir, name)))
    },
    delete_data <- function(name, dir) {
      crew_assert(is.character(name) & length(name) == 1L & nzchar(name))
      path <- file.path(dir, name)
      file.remove(path, recursive = TRUE, force = TRUE)
    }
  )
  public = list(
    #' @description Write worker input.
    #' @param name Character of length 1, worker name.
    #' @param fun Function that the worker will run.
    #' @param args Named list of arguments to `fun`.
    write_input = function(name, fun, args = list()) {
      input <- list(fun = deparse(fun), args = args)
      private$write_data(name = name, dir = self$dir_input, data = input)
    },
    #' @description Write worker output.
    #' @param name Character of length 1, worker name
    #' @param output Worker output to write.
    write_output = function(name, output) {
      private$write_data(name = name, dir = self$dir_output, data = output)
    },
    #' @description Read worker input.
    #' @param name Character of length 1, worker name
    read_input = function(name) {
      input <- private$read_data(name = name, dir = self$dir_input)
      input$fun <- parse(text = input$fun, keep.source = FALSE)
      input
    },
    #' @description Read worker output.
    #' @param name Character of length 1, worker name
    read_output = function(name) {
      private$read_data(name = name, dir = self$dir_output)
    },
    #' @description Exists worker input.
    #' @param name Character of length 1, worker name
    exists_input = function(name) {
      private$exists_data(name = name, dir = self$dir_input)
    },
    #' @description Exists worker output.
    #' @param name Character of length 1, worker name
    exists_output = function(name) {
      private$exists_data(name = name, dir = self$dir_output)
    },
    #' @description Delete worker input.
    #' @param name Character of length 1, worker name
    delete_input = function(name) {
      private$delete_data(name = name, dir = self$dir_input)
    },
    #' @description Delete worker input.
    #' @param name Character of length 1, worker name
    delete_output = function(name) {
      private$delete_data(name = name, dir = self$dir_output)
    }
  )
)
