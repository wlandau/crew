#' @title Evaluate an R command and return results as a monad.
#' @export
#' @family utilities
#' @keywords internal
#' @description Not a user-side function. Do not call directly.
#' @details The `crew_eval()` function evaluates an R expression
#'   in an encapsulated environment and returns a monad with the results,
#'   including warnings and error messages if applicable.
#'   The random number generator seed, `globals`, and global options
#'   are restored to their original values on exit.
#' @return A monad object with results and metadata.
#' @param command Language object with R code to run.
#' @param data Named list of local data objects in the evaluation environment.
#' @param globals Named list of objects to temporarily assign to the
#'   global environment for the task. At the end of the task,
#'   these values are reset to their previous values.
#' @param seed Integer of length 1 with the pseudo-random number generator
#'   seed to temporarily set for the evaluation of the task.
#'   At the end of the task, the seed is restored.
#' @param garbage_collection Logical, whether to run garbage collection
#'   with `gc()` before running the task.
#' @param packages Character vector of packages to load for the task.
#' @param library Library path to load the packages. See the `lib.loc`
#'   argument of `require()`.
#' @examples
#' crew_eval(quote(1 + 1))
crew_eval <- function(
  command,
  data = list(),
  globals = list(),
  seed = sample.int(n = 1e9L, size = 1L),
  garbage_collection = FALSE,
  packages = character(0),
  library = NULL
) {
  true(is.language(command))
  true(data, is.list(.), is_named(.))
  true(globals, is.list(.), is_named(.))
  true(seed, is.numeric(.), length(.) == 1L, !anyNA(.))
  true(garbage_collection, isTRUE(.) || isFALSE(.))
  load_packages(packages = packages, library = library)
  old_options <- options()
  old_globals <- envir_state(names(globals), envir = globalenv())
  withr::local_seed(seed)
  on.exit(envir_restore(state = old_globals, envir = globalenv()), add = TRUE)
  on.exit(options_restore(old_options), add = TRUE)
  list2env(x = globals, envir = globalenv())
  envir <- list2env(x = data, parent = globalenv())
  if (garbage_collection) {
    gc()
  }
  capture_error <- function(condition) {
    state$error <- crew_eval_message(condition)
    state$error_class <- class(condition)
    state$traceback <- paste(as.character(sys.calls()), collapse = "\n")
    NULL
  }
  capture_warning <- function(condition) {
    state$count_warnings <- (state$count_warnings %||% 0L) + 1L
    should_store_warning <- (state$count_warnings < crew_eval_max_warnings) &&
      (nchar(state$warnings %||% "") < crew_eval_max_nchar)
    if (should_store_warning) {
      state$warnings <- paste(
        c(state$warnings, crew_eval_message(condition)),
        collapse = ". "
      )
      state$warnings <- substr(
        state$warnings,
        start = 0,
        stop = crew_eval_max_nchar
      )
    }
    invokeRestart("muffleWarning")
  }
  state <- new.env(hash = FALSE, parent = emptyenv())
  start <- as.numeric(proc.time()["elapsed"])
  result <- tryCatch(
    expr = withCallingHandlers(
      expr = eval(expr = command, envir = envir),
      error = capture_error,
      warning = capture_warning
    ),
    error = function(condition) NULL
  )
  seconds <- as.numeric(proc.time()["elapsed"]) - start
  monad_init(
    command = deparse_safe(command),
    result = result,
    seconds = seconds,
    seed = seed,
    error = state$error %|||% NA_character_,
    traceback = state$traceback %|||% NA_character_,
    warnings = state$warnings %|||% NA_character_,
    socket_data = Sys.getenv("CREW_SOCKET_DATA", unset = NA_character_),
    socket_session = Sys.getenv("CREW_SOCKET_SESSION", unset = NA_character_)
  )
}

envir_state <- function(names, envir) {
  names_revert <- intersect(names, names(envir))
  revert <- map(names_revert, get, envir = envir)
  names(revert) <- names_revert
  list(
    delete = setdiff(names, names(envir)),
    revert = revert
  )
}

envir_restore <- function(state, envir) {
  rm(list = state$delete, envir = envir)
  list2env(state$revert, envir = envir)
  invisible()
}

options_restore <- function(x) {
  names <- setdiff(names(options()), names(x))
  drop <- replicate(length(names), NULL, simplify = FALSE)
  names(drop) <- names
  do.call(what = options, args = drop)
  options(x)
  invisible()
}

crew_eval_message <- function(condition, prefix = character(0)) {
  out <- crew_eval_text_substring(
    message = conditionMessage(condition),
    prefix = prefix
  )
  if_any(nzchar(out), out, ".")
}

crew_eval_text_substring <- function(message, prefix = character(0)) {
  tryCatch(
    substr(
      paste(c(prefix, message), collapse = " "),
      start = 0L,
      stop = crew_eval_max_nchar
    ),
    error = function(condition) {
      paste(
        "crew could not process the error or warning message",
        "due to a text encoding issue."
      )
    }
  )
}

crew_eval_max_nchar <- 2048L
crew_eval_max_warnings <- 51L
