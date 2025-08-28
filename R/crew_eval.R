#' @title Evaluate an R command and return results as a monad.
#' @export
#' @family utility
#' @description Not a user-side function. Do not call directly.
#' @details The `crew_eval()` function evaluates an R expression
#'   in an encapsulated environment and returns a monad with the results,
#'   including warnings and error messages if applicable.
#'   The random number generator seed, `globals`, and global options
#'   are restored to their original values on exit.
#' @return A monad object with results and metadata.
#' @param command Language object with R code to run.
#' @param name Character of length 1, name of the task.
#' @param data Named list of local data objects in the evaluation environment.
#' @param globals Named list of objects to temporarily assign to the
#'   global environment for the task.
#' @param seed Integer of length 1 with the pseudo-random number generator
#'   seed to set for the evaluation of the task. Passed to the
#'   `seed` argument of `set.seed()` if not `NULL`.
#'   If `algorithm` and `seed` are both `NULL`,
#'   then the random number generator defaults to the
#'   recommended widely spaced worker-specific
#'   L'Ecuyer streams as supported by `mirai::nextstream()`.
#'   See `vignette("parallel", package = "parallel")` for details.
#' @param algorithm Integer of length 1 with the pseudo-random number
#'   generator algorithm to set for the evaluation of the task.
#'   Passed to the `kind` argument of `RNGkind()` if not `NULL`.
#'   If `algorithm` and `seed` are both `NULL`,
#'   then the random number generator defaults to the
#'   recommended widely spaced worker-specific
#'   L'Ecuyer streams as supported by `mirai::nextstream()`.
#'   See `vignette("parallel", package = "parallel")` for details.
#' @param packages Character vector of packages to load for the task.
#' @param library Library path to load the packages. See the `lib.loc`
#'   argument of `require()`.
#' @param reset_globals `TRUE` to reset global environment
#'   variables between tasks, `FALSE` to leave them alone.
#' @param reset_packages `TRUE` to detach any packages loaded during
#'   a task (runs between each task), `FALSE` to leave packages alone.
#'   In either case, the namespaces are not detached.
#' @param reset_options `TRUE` to reset global options to their original
#'   state between each task, `FALSE` otherwise. It is recommended to
#'   only set `reset_options = TRUE` if `reset_packages` is also `TRUE`
#'   because packages sometimes rely on options they set at loading time.
#'   for this and other reasons, `reset_options` only resets options
#'   that were nonempty at the beginning of the task.
#'   If your task sets an entirely new option not already in `options()`,
#'   then `reset_options = TRUE` does not delete the option.
#' @param garbage_collection `TRUE` to run garbage collection after each task
#'   task, `FALSE` to skip.
#' @examples
#' crew_eval(quote(1 + 1), name = "task_name")
crew_eval <- function(
  command,
  name,
  data = list(),
  globals = list(),
  seed = NULL,
  algorithm = NULL,
  packages = character(0),
  library = NULL,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE
) {
  # Begin cleanup.
  # Cleanup partially borrowed from mirai under MIT license:
  # do_cleanup() in https://github.com/r-lib/mirai/blob/main/R/daemon.R
  if (reset_globals) {
    old_globals <- names(.GlobalEnv)
    on.exit(
      {
        new_globals <- names(.GlobalEnv)
        rm(list = setdiff_chr(new_globals, old_globals), envir = .GlobalEnv)
      },
      add = TRUE
    )
  }
  if (reset_packages) {
    old_packages <- search()
    on.exit(
      {
        new_packages <- search()
        detach_packages <- setdiff_chr(new_packages, old_packages)
        try(
          lapply(detach_packages, detach, character.only = TRUE),
          silent = TRUE
        )
      },
      add = TRUE
    )
  }
  if (reset_options) {
    old_options <- options()
    # options(old_options) does not remove newly set options, only
    # the values of previously nonempty options.
    # However, a more aggressive approach causes mysterious errors, including
    # false positives in targets:::compare_working_directories().
    on.exit(options(old_options), add = TRUE)
  }
  if (garbage_collection) {
    on.exit(gc(verbose = FALSE), add = TRUE)
  }
  # End cleanup.
  if (!is.null(algorithm) || !is.null(seed)) {
    old_algorithm <- RNGkind()[1L]
    old_seed <- .subset2(.GlobalEnv, ".Random.seed")
    if (!is.null(algorithm)) {
      RNGkind(kind = algorithm)
    }
    if (!is.null(seed)) {
      set.seed(seed = seed)
    }
    on.exit(RNGkind(kind = old_algorithm), add = TRUE)
    if (is.null(old_seed)) {
      # Testing this case depends on having a fresh R session outside RStudio.
      set.seed(seed = NULL) # nocov
    } else {
      on.exit(.GlobalEnv$.Random.seed <- old_seed, add = TRUE)
    }
  }
  if (package_installed("autometric (>= 0.1.0)")) {
    autometric::log_phase_set(phase = name)
    on.exit(autometric::log_phase_reset(), add = TRUE)
  }
  load_packages(packages = packages, library = library)
  list2env(x = globals, envir = globalenv())
  envir <- list2env(x = data, parent = globalenv())
  capture_error <- function(condition) {
    message(paste("Error:", conditionMessage(condition))) # for log files
    state$error <- crew_eval_message(condition)
    state$error_class <- class(condition)
    state$trace <- paste(as.character(sys.calls()), collapse = "\n")
    NULL
  }
  capture_warning <- function(condition) {
    message(paste("Warning:", conditionMessage(condition))) # for log files
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
  start <- nanonext::mclock()
  result <- tryCatch(
    expr = withCallingHandlers(
      expr = eval(expr = command, envir = envir),
      error = capture_error,
      warning = capture_warning
    ),
    error = function(condition) NA
  )
  seconds <- (nanonext::mclock() - start) / 1000
  error <- .subset2(state, "error")
  trace <- .subset2(state, "trace")
  warnings <- .subset2(state, "warnings")
  if (is.null(seed)) {
    seed <- NA_integer_
  }
  if (is.null(algorithm)) {
    algorithm <- NA_character_
  }
  if (is.null(error)) {
    error <- NA_character_
    status <- "success"
    code <- 0L
  } else {
    status <- "error"
    code <- -1L
  }
  if (is.null(trace)) {
    trace <- NA_character_
  }
  if (is.null(warnings)) {
    warnings <- NA_character_
  }
  monad_init(
    name = name,
    command = deparse_safe(command),
    result = result,
    status = status,
    error = error,
    code = code,
    trace = trace,
    warnings = warnings,
    seconds = seconds,
    seed = seed,
    algorithm = algorithm,
    controller = Sys.getenv("CREW_CONTROLLER", unset = NA_character_)
  )
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

expr_crew_eval <- quote(
  crew::crew_eval(
    name = name,
    command = command,
    data = data,
    globals = globals,
    seed = seed,
    algorithm = algorithm,
    packages = packages,
    library = library,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection
  )
)
