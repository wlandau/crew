load_packages <- function(packages, library) {
  out <- lapply(
    packages,
    require,
    lib.loc = library,
    quietly = TRUE,
    character.only = TRUE
  )
  out <- as.logical(unlist(out))
  if (!all(out)) {
    msg <- paste(
      "could not find packages",
      paste(packages[!out], collapse = ", "),
      "in library paths",
      paste(library, collapse = ", ")
    )
    crew_error(message = msg)
  }
}

package_installed <- function(package) {
  result <- .subset2(packages_installed, package)
  if (is.null(result)) {
    result <- rlang::is_installed(pkg = package)
    packages_installed[[package]] <- result
  }
  result
}

packages_installed <- new.env(parent = emptyenv(), hash = TRUE)
