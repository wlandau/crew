# TODO: remove .onLoad() and dependent functions when
# the next {nanonext} and {mirai} arrive on CRAN.
# At that point, update the required versions of those packages.
# nocov start
.onLoad <- function(libname, pkgname) {
  package_version_check(
    package = "nanonext",
    version = "0.8.3.9010",
    repo = "https://shikokuchuo.r-universe.dev"
  )
  package_version_check(
    package = "mirai",
    version = "0.8.7.9013",
    repo = "https://shikokuchuo.r-universe.dev"
  )
}
# nocov end

package_version_check <- function(package, version, repo) {
  if (!rlang::is_installed(pkg = package, version = version)) {
    package_version_message(package = package, version = version, repo = repo)
  }
}

package_version_message <- function(package, version, repo) {
  template <- paste(
    "For {crew}, version %s or higher of packge {%s} is recommended.",
    "If the current CRAN version of {%s} is too low, you can",
    "install the development version using",
    "install.packages(\"%s\", repos = \"%s\").",
    "If you cannot install packages to the main local library, e.g. if",
    "you work in an institution with a central qualified/validated R",
    "installation, consider creating a local self-contained",
    "{renv} package library for your project. For a friendly walkthrough",
    "of {renv}, see https://rstudio.github.io/renv/articles/renv.html"
  )
  message <- sprintf(template, version, package, package, package, repo)
  packageStartupMessage(message)
}

load_packages <- function(packages, library) {
  out <- suppressPackageStartupMessages(
    lapply(
      packages,
      require,
      lib.loc = library,
      quietly = TRUE,
      character.only = TRUE
    )
  )
  out <- as.logical(unlist(out))
  msg <- paste(
    "could not find packages",
    paste(packages[!out], collapse = ", "),
    "in library paths",
    paste(library, collapse = ", ")
  )
  crew_assert(all(out), message = msg)
}
