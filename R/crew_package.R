#' crew: abstract interface specification for semi-persistent
#'   high-performance computing workers
#' @docType package
#' @name crew-package
#' @description The user interface in `crew` is an abstraction for
#'   high-performance computing technologies.
#'   Like the `future` R package,
#'   `crew` provides a backend-agnostic standard.
#'   But unlike `future`, `crew` prioritizes centralized scheduling,
#'   semi-persistent (reusable) workers, and user-driven customization.
#'   The user supplies methods to initialize, dispatch, poll,
#'   and terminate a worker on a specific backend technology.
#'   The backend technology could be forked processes,
#'   an on-premises high-performance computing cluster,
#'   a cloud computing service, or even another worker abstraction
#'   like `future`. Then, `crew` supplies an `R6` subclass to
#'   create objects that manage the actual workers.
#'   Every worker group has a centralized mutable cache
#'   and a centralized registry of all the workers in the group,
#'   both of which are available to the user-defined worker methods.
#'   This facilitates advanced capabilities such as worker affinities
#'   and batched polling. The end goal is to help
#'   pipeline tools such as such `targets`
#'   efficiently orchestrate tasks
#'   without having to support individual low-level interfaces to specific
#'   high-performance computing platforms or cloud computing services.
#' @family help
#' @importFrom R6 R6Class
NULL
