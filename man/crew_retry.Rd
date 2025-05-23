% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crew_retry.R
\name{crew_retry}
\alias{crew_retry}
\title{Retry code.}
\usage{
crew_retry(
  fun,
  args = list(),
  seconds_interval = 1,
  seconds_timeout = 60,
  max_tries = Inf,
  error = TRUE,
  message = character(0),
  envir = parent.frame(),
  assertions = TRUE
)
}
\arguments{
\item{fun}{Function that returns \code{FALSE} to keep waiting
or \code{TRUE} to stop waiting.}

\item{args}{A named list of arguments to \code{fun}.}

\item{seconds_interval}{Nonnegative numeric of length 1,
number of seconds to wait between calls to \code{fun}.}

\item{seconds_timeout}{Nonnegative numeric of length 1,
number of seconds to loop before timing out.}

\item{max_tries}{Maximum number of calls to \code{fun} to try
before giving up.}

\item{error}{Whether to throw an error on a timeout or max tries.}

\item{message}{Character of length 1, optional error message
if the wait times out.}

\item{envir}{Environment to evaluate \code{fun}.}

\item{assertions}{\code{TRUE} to run assertions to check if arguments are
valid, \code{FALSE} otherwise. \code{TRUE} is recommended for users.}
}
\value{
\code{NULL} (invisibly).
}
\description{
Repeatedly retry a function while it keeps returning \code{FALSE}
and exit the loop when it returns \code{TRUE}
}
\examples{
crew_retry(fun = function() TRUE)
}
\seealso{
Other utility: 
\code{\link{crew_assert}()},
\code{\link{crew_clean}()},
\code{\link{crew_deprecate}()},
\code{\link{crew_eval}()},
\code{\link{crew_random_name}()},
\code{\link{crew_terminate_process}()},
\code{\link{crew_terminate_signal}()},
\code{\link{crew_worker}()}
}
\concept{utility}
