% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crew_terminate_process.R
\name{crew_terminate_process}
\alias{crew_terminate_process}
\title{Manually terminate a local process.}
\usage{
crew_terminate_process(pid)
}
\arguments{
\item{pid}{Integer of length 1, process ID to terminate.}
}
\value{
\code{NULL} (invisibly).
}
\description{
Manually terminate a local process.
}
\examples{
if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
process <- processx::process$new("sleep", "60")
process$is_alive()
crew_terminate_process(pid = process$get_pid())
process$is_alive()
}
}
\seealso{
Other utility: 
\code{\link{crew_assert}()},
\code{\link{crew_clean}()},
\code{\link{crew_deprecate}()},
\code{\link{crew_eval}()},
\code{\link{crew_random_name}()},
\code{\link{crew_retry}()},
\code{\link{crew_terminate_signal}()},
\code{\link{crew_worker}()}
}
\concept{utility}
