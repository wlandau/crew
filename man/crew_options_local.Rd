% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crew_options_local.R
\name{crew_options_local}
\alias{crew_options_local}
\title{Local \code{crew} launcher options.}
\usage{
crew_options_local(log_directory = NULL, log_join = TRUE)
}
\arguments{
\item{log_directory}{Either \code{NULL} or a character of length 1
with the file path to a directory to write worker-specific log files
with standard output and standard error messages.
Each log file represents a single \emph{instance} of a running worker,
so there will be more log files
if a given worker starts and terminates a lot. Set to \code{NULL} to suppress
log files (default).}

\item{log_join}{Logical of length 1. If \code{TRUE}, \code{crew} will write
standard output and standard error to the same log file for
each worker instance. If \code{FALSE}, then they these two streams
will go to different log files with informative suffixes.}
}
\value{
A classed list of options for the local launcher.
}
\description{
Options for the local \code{crew} launcher.
}
\examples{
  crew_options_local()
}
\seealso{
Other options: 
\code{\link{crew_options_metrics}()}
}
\concept{options}
