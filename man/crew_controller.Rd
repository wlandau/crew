% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crew_controller.R
\name{crew_controller}
\alias{crew_controller}
\title{Create a controller object from a client and launcher.}
\usage{
crew_controller(
  client,
  launcher,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  crashes_max = 5L,
  backup = NULL,
  auto_scale = NULL
)
}
\arguments{
\item{client}{An \code{R6} client object created by \code{\link[=crew_client]{crew_client()}}.}

\item{launcher}{An \code{R6} launcher object created by one of the
\verb{crew_launcher_*()} functions such as \code{\link[=crew_launcher_local]{crew_launcher_local()}}.}

\item{reset_globals}{\code{TRUE} to reset global environment
variables between tasks, \code{FALSE} to leave them alone.}

\item{reset_packages}{\code{TRUE} to detach any packages loaded during
a task (runs between each task), \code{FALSE} to leave packages alone.
In either case, the namespaces are not detached.}

\item{reset_options}{\code{TRUE} to reset global options to their original
state between each task, \code{FALSE} otherwise. It is recommended to
only set \code{reset_options = TRUE} if \code{reset_packages} is also \code{TRUE}
because packages sometimes rely on options they set at loading time.
for this and other reasons, \code{reset_options} only resets options
that were nonempty at the beginning of the task.
If your task sets an entirely new option not already in \code{options()},
then \code{reset_options = TRUE} does not delete the option.}

\item{garbage_collection}{\code{TRUE} to run garbage collection after each task
task, \code{FALSE} to skip.}

\item{crashes_max}{In rare cases, a worker may exit unexpectedly
before it completes its current task. If this happens, \code{pop()}
returns a status of \code{"crash"} instead of \code{"error"} for the task.
The controller does not automatically retry the task, but
you can retry it manually by calling \code{push()} again and using the same
task name as before. (However, \code{targets} pipelines running \code{crew}
do automatically retry tasks whose workers crashed.)

\code{crashes_max} is a non-negative integer, and it sets the maximum number of
allowable consecutive crashes for a given task.
If a task's worker crashes more than \code{crashes_max} times in a row,
then \code{pop()} throws an error when it tries to return the results
of the task.}

\item{backup}{An optional \code{crew} controller object, or \code{NULL} to omit.
If supplied, the \code{backup} controller runs any pushed tasks that have
already reached \code{crashes_max} consecutive crashes.
Using \code{backup}, you can create
a chain of controllers with different levels of resources
(such as worker memory and CPUs) so that a task that fails on
one controller can retry using incrementally more powerful workers.
All controllers in a backup chain should be part of the same
controller group (see \code{\link[=crew_controller_group]{crew_controller_group()}}) so you can call the
group-level \code{pop()} and \code{collect()} methods to make sure you get results
regardless of which controller actually ended up running the task.

Limitations of \code{backup}:
* \code{crashes_max} needs to be positive in order for \code{backup} to be used.
Otherwise, every task would always skip the current controller and
go to \code{backup}.
* \code{backup} cannot be a controller group. It must be an ordinary
controller.}

\item{auto_scale}{Deprecated. Use the \code{scale} argument of \code{push()},
\code{pop()}, and \code{wait()} instead.}
}
\description{
This function is for developers of \code{crew} launcher plugins.
Users should use a specific controller helper such as
\code{\link[=crew_controller_local]{crew_controller_local()}}.
}
\examples{
if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
client <- crew_client()
launcher <- crew_launcher_local()
controller <- crew_controller(client = client, launcher = launcher)
controller$start()
controller$push(name = "task", command = sqrt(4))
controller$wait()
controller$pop()
controller$terminate()
}
}
\seealso{
Other controller: 
\code{\link{crew_class_controller}}
}
\concept{controller}
