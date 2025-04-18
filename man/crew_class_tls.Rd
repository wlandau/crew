% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crew_tls.R
\name{crew_class_tls}
\alias{crew_class_tls}
\title{\code{R6} TLS class.}
\description{
\code{R6} class for TLS configuration.
}
\details{
See \code{\link[=crew_tls]{crew_tls()}}.
}
\examples{
crew_tls(mode = "automatic")

## ------------------------------------------------
## Method `crew_class_tls$new`
## ------------------------------------------------

crew_tls(mode = "automatic")
}
\seealso{
Other tls: 
\code{\link{crew_tls}()}
}
\concept{tls}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{mode}}{See \code{\link[=crew_tls]{crew_tls()}}.}

\item{\code{key}}{See \code{\link[=crew_tls]{crew_tls()}}.}

\item{\code{password}}{See \code{\link[=crew_tls]{crew_tls()}}.}

\item{\code{certificates}}{See \code{\link[=crew_tls]{crew_tls()}}.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-crew_class_tls-new}{\code{crew_class_tls$new()}}
\item \href{#method-crew_class_tls-validate}{\code{crew_class_tls$validate()}}
\item \href{#method-crew_class_tls-client}{\code{crew_class_tls$client()}}
\item \href{#method-crew_class_tls-worker}{\code{crew_class_tls$worker()}}
\item \href{#method-crew_class_tls-url}{\code{crew_class_tls$url()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-crew_class_tls-new"></a>}}
\if{latex}{\out{\hypertarget{method-crew_class_tls-new}{}}}
\subsection{Method \code{new()}}{
TLS configuration constructor.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{crew_class_tls$new(
  mode = NULL,
  key = NULL,
  password = NULL,
  certificates = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mode}}{Argument passed from \code{\link[=crew_tls]{crew_tls()}}.}

\item{\code{key}}{Argument passed from \code{\link[=crew_tls]{crew_tls()}}.}

\item{\code{password}}{Argument passed from \code{\link[=crew_tls]{crew_tls()}}.}

\item{\code{certificates}}{Argument passed from \code{\link[=crew_tls]{crew_tls()}}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An \code{R6} object with TLS configuration.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{crew_tls(mode = "automatic")
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-crew_class_tls-validate"></a>}}
\if{latex}{\out{\hypertarget{method-crew_class_tls-validate}{}}}
\subsection{Method \code{validate()}}{
Validate the object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{crew_class_tls$validate(test = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{test}}{Logical of length 1, whether to test the TLS configuration
with \code{nanonext::tls_config()}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{NULL} (invisibly).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-crew_class_tls-client"></a>}}
\if{latex}{\out{\hypertarget{method-crew_class_tls-client}{}}}
\subsection{Method \code{client()}}{
TLS credentials for the \code{crew} client.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{crew_class_tls$client()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{NULL} or character vector, depending on the mode.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-crew_class_tls-worker"></a>}}
\if{latex}{\out{\hypertarget{method-crew_class_tls-worker}{}}}
\subsection{Method \code{worker()}}{
TLS credentials for \code{crew} workers.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{crew_class_tls$worker(profile)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{profile}}{Character of length 1 with the \code{mirai} compute profile.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{NULL} or character vector, depending on the mode.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-crew_class_tls-url"></a>}}
\if{latex}{\out{\hypertarget{method-crew_class_tls-url}{}}}
\subsection{Method \code{url()}}{
Form the URL for \code{crew} worker connections.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{crew_class_tls$url(host, port)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{host}}{Character string with the host name or IP address.}

\item{\code{port}}{Non-negative integer with the port number
(0 to let NNG select a random ephemeral port).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Character string with the URL.
}
}
}
