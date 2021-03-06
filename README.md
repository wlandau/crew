
# crew

[![status](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#Concept)
[![check](https://github.com/wlandau/crew/workflows/check/badge.svg)](https://github.com/wlandau/crew/actions?query=workflow%3Acheck)
[![codecov](https://codecov.io/gh/wlandau/crew/branch/main/graph/badge.svg?token=3T5DlLwUVl)](https://app.codecov.io/gh/wlandau/crew)
[![lint](https://github.com/wlandau/crew/workflows/lint/badge.svg)](https://github.com/wlandau/crew/actions?query=workflow%3Alint)

A task queue is a central hub for orchestrating computationally
demanding workloads dynamically and asynchronously. The `crew` package
supports a variety of task queues with a common interface and different
types of workers for different platforms, similar to the way the
[`future`](https://future.futureverse.org/) package provides a unified R
interface for single jobs and individual map-reduce calls. The `crew`
package borrows the task queue design from the [`callr` package
vignettes](https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/Task-queue-with-callr.Rmd)
and <https://www.tidyverse.org/blog/2019/09/callr-task-q/>, enhances the
core feature set (for example, crash detection), and establishes an
extensible [`R6`](https://r6.r-lib.org/) class system to add more worker
types as development continues (e.g. jobs on Amazon and Google cloud
computing platforms). Multi-queue structures, such as the
already-supported ’[`future`](https://future.futureverse.org/)-based
task queue, increase efficiency in heavily parallel
[cluster](https://future.batchtools.futureverse.org/) workloads with an
inner local asynchronous task queue to manage task submission,
monitoring, and collection.

## Similar work

-   [`targets`](https://docs.ropensci.org/targets/): a Make-like
    pipeline tool for R. Event loops in
    [`targets`](https://docs.ropensci.org/targets/) invoke packages
    [`clustermq`](https://mschubert.github.io/clustermq/) and
    [`future`](https://future.futureverse.org/) to run tasks
    concurrently and asynchronously.
    [`targets`](https://docs.ropensci.org/targets/) would be able to
    support different scheduling techniques and run on more platforms if
    it incorporated a formal task queue behind the scenes. That task
    queue is probably going to be `crew` or
    [`rrq`](https://mrc-ide.github.io/rrq/).
-   [`callr`](https://github.com/r-lib/callr): the previous [task queue
    vignette](https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/Task-queue-with-callr.Rmd),
    as well as the blog post at
    <https://www.tidyverse.org/blog/2019/09/callr-task-q/>, show how to
    create an efficient multi-process task queue using
    [`callr::r_session`](https://callr.r-lib.org/reference/r_session.html).
    The goal of `crew` is to eventually extend this idea to different
    worker types, such as [Amazon Web Services (AWS)
    Batch](https://aws.amazon.com/batch/) jobs.
-   [`rrq`](https://mrc-ide.github.io/rrq/): an R-focused task queue
    built on Redis, successor of
    [`rrqueue`](http://traitecoevo.github.io/rrqueue/). It is possible
    that [`rrq`](https://mrc-ide.github.io/rrq/) makes `crew`
    irrelevant.
-   [`rrqueue`](http://traitecoevo.github.io/rrqueue/): an R-focused
    task queue built on Redis, no longer maintained.
-   [`future`](https://future.futureverse.org/): a unified interface for
    asynchronous evaluation of single tasks and map-reduce calls on a
    wide variety of backend technologies. One of the backends of `crew`.
-   [`clustermq`](https://mschubert.github.io/clustermq/): an R layer on
    top of [ZeroMQ](https://zeromq.org/) for submitting tasks as array
    jobs on traditional clusters.
-   [`mirai`](https://github.com/shikokuchuo/mirai): an R framework for
    asynchronous tasks built on [NNG](https://nng.nanomsg.org).
-   [`promises`](https://rstudio.github.io/promises/):
    minimally-invasive asynchronous programming for a small number of
    tasks within Shiny apps.
-   [`later`](https://r-lib.github.io/later/): delayed evaluation of
    synchronous tasks.
-   [High-performance computing CRAN task
    view](https://CRAN.R-project.org/view=HighPerformanceComputing).

## Installation

``` r
remotes::install_github("wlandau/crew")
```

## Documentation

Please see <https://wlandau.github.io/crew/articles/usage.html> for a
tutorial.

## Future development

`crew`, [`rrq`](https://mrc-ide.github.io/rrq/), or something like it
will serve as a backend to get
[`targets`](https://docs.ropensci.org/targets/) running distributed
workers on the cloud (AWS Batch, AWS Fargate, Google Cloud Run, etc.).
If `crew` fills this role, new [task
queues](https://wlandau.github.io/crew/reference/index.html#queues) and
[data
stores](https://wlandau.github.io/crew/reference/index.html#stores) will
be developed for cloud platforms.

## Thanks

The `crew` package incorporates insightful ideas from the following
people.

-   [Gábor Csárdi](https://github.com/gaborcsardi/). Gábor created the
    [`callr`](https://github.com/r-lib/callr) R package and wrote
    instructional code for creating task queues using
    [`callr::r_session`](https://callr.r-lib.org/reference/r_session.html)
    (links below). `crew` borrows and modifies the [vignette
    code](https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R)
    under the MIT license. See also the `crew` package `NOTICE` file.
    -   Blog post:
        <https://www.tidyverse.org/blog/2019/09/callr-task-q/>
    -   `callr` vignette code:
        <https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R>
-   [Kirill Müller](https://github.com/krlmlr/). The
    [`workers`](https://github.com/wlandau/workers) prototype was
    entirely his vision, and `crew` would not exist without it. `crew`
    reflects this and many other insights from Kirill about
    orchestration models for R processes.
-   [Henrik Bengtsson](https://github.com/HenrikBengtsson/). Henrik’s
    [`future`](https://github.com/HenrikBengtsson/future/) package
    ecosystem demonstrates the incredible power of a consistent R
    interface on top of a varying collection of high-performance
    computing technologies.
-   [Michael Schubert](https://github.com/mschubert/). Michael’s
    [`clustermq`](https://mschubert.github.io/clustermq/) package
    supports efficient high-performance computing on traditional
    clusters, and it demonstrates the value of a central `R6` object to
    manage an entire collection of persistent workers.
-   [David Kretch](https://github.com/davidkretch). The
    [`paws`](https://github.com/paws-r/paws) R package is a powerful
    interface to Amazon Web Services, and the documentation clearly
    communicates the capabilities and limitations of AWS to R users.
-   [Adam Banker](https://github.com/adambanker), co-authored
    [`paws`](https://github.com/paws-r/paws) with [David
    Kretch](https://github.com/davidkretch).
-   [David Neuzerling](https://github.com/mdneuzerling). David’s
    [`lambdr`](https://github.com/mdneuzerling/lambdr/) package
    establishes a helpful pattern to submit and collect AWS Lambda jobs
    from R.
-   [Mark Edmondson](https://github.com/MarkEdmondson1234/). Mark
    maintains several R packages to interface with Google Cloud Platform
    such as
    [`googleCloudStorageR`](https://github.com/cloudyr/googleCloudStorageR)
    and
    [`googleCloudRunner`](https://github.com/MarkEdmondson1234/googleCloudRunner),
    and he [started the
    conversation](https://github.com/ropensci/targets/issues/720) around
    helping [`targets`](https://github.com/ropensci/targets) submit jobs
    to Google Cloud Run.

## Code of Conduct

Please note that the `crew` project is released with a [Contributor Code
of
Conduct](https://github.com/wlandau/crew/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Citation

``` r
citation("crew")

To cite package 'crew' in publications use:

  Landau WM (????). _crew: Centralized Workers_.
  https://wlandau.github.io/crew/, https://github.com/wlandau/crew.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {crew: Centralized Workers},
    author = {William Michael Landau},
    note = {https://wlandau.github.io/crew/, https://github.com/wlandau/crew},
  }
```
