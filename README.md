
# crew: components to extend rrq workers <img src='man/figures/logo-readme.png' align="right" height="139"/>

[![status](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#WIP)
[![check](https://github.com/wlandau/crew/workflows/check/badge.svg)](https://github.com/wlandau/crew/actions?query=workflow%3Acheck)
[![codecov](https://codecov.io/gh/wlandau/crew/branch/main/graph/badge.svg?token=3T5DlLwUVl)](https://app.codecov.io/gh/wlandau/crew)
[![lint](https://github.com/wlandau/crew/workflows/lint/badge.svg)](https://github.com/wlandau/crew/actions?query=workflow%3Alint)

In computationally demanding analysis workflows, statisticians and data
scientists asynchronously deploy long-running tasks to distributed
systems, ranging from traditional clusters to cloud services. The `crew`
package extends existing task schedulers to run workers on these
systems. With its unifying interface to multiple backends, `crew`
resembles packages
[`clustermq`](https://mschubert.github.io/clustermq/),
[`future`](https://future.futureverse.org/), and
[`batchtools`](https://mllg.github.io/batchtools/).

## Installation

``` r
remotes::install_github("wlandau/crew")
```

## Documentation

Please see <https://wlandau.github.io/crew/> for documentation,
including a full function reference and usage tutorial vignettes.

## Similar work

- [`mirai`](https://github.com/shikokuchuo/mirai): an R framework for
  asynchronous tasks built on [NNG](https://nng.nanomsg.org). `crew`
  builds directly on [`mirai`](https://github.com/shikokuchuo/mirai).
- [`rrq`](https://mrc-ide.github.io/rrq/): a task queue for R powered by
  [Redis](https://redis.io).
- [`rrqueue`](http://traitecoevo.github.io/rrqueue/): predecessor of
  [`rrq`](https://mrc-ide.github.io/rrq/).
- [`clustermq`](https://mschubert.github.io/clustermq/): sends R
  function calls as jobs to computing clusters.
- [`future`](https://future.futureverse.org/): a unified interface for
  asynchronous evaluation of single tasks and map-reduce calls on a wide
  variety of backend technologies.
- [`batchtools`](https://mllg.github.io/batchtools/): tools for
  computation on batch systems.
- [`targets`](https://docs.ropensci.org/targets/): a Make-like pipeline
  tool for R.
- [`later`](https://r-lib.github.io/later/): delayed evaluation of
  synchronous tasks.
- [`promises`](https://rstudio.github.io/promises/): minimally-invasive
  asynchronous programming for a small number of tasks within Shiny
  apps.
- [`callr`](https://github.com/r-lib/callr): initiates R process from
  other R processes.
- [High-performance computing CRAN task
  view](https://CRAN.R-project.org/view=HighPerformanceComputing).

## Thanks

The `crew` package incorporates insightful ideas from the following
people.

- [Charlie Gao](https://github.com/shikokuchuo) for his work on
  [`mirai`](https://github.com/shikokuchuo/mirai) and for accommodating
  the feature requests that made `crew` possible.
- [Rich FitzJohn](https://github.com/richfitz) and [Robert
  Ashton](https://github.com/r-ash) developed
  [`rrq`](https://mrc-ide.github.io/rrq//). `crew` is merely an
  extension of their work.
- [Gábor Csárdi](https://github.com/gaborcsardi/) developed
  [`callr`](https://github.com/r-lib/callr) and wrote an [edifying blog
  post on implementing task
  queues](https://www.tidyverse.org/blog/2019/09/callr-task-q/).
- [Kirill Müller](https://github.com/krlmlr/) created the
  [`workers`](https://github.com/wlandau/workers) prototype, an initial
  effort that led directly to the current implementation of `crew`.
  `crew` would not exist without Kirill’s insights about orchestration
  models for R processes.
- [Henrik Bengtsson](https://github.com/HenrikBengtsson/). Henrik’s
  [`future`](https://github.com/HenrikBengtsson/future/) package
  ecosystem demonstrates the incredible power of a consistent R
  interface on top of a varying collection of high-performance computing
  technologies.
- [Michael Schubert](https://github.com/mschubert/). Michael’s
  [`clustermq`](https://mschubert.github.io/clustermq/) package supports
  efficient high-performance computing on traditional clusters, and it
  demonstrates the value of a central `R6` object to manage an entire
  collection of persistent workers.
- [David Kretch](https://github.com/davidkretch). The
  [`paws`](https://github.com/paws-r/paws) R package is a powerful
  interface to Amazon Web Services, and the documentation clearly
  communicates the capabilities and limitations of AWS to R users.
- [Adam Banker](https://github.com/adambanker), co-authored
  [`paws`](https://github.com/paws-r/paws) with [David
  Kretch](https://github.com/davidkretch).
- [David Neuzerling](https://github.com/mdneuzerling). David’s
  [`lambdr`](https://github.com/mdneuzerling/lambdr/) package
  establishes a helpful pattern to submit and collect AWS Lambda jobs
  from R.
- [Mark Edmondson](https://github.com/MarkEdmondson1234/). Mark
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

  Landau WM (????). _crew: Components to Extend 'rrq' Workers_.
  https://wlandau.github.io/crew/, https://github.com/wlandau/crew.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {crew: Components to Extend 'rrq' Workers},
    author = {William Michael Landau},
    note = {https://wlandau.github.io/crew/, https://github.com/wlandau/crew},
  }
```
