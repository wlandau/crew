
# crew

<!--[![CRAN](https://www.r-pkg.org/badges/version/crew)](https://CRAN.R-project.org/package=crew)-->

[![status](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![check](https://github.com/ropensci/crew/workflows/check/badge.svg)](https://github.com/ropensci/crew/actions?query=workflow%3Acheck)
[![codecov](https://codecov.io/gh/ropensci/crew/branch/main/graph/badge.svg?token=3T5DlLwUVl)](https://app.codecov.io/gh/ropensci/crew)
[![lint](https://github.com/ropensci/crew/workflows/lint/badge.svg)](https://github.com/ropensci/crew/actions?query=workflow%3Alint)

The [`R6`](https://r6.r-lib.org) classes of `crew` establish a
standardized user interface to high-performance computing technologies.
Unlike its closely related [`future`](https://future.futureverse.org/)
package, `crew` prioritizes centralized scheduling, heterogeneous
semi-persistent workers, and user-driven customization. The primary goal
is to help pipeline tools such as such
[`targets`](https://docs.ropensci.org/targets/) efficiently orchestrate
tasks without having to support individual low-level interfaces to
specific high-performance computing platforms or cloud services.

## Installation

``` r
remotes::install_github("ropensci/crew")
```

## Usage

TBD. Need <https://github.com/wlandau/crew/issues/1> first.

## Nested crews

TBD. Advantages:

1.  Avoid blocking the main process.
2.  Manage a small number of local cores and a large number of remote
    workers.

## Thanks

The `crew` package incorporates insightful ideas from the following
people.

-   [Kirill Müller](https://github.com/krlmlr/). The
    [`workers`](https://github.com/wlandau/worker) prototype was
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
#> 
#> To cite package 'crew' in publications use:
#> 
#>   William Michael Landau (NA). crew: Centralized Reusable Workers.
#>   https://wlandau.github.io/crew/, https://github.com/wlandau/crew.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {crew: Centralized Reusable Workers},
#>     author = {William Michael Landau},
#>     note = {https://wlandau.github.io/crew/, https://github.com/wlandau/crew},
#>   }
```
