
# crew

<!--[![CRAN](https://www.r-pkg.org/badges/version/crew)](https://CRAN.R-project.org/package=crew)-->

[![status](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![check](https://github.com/ropensci/crew/workflows/check/badge.svg)](https://github.com/ropensci/crew/actions?query=workflow%3Acheck)
[![codecov](https://codecov.io/gh/ropensci/crew/branch/main/graph/badge.svg?token=3T5DlLwUVl)](https://app.codecov.io/gh/ropensci/crew)
[![lint](https://github.com/ropensci/crew/workflows/lint/badge.svg)](https://github.com/ropensci/crew/actions?query=workflow%3Alint)

The [`R6`](https://r6.r-lib.org) classes of `crew` establish a
standardized user interface to high-performance computing technologies.
Unlike its closely related [`future`](https://future.futureverse.org/)
package, `crew` prioritizes centralized scheduling, semi-persistent
(reusable) workers, and user-driven customization. The primary goal is
to help pipeline tools such as such
[`targets`](https://docs.ropensci.org/targets/) efficiently orchestrate
tasks without having to support individual low-level interfaces to
specific high-performance computing platforms or cloud services.

## Installation

``` r
remotes::install_github("ropensci/crew")
```

## Thanks

The `crew` package incorporates insightful ideas from the following
people.

-   [Kirill Müller](https://github.com/krlmlr/)
-   [Henrik Bengtsson](https://github.com/HenrikBengtsson/)
-   [Michael Schubert](https://github.com/mschubert/)
-   [David Kretch](https://github.com/davidkretch)
-   [Adam Banker](https://github.com/adambanker)
-   [David Neuzerling](https://github.com/mdneuzerling)

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
