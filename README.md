
<!-- README.md is generated from README.Rmd. Please edit that file -->

# movementsync - Analysis and Visualisation of Movement and Music Onset Data

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the current version of `movementsync` from Github by
entering the following commands into R:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("paulemms/movementsync")
```

## Usage

``` r
library(movementsync)
packageVersion("movementsync")
```

From a fresh install of the package, you can get the original set of
five recordings from OSF using the following function:

``` r
get_osf_recordings()
```

The R scripts in the [inst/samplescripts](inst/samplescripts) directory
walk-through the package functionality using these recordings.
