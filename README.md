
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

You can get the original set of five recordings from
[OSF](https://osf.io/w2s3a) using the following function:

``` r
get_osf_recordings(c("NIR_ABh_Puriya", "NIRP1_VS_Hams", "NIRP1_MAK_Jaun",
                     "Gagaku_5_Juha", "NIR_DBh_Malhar"), node = "https://osf.io/w2s3a")
```

This function will download five zip files containing movement data and
extract the contents to the folder `movementsync/Original` in your home
directory.

The R scripts in the [inst/samplescripts](inst/samplescripts) directory
walk-through the package functionality using these recordings. These
scripts are installed as part of the package. You can see them locally
by typing

``` r
open_walk_throughs()
```
