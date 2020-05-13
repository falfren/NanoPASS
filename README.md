
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NanoPASS

<!-- badges: start -->

[![German Federal Institute for Risk
Assessment](https://www.bfr.bund.de/images/bfr_logo.gif)](www.bfr.bund.de)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

The **Nano** **P**article **A**dministration **S**edimentation
**S**imulator (NanoPASS) is a package which intends to simulate the
sedimentation process of nano particles in a cell culture well. By
simulating this process the experimentator can estimate the effective
dose over time the cells are exposed to. Also, this gives an idea on at
which time point the cells are exposed to the calculated concentration
of sedimenting particles.

## Installation

First download the current version `NanoPASS_0.1.15.2.tar.gz` from the
repository.

You can install the released version of NanoPASS using the tar-file from
the supplemental material or using the recently downloaded current
version of this
repository.

``` r
install.packages("/path/to/NanoPASS_0.1.15.2.tar.gz", repos = NULL, type = "source")
```

## Example

This is a basic example which shows how to start the main function in
this package:

``` r
library(NanoPASS)
## main function will be started typing:
NanoPASS::ThreeDSDD()
```

The command presented above does start the shiny app.
