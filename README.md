
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gpsactivs

<!-- badges: start -->
<!-- badges: end -->

The goal of gpsactivs is to provide algorithms for developing semantic
understanding of GPS profiles.

## Installation

You can install the development version of `gpsactivs` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("byu-transpolab/gpsactivs")
```

## Example

The package currently includes code to compute activity locations using
a modified DBSCAN-TE (Gong et al., 2018) algorithm.

``` r
library(gpsactivs)
## basic example code
```

## References

<div class="csl-entry">

Gong, L., Yamamoto, T., & Morikawa, T. (2018). Identification of
activity stop locations in GPS trajectories by DBSCAN-TE method combined
with support vector machines. <i>Transportation Research Procedia</i>,
<i>32</i>, 146–154. <https://doi.org/10.1016/J.TRPRO.2018.10.028>

</div>
