
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nsink

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The `nsink` package is an R implementation of the methods described in
[Kellogg et. al (2010)](https://doi.org/10.1016/j.ecoleng.2010.02.006).
Previous implementation of this approach relied on a vector based
approach that was time consuming to prepare. This approach uses a hybrid
raster-vector approach that takes very little time to set up for each
new watershed and relies on readily available data. Thus, its
application is national for the United States.

Proposed (as of 2019-06-12) functions for the `nsink` package are:

  - `nsink_get_data()` - Pass HUC, get data, use cache to avoid repeat
    downloads
  - `nsink_prep_data()` - Prepares data by clipping to specified HUC,
    standardizing projections, standardizing raster extents. Outputs
    will be HUC level for HUC, flow direction, streams, waterbodies,
    impervious, and soils.
  - `nsink_calc_removal()` - implemented now as purely raster, might be
    able to do hybrid approach (see lines 384+ in working\_nsink.Rmd).
    Use a method argument for c(“raster”, “hybrid”).
      - `nsink_calc_land_removal()`
      - `nsink_calc_stream_removal()`
      - `nsink_calc_lake_removal()`
      - `nsink_calc_removal_type()` - maybe include this as part of each
        type removal
  - `nsink_flowpath_removal()`
      - `nsink_flowpath_ends()`
      - `nsink_flowpath_flowlines()`
      - `nsink_flowpath_removal_filter()` - raster only.
  - `nsink_flowpath_removal_summary()` - summarizes flowpath removal

The API for this package is still very much a work in progress and may
change at anytime with possible breakage of past functionality. Use at
own risk\!

## Installation

When released to CRAN, you can install the released version of nsink
from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("nsink")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jhollist/nsink")
```
