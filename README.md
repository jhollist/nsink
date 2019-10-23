
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

Proposed (as of 2019-10-23) functions for the `nsink` package are:

  - `nsink_get_data()` - Pass HUC, get data, use cache to avoid repeat
    downloads
  - `nsink_prep_data()` - Prepares data by clipping to specified HUC,
    standardizing projections, standardizing raster extents. Outputs
    will be HUC level for HUC, flow direction, streams, waterbodies,
    impervious, and soils.
  - `nsink_calc_removal()` - implemented now as purely raster, might be
    able to do hybrid approach (see lines 384+ in working\_nsink.Rmd).
    Use a method argument for c(“raster”, “hybrid”). Maybe not here?  
  - `nsink_generate_flowpath()` - This generates a single flowpath for a
    point selected on the landscape. The flowpath is contstructed of the
    raster flow direction generated flowpath on land, and then uses the
    NHDPlus flowpath once the raster flowpath intersects the NHDPlus
    flowpath.
  - `nsink_summarize_flowpath()` - Summarizes flowpath removal and
    returns a list with flowpath rasters for removal and type, the total
    removal for the flowpath, and a summary data frame of removal per
    segment along the flowpath.
  - `nsink_generate_static_maps()` - Function to create static maps for
    a HUC.
      - `nsink_generate_removal()` - Already exists in prep data, but
        need to create a nice clean raster version of removal and output
        as a tif (probably)
      - `nsink_generate_n_loading_index()` - Use NLCD as the basis for
        this. Essentially a reclass of NLCD to index of n loading. Q has
        these numbers. 0 to 1
      - `nsink_generate_n_removal_heat_map` - This is the per pixel
        total flowpath removal. Probably can’t do every pixel, so sub
        sample of watershed pixels and interpolate final surface. Should
        be a parallel thing as each pixel is independent. 0 to 1
      - `nsink_generate_n_delivery_index()` - This is n\_loading\_index
        multiplied by n\_removal\_heat\_map. 0 to 1

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
