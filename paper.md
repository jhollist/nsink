nsink: An R package for flow path based nitrogen removal estimation
================
28 May 2021

# Summary

The `nsink` package was written to estimate cumulative nitrogen (N)
removal along a specified flowpath and is based of the methodologies
outlined in Kellogg et al. (2010). `nsink` accesses and downloads all
required datasets from public datasets in the United States, prepares
that data for use, summarizes N removal along a flowpath and creates a
set of static watershed maps that summarize flowpath based relative N
removal. Additionally, the results of an `nsink` analysis may be
exported to standard geospatial files (e.g. shapefiles and tiff images)
for use in a Geographic Information System or other application.

# Statement of need

Excess N delivery via surface water to downstream aquatic resources such
as coastal estuaries contributes to impaired water quality and leads to
several ecosystem impacts including harmful algal blooms (HABs) and
hypoxia (Rabalais, Turner, and Scavia 2002). Identifying landscape N
sinks (areas where dissolved N is transformed to gaseous N and
effectively removed from the aquatic system) and their effect on N
delivery at the watershed scale is helpful to watershed managers, land
use planners and conservation organizations. The theoretical
underpinnings for identifying N sinks rely on decades of research and
are explained in Kellogg et al. (2010). ADD IN A FEW SENTENCES ABOUT THE
GIST OF RELATIVE NITROGEN REMOVAL.The landscape N sinks considered in
the approach implemented in `nsink` include wetlands (as hydric soils),
lakes, and streams, as these are all areas were water flow is slowed and
N transformations have time to occur.

The first implementation of this type of approach was done on a case by
case basis. Data acquisition and manipulation were mostly manual
processes and the end result could take weeks to months for a single
small watershed to be completed (C. Damon, pers. comm.). Thus, the
effort required for the analysis limited it’s application and scaling an
flowpath based N sink analysis beyond a few pilot studies was not
feasible. The goal of `nsink` was to address this limitation and provide
an open source solution that could be run on a single small watershed in
minutes to hours without significant manual input.

# The `nsink` package

## Package Installation

The `nsink` package is available from <https://github.com/usepa/nsink>
and may be installed in R with the following:

``` r
# If not installed, install remotes
install.packages("remotes")
# Install nsink from GitHub
remotes::install_github("USEPA/nsink", build_vignettes = TRUE)
```

## Package Details

The `nsink` package is designed around the major steps in running a
flowpath based analysis with the following major steps:

1.  Prepare for analysis
    -   Get required data
    -   Prepare that data for analysis
    -   Calculate relative N removal layer
2.  Run an interactive analysis
    -   Calculate a flowpath
    -   Summarize relative N removal along a flowpath
3.  Run a watershed based analysis
    -   Develop static maps
    -   Generate output datasets

### Required Data

The ability to run an `nsink` analysis relies on several national scale
dataset for the United States. By limiting our approach to these
national datasets we are ensuring scalability of the application of
`nsink` because the datasets will be available for nearly all locations
in the United States. The four datasets that `nsink` uses are the
National Hydrography Dataset Plus (NHDPlus), Soil Survey Geographic
Database (SSURGO), the National Land Cover Dataset (NLCD) land cover,
and the National Land Cover Dataset (NLCD) impervious surface \[ADD
CITATION FOR DATASETS\]. These datasets are all available via either an
Application Programming Interface (API) or via direct download
(e.g. FTP).

### Dependencies

To acquire the datasets and run the analysis, `nsink` depends on several
existing R packages to facilitate spatial data handling, data
acquisition, data management, data analysis and data processing. These
are detailed in Table 1.

| Package     | Task                               | Citation                                                                                               |
|-------------|------------------------------------|--------------------------------------------------------------------------------------------------------|
| `sf`        | Spatial Data Handling and Analysis | E. Pebesma (2018); E. Pebesma (2021b)                                                                  |
| `raster`    | Spatial Data Handling and Analysis | Hijmans (2021)                                                                                         |
| `stars`     | Spatial Data Handling and Analysis | E. Pebesma (2021c)                                                                                     |
| `fasterize` | Spatial Data Handling and Analysis | Ross (2020)                                                                                            |
| `lwgeom`    | Spatial Data Handling and Analysis | E. Pebesma (2021a)                                                                                     |
| `gstat`     | Spatial Data Handling and Analysis | E. J. Pebesma (2004); Gräler, Pebesma, and Heuvelink (2016); E. Pebesma and Graeler (2021)             |
| `sp`        | Spatial Data Handling and Analysis | E. J. Pebesma and Bivand (2005); Bivand, Pebesma, and Gomez-Rubio (2013); E. Pebesma and Bivand (2021) |
| `units`     | Unit Transformations               | E. Pebesma, Mailund, and Hiebert (2016); E. Pebesma et al. (2021)                                      |
| `FedData`   | Data Acquisition                   | Bocinsky (2020)                                                                                        |
| `httr`      | Data Acquisition                   | Wickham (2020)                                                                                         |
| `dplyr`     | Data Management and Analysis       | Wickham et al. (2021)                                                                                  |
| `zoo`       | Data Management and Analysis       | Zeileis and Grothendieck (2005); Zeileis, Gorthendieck, and Ryan (2021)                                |
| `igraph`    | Data Management and Analysis       | Csardi and Nepusz (2006); Csardi et al. (2020)                                                         |
| `readr`     | Data Management and Analysis       | Wickham and Hester (2020)                                                                              |
| `foreign`   | Data Management and Analysis       | R Core Team (2020)                                                                                     |
| `rlang`     | Data Management and Analysis       | Henry and Wickham (2021)                                                                               |
| `furrr`     | Parallel Processing                | Vaughan and Dancho (2021)                                                                              |
| `future`    | Parallel Processing                | Bengtsson (2021); Bengtsson (2020)                                                                     |

Table 1. R Package Dependencies for the `nsink` package

### Functionality

Currently, `nsink` provides 10 exported functions to facilitate a
flowpath based analysis of relative N removal.

-   `nsink_get_huc_id()`: A function for searching the name of a USGS
    Watershed Boundary Dataset Hydrologic Unit
    (<https://www.usgs.gov/core-science-systems/ngp/national-hydrography/watershed-boundary-dataset>)
    and retrieving it’s 12-digit Hydrologic Unit Code (HUC). Partial and
    exact matching of names are enabled.
-   `nsink_get_data()`: Many data sources are required for an `nsink`
    analysis. This function uses any acceptable (e.g. 2-digit to
    12-digit) HUC ID to download the following datasets: NHDPlus,
    SSURGO, NLCD Land Cover, and the NLCD Impervious.  
-   `nsink_prep_data()`: An `nsink` analysis requires data to be in a
    standard coordinate reference system combined mutliple NHDPlus
    tables and merges information from different parts of SSURGO. This
    function completes these data preparation steps and outputs all
    data, clipped to the HUC boundary and outputs a list containing all
    the required data for an `nsink` analysis.
-   `nsink_calc_removal()`: Quantifying relative N removal across a
    landscape is one of the key aspects of an `nsink` analysis and is
    the sole purpose of this function. The `nsink_calc_removal()`
    function takes the list object that is returned from
    `nsink_prep_data()` and calculates N removal for each landscape
    sink. This includes relative N removal for hydric soils, lakes,
    ponds, and reservoirs, and streams. See Kellogg et al (2010) for
    details on how relative N removal is estimated for each landscape
    sink.
-   `nsink_generate_flowpath()`: In addition to quantifying relative N
    removal, the other key aspect of an `nsink` analysis is the
    flowpath. This function uses a combination of flow determined by
    topography (e.g. via a flow-direction raster) for portions of a
    flowpath that are on land and of downstream flow along the NHDPlus
    stream network.  
-   `nsink_summarize_flowpath()`: The `nsink_summarize_flowpath()`
    function uses the relative nitrogren removal layer and a generated
    flowpath to provide and sink-by-sink (e.g. hydric soils, lakes,
    streams, etc.) summary of relative N removal along a flowpath.
-   `nsink_generate_static_maps()`: An `nsink` analysis can be run at
    the watershed scale by calculating and summarizing the results of
    multiple flowpaths. The `nsink_generate_static_maps()` function does
    this. Four static maps are returned as a list of rasters that
    include 1) N removal efficiency; 2) N loading index; 3) N transport
    index; 4) N delivery index. The N removal efficiciency is a raster
    version of what is created by `nsink_calc_removal()` and is the
    estimated relative N removal capacity of the different landscape
    sinks. The N loading index is a heat map with the cumulative
    relative N removal along flowpaths originating from a grid of points
    (density set by the user) across a watershed, highlighting “leaky”
    areas with less downstream N retention vs. those with higher
    downstream retention. The N loading indes is all N sources based on
    NLCD categories and expressed as an N index ranging from 0 to 1.
    Lastly, the N delivery index is the result of multiplying the N
    loading index and the N transport index and shows potential N
    delivery (again, as an index) from different sources, taking into
    account the potential relative N removal as water moves downstream.
-   `nsink_plot()`: A plot function that plot’s each of the rasters in
    the list returned from `nsink_generate_static_maps()`.  
-   `nsink_build()`: One of the drivers behind the development of the
    `nsink` package was to provide `n-sink` analysis output that could
    be used more broadly (e.g. within a GIS or as part of a web
    application). The `nsink_build()` function is a wrapper function for
    an `nsink` analysis that get and prepare data, calculate N removal,
    and then generate static maps. In addition to the R objects output
    by each function, the resultant datasets are saved as shapefiles or
    TIFF’s for use in an external GIS.
-   `nsink_load()`: This function is essentially the inverse of the
    `nsink_build()` function. It takes a folder of files, likely created
    with `nsink_build()`, and reads them in to R.

The R package documentation contains a detailed description of each of
these functions. Additionally, the package contains a vignette that
outlines a typical workflow for running an N-Sink analysis with the
`nsink` package. Upon install, the vignette is accessed in R with
`vignette("intro", package = "nsink")`.

<!--
## Example workflow

The R package documentation contains a detailed description of each function as well as a detailed workflow, described in the R package vignette.  The following workflow is a simplified example. 

- Step 1: Get HUC ID and use to aacquire datasets



- Step 2: Prepare data



- Step 3: Calculate removal



- Step 4: Flowpath N removal summary





- Step 5: Build static maps



- Step 6: Plot static maps



-->

# Acknowledgements

Many people have contributed in various ways to the development of the
N-Sink concept. In particular, we would like to thank, Chet Arnold, Cary
Chadwick, David Dickson, and Emily Wilson of the University of
Connecticut’s Center for Land Use Education and Research as well as
Peter August, Chris Damon, and Art Gold of the University of Rhode
Island’s Department of Natural Resources Science. Both the UCONN and URI
crews have contributed tremendously to the development of the N-Sink
concept. Additionally, we are grateful to X X, X X, X X, Joe LiVolsi,
Tim Gleason, and Wayne Munns from the US EPA, Atlantic Coastal
Environmental Sciences Division for constructive reviews of this paper.
The views expressed in this article are those of the authors and do not
necessarily represent the views or policies of the U.S. Environmental
Protection Agency. Any mention of trade names, products, or services
does not imply an endorsement by the U.S. Government or the U.S.
Environmental Protection Agency. The EPA does not endorse any commercial
products, services, or enterprises. This contribution is identified by
the tracking number ORD-XXXXX of the Atlantic Coastal Environmental
Sciences Division, Office of Research and Development, Center for
Environmental Measurement and Modeling, US Environmental Protection
Agency.

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-future" class="csl-entry">

Bengtsson, Henrik. 2020. *Future: Unified Parallel and Distributed
Processing in r for Everyone*.
<https://CRAN.R-project.org/package=future>.

</div>

<div id="ref-futurepaper" class="csl-entry">

———. 2021. “<span class="nocase">A Unifying Framework for Parallel and
Distributed Processing in R using Futures</span>.” *The R Journal*.
<https://doi.org/10.32614/RJ-2021-048>.

</div>

<div id="ref-spbook" class="csl-entry">

Bivand, Roger S., Edzer Pebesma, and Virgilio Gomez-Rubio. 2013.
*Applied Spatial Data Analysis with R, Second Edition*. Springer, NY.
<https://doi.org/10.1007/978-1-4614-7618-4>.

</div>

<div id="ref-feddata" class="csl-entry">

Bocinsky, R. Kyle. 2020. *FedData: Functions to Automate Downloading
Geospatial Data Available from Several Federated Data Sources*.
<https://CRAN.R-project.org/package=FedData>.

</div>

<div id="ref-igraphpaper" class="csl-entry">

Csardi, Gabor, and Tamas Nepusz. 2006. “The Igraph Software Package for
Complex Network Research.” *InterJournal* Complex Systems: 1695.
<https://igraph.org>.

</div>

<div id="ref-igraph" class="csl-entry">

Csardi, Gabor, Tamas Nepusz, Szabolcs Horvat, Vincent Traag, and Fabio
Zanini. 2020. *Network Analysis and Visualization*.
<https://CRAN.R-project.org/package=zoo>.

</div>

<div id="ref-gstatpaper2016" class="csl-entry">

Gräler, Benedikt, Edzer Pebesma, and Gerard Heuvelink. 2016.
“Spatio-Temporal Interpolation Using Gstat.” *The R Journal* 8: 204–18.
<https://doi.org/10.32614/RJ-2016-014>.

</div>

<div id="ref-rlang" class="csl-entry">

Henry, Lionel, and Hadley Wickham. 2021. *Rlang: Functions for Base
Types and Core r and ’Tidyverse’ Features*.
<https://CRAN.R-project.org/package=rlang>.

</div>

<div id="ref-raster" class="csl-entry">

Hijmans, Robert J. 2021. *Raster: Geographic Data Analysis and
Modeling*. <https://CRAN.R-project.org/package=raster>.

</div>

<div id="ref-kellogg2010geospatial" class="csl-entry">

Kellogg, DQ, Arthur J Gold, Suzanne Cox, Kelly Addy, and Peter V August.
2010. “A Geospatial Approach for Assessing Denitrification Sinks Within
Lower-Order Catchments.” *Ecological Engineering* 36 (11): 1596–606.
<https://doi.org/10.1016/j.ecoleng.2010.02.006>.

</div>

<div id="ref-sfpaper" class="csl-entry">

Pebesma, Edzer. 2018. “<span class="nocase">Simple Features for R:
Standardized Support for Spatial Vector Data</span>.” *The R Journal* 10
(1): 439–46. <https://doi.org/10.32614/RJ-2018-009>.

</div>

<div id="ref-lwgeom" class="csl-entry">

———. 2021a. *Lwgeom: Bindings to Selected ’Liblwgeom’ Functions for
Simple Features*. <https://CRAN.R-project.org/package=lwgeom>.

</div>

<div id="ref-sf" class="csl-entry">

———. 2021b. *Simple Features for r*.
<https://CRAN.R-project.org/package=sf>.

</div>

<div id="ref-stars" class="csl-entry">

———. 2021c. *Stars: Spatiotemporal Arrays, Raster and Vector Data
Cubes*. <https://CRAN.R-project.org/package=stars>.

</div>

<div id="ref-gstatpaper2004" class="csl-entry">

Pebesma, Edzer J. 2004. “Multivariable Geostatistics in S: The Gstat
Package.” *Computers & Geosciences* 30: 683–91.
<https://doi.org/10.1016/j.cageo.2004.03.012>.

</div>

<div id="ref-sppaper" class="csl-entry">

Pebesma, Edzer J., and Roger S. Bivand. 2005. “Classes and Methods for
Spatial Data in R.” *R News* 5 (2): 9–13.
<https://CRAN.R-project.org/doc/Rnews/>.

</div>

<div id="ref-sp" class="csl-entry">

Pebesma, Edzer, and Roger Bivand. 2021. *Sp: Classes and Methods for
Spatial Data*. <https://CRAN.R-project.org/package=sp>.

</div>

<div id="ref-gstat" class="csl-entry">

Pebesma, Edzer, and Benedikt Graeler. 2021. *Gstat: Spatial and
Spatio-Temporal Geostatistical Modelling, Prediction and Simulation*.
<https://CRAN.R-project.org/package=gstat>.

</div>

<div id="ref-unitspaper" class="csl-entry">

Pebesma, Edzer, Thomas Mailund, and James Hiebert. 2016. “Measurement
Units in R.” *R Journal* 8 (2): 486–94.
<https://doi.org/10.32614/RJ-2016-061>.

</div>

<div id="ref-units" class="csl-entry">

Pebesma, Edzer, Thomas Mailund, Tomasz Kalinowski, and Iñaki Ucar. 2021.
*Units: Spatiotemporal Arrays, Raster and Vector Data Cubes*.
<https://CRAN.R-project.org/package=units>.

</div>

<div id="ref-foreign" class="csl-entry">

R Core Team. 2020. *Foreign: Read Data Stored by ’Minitab’, ’s’, ’SAS’,
’SPSS’, ’Stata’, ’Systat’, ’Weka’, ’dBase’, ...*
<https://CRAN.R-project.org/package=foreign>.

</div>

<div id="ref-rabalais2002beyond" class="csl-entry">

Rabalais, Nancy N, R Eugene Turner, and Donald Scavia. 2002. “Beyond
Science into Policy: Gulf of Mexico Hypoxia and the Mississippi River:
Nutrient Policy Development for the Mississippi River Watershed Reflects
the Accumulated Scientific Evidence That the Increase in Nitrogen
Loading Is the Primary Factor in the Worsening of Hypoxia in the
Northern Gulf of Mexico.” *BioScience* 52 (2): 129–42.
[https://doi.org/10.1641/0006-3568(2002)052\[0129:BSIPGO\]2.0.CO;2](https://doi.org/10.1641/0006-3568(2002)052[0129:BSIPGO]2.0.CO;2).

</div>

<div id="ref-fasterize" class="csl-entry">

Ross, Noam. 2020. *Fasterize: Fast Polygon to Raster Conversion*.
<https://CRAN.R-project.org/package=fasterize>.

</div>

<div id="ref-furrr" class="csl-entry">

Vaughan, Davis, and Matt Dancho. 2021. *Furrr: Apply Mapping Functions
in Parallel Using Futures*. <https://CRAN.R-project.org/package=furrr>.

</div>

<div id="ref-httr" class="csl-entry">

Wickham, Hadley. 2020. *Httr: Tools for Working with URLs and HTTP*.
<https://CRAN.R-project.org/package=httr>.

</div>

<div id="ref-dplyr" class="csl-entry">

Wickham, Hadley, Romain François, Lionel Henry, and Kirill Müller. 2021.
*Dplyr: A Grammar of Data Manipulation*.
<https://CRAN.R-project.org/package=dplyr>.

</div>

<div id="ref-readr" class="csl-entry">

Wickham, Hadley, and Jim Hester. 2020. *Readr: Read Rectangular Text
Data*. <https://CRAN.R-project.org/package=readr>.

</div>

<div id="ref-zoo" class="csl-entry">

Zeileis, Achim, Gabor Gorthendieck, and Jeffrey A. Ryan. 2021. *Zoo: S3
Infrastructure for Regular and Irregular Time Series <span
class="nocase">(Z’s Ordered Observations)</span>*.
<https://CRAN.R-project.org/package=zoo>.

</div>

<div id="ref-zoopaper" class="csl-entry">

Zeileis, Achim, and Gabor Grothendieck. 2005. “Zoo: S3 Infrastructure
for Regular and Irregular Time Series.” *Journal of Statistical
Software* 14 (6): 1–27. <https://doi.org/10.18637/jss.v014.i06>.

</div>

</div>
