---
title: 'nsink: An R package for flow path nitrogen removal estimation'
tags:
- R
- nitrogen
- nitrogen sinks
- landscape
- gis
date: "2021-12-20"
output: github_document
authors:
- name: Jeff Hollister
  orcid: 0000-0002-9254-9740
  affiliation: 1
- name: Dorothy Q. Kellogg
  orcid: 0000-0002-9509-4606
  affiliation: 2
- name: Qian Lei-Parent
  orcid: 0000-0002-1904-2513
  affiliation: 3
- name: Emily Wilson
  orcid: 0000-0003-0035-5752
  affiliation: 3
- name: Cary Chadwick
  orcid: 0000-0002-6952-7535
  affiliation: 3
- name: David Dickson
  orcid: 0000-0002-2660-6460
  affiliation: 3
- name: Arthur Gold
  orcid: 0000-0002-0290-1377
  affiliation: 2
- name: Chester Arnold
  affiliation: 3

bibliography: paper.bib
affiliations:
- name: U. S. Environmental Protection Agency, Atlantic Coastal Environmental Sciences
    Division
  index: 1
- name: University of Rhode Island, Department of Natural Resources Science
  index: 2
- name: University of Connecticut, Center for Land Use Education and Research
  index: 3
---
  
  
# Summary

The `nsink` package estimates cumulative nitrogen (N) removal along a specified flow path and is based on methodologies outlined in Kellogg et al. [ -@kellogg2010geospatial]. For a user-specified watershed (i.e., hydrologic unit code (HUC), `nsink` downloads all required datasets from public datasets in the United States, prepares data for use, summarizes N removal along a flow path and creates several static maps.  The results of an `nsink` analysis may be exported to standard geospatial files for use in other applications.  

# Statement of need

Excess N delivery via surface water to downstream aquatic resources contributes to impaired water quality and impacts ecosystem services including harmful algal blooms (HABs) and hypoxia [@rabalais2002beyond]. Identifying landscape N sinks (i.e., areas where N is effectively removed from the aquatic system) and analyzing N delivery at the watershed scale is helpful to watershed managers, land use planners and conservation organizations.  The theoretical underpinnings for identifying N sinks rely on decades of research and are explained in Kellogg et al. [-@kellogg2010geospatial]. 

Prior N-sink implementations were done case-by-case.  Data acquisition and manipulation were mostly manual and took weeks to months to complete for a single 12-digit HUC.  The effort required for the analysis limited it's application as scaling beyond a few pilot studies was not feasible.  The goal of `nsink` was to address this limitation and provide an open source solution that could be run on a single small watershed (e.g., 12-digit HUC) in minutes to hours with minimal manual input.

# The `nsink` package

## Package Installation
The `nsink` package is available from <https://github.com/usepa/nsink> and may be installed in R with the following:

```r
# If not installed, install remotes
install.packages("remotes")

# Install nsink from GitHub
remotes::install_github("USEPA/nsink", dependencies = TRUE, build_vignettes = TRUE)
```

## Package Details

The `nsink` package is designed around the major steps in running an N-Sink analysis and includes functions for the following tasks:

1. Prepare for analysis
    - Get data
    - Prepare data for analysis
    - Calculate relative N removal layer for hydric soils, lakes and streams.
2. Run a point-based analysis 
    - Calculate a flow path 
    - Summarize relative N removal along a flow path
3. Run a HUC-based analysis
    - Develop static maps
    - Generate output datasets

### Required Data

The ability to run an `nsink` analysis relies on several datasets for the conterminous United States.  By limiting our approach to these national datasets we are ensuring scalability of `nsink` because the datasets will be available for most locations in the United States.  The datasets that `nsink` uses are the National Hydrography Dataset Plus version 2 (NHDPlus), Soil Survey Geographic Database (SSURGO), the National Land Cover Dataset (NLCD) land cover, and the National Land Cover Dataset (NLCD) impervious surface [@moore2019user; @soil2017web; @jin2019overall]. These datasets are all available via an Application Programming Interface (API) or via direct download.   

### Dependencies

The `nsink` package depends on several existing R packages to facilitate spatial data handling, data acquisition, data management, data analysis and data processing.  These are detailed in Table 1.  

Table 1. R package dependencies for the `nsink` package

|Package|Task|Citation|
|-------|----|--------|
|`sf`|Spatial Data Handling and Analysis|@sfpaper; @sf|
|`raster`|Spatial Data Handling and Analysis|@raster|
|`stars`|Spatial Data Handling and Analysis|@stars|
|`fasterize`|Spatial Data Handling and Analysis|@fasterize|
|`lwgeom`|Spatial Data Handling and Analysis|@lwgeom|
|`gstat`|Spatial Data Handling and Analysis|@gstatpaper2004; @gstatpaper2016; @gstat|
|`sp`|Spatial Data Handling and Analysis|@sppaper; @spbook; @sp|
|`units`|Unit Transformations|@unitspaper; @units|
|`FedData`|Data Acquisition|@feddata|
|`httr`|Data Acquisition|@httr|
|`dplyr`|Data Management and Analysis|@dplyr|
|`zoo`|Data Management and Analysis|@zoopaper; @zoo|
|`igraph`|Data Management and Analysis|@igraphpaper; @igraph|
|`readr`|Data Management and Analysis|@readr|
|`foreign`|Data Management and Analysis|@foreign|
|`rlang`|Data Management and Analysis|@rlang|
|`furrr`|Parallel Processing|@furrr|
|`future`|Parallel Processing|@futurepaper; @future|


### Functionality

Currently, `nsink` provides 10 exported functions to facilitate a flow path analysis of relative N removal. The `nsink` repository (<https://github.com/usepa/nsink>) and R package documentation contain detailed documentation of each function.  The pacakge also has a vignette that outlines a typical workflow for running an N-Sink analysis with the `nsink` package.  Upon install, the vignette is accessed in R with `vignette("intro", package = "nsink")`. 

# Acknowledgements
    
Many people have contributed in various ways to the development of the N-Sink concept.  In particular, we would like to thank, Chet Arnold, Cary Chadwick, David Dickson, and Emily Wilson of the University of Connecticut's Center for Land Use Education and Research as well as Peter August, Chris Damon, and Art Gold of the University of Rhode Island's Department of Natural Resources Science.  Both the UCONN and URI crews have contributed tremendously to the development of the N-Sink concept.  Additionally, we are grateful to Stephen Shivers, Michael Dumelle, Justin Bousquin, Joe LiVolsi, Tim Gleason, and Wayne Munns for constructive early reviews of this paper. Lastly, Ken Forshay from the US EPA's Center for Environmental Solutions and Emergency Response deserves our thanks for shepherding the development of N-Sink for many years. The views expressed in this article are those of the authors and do not necessarily represent the views or policies of the U.S. Environmental Protection Agency. Any mention of trade names, products, or services does not imply an endorsement by the U.S. Government or the U.S. Environmental Protection Agency. The EPA does not endorse any commercial products, services, or enterprises. This contribution is identified by the tracking number ORD-044618 of the Atlantic Coastal Environmental Sciences Division, Office of Research and Development, Center for Environmental Measurement and Modeling, US Environmental Protection Agency.
    
# References
