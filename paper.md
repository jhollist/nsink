---
title: 'nsink: An R package for flow path based nitrogen removal estimation'
tags:
- R
- nitrogen
- nitrogen sinks
- landscape
- gis
date: "28 May 2021"
output: pdf_document
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

The `nsink` package was written to estimate cumulative nitrogen (N) removal along a specified flowpath and is based of the methodologies outlined in Kellogg et al. [ -@kellogg2010geospatial].  `nsink` accesses and downloads all required datasets from public datasets in the United States, prepares that data for use, summarizes N removal along a flowpath and creates a set of static watershed maps that summarize flowpath based relative N removal.  Additionally, the results of an `nsink` analysis may be exported to standard geospatial files (e.g. shapefiles and tiff images) for use in a Geographic Information System or other application.  

# Statement of need

Excess N delivery via surface water to downstream aquatic resources such as coastal estuaries contributes to impaired water quality and leads to several ecosystem impacts including harmful algal blooms (HABs) and hypoxia [@rabalais2002beyond]. Identifying landscape N sinks (areas where dissolved N is transformed to gaseous N and effectively removed from the aquatic system) and their effect on N delivery at the watershed scale is helpful to watershed managers, land use planners and conservation organizations.  The theoretical underpinnings for identifying N sinks rely on decades of research and are explained in Kellogg et al. [-@kellogg2010geospatial]. ADD IN A FEW SENTENCES ABOUT THE GIST OF RELATIVE NITROGEN REMOVAL.The landscape N sinks considered in the approach implemented in `nsink` include wetlands (as hydric soils), lakes, and streams, as these are all areas were water flow is slowed and N transformations have time to occur.

The first implementation of this type of approach was done on a case by case basis.  Data acquisition and manipulation were mostly manual processes and the end result could take weeks to months for a single small watershed to be completed (C. Damon, pers. comm.).  Thus, the effort required for the analysis limited it's application and scaling an flowpath based N sink analysis beyond a few pilot studies was not feasible.  The goal of `nsink` was to address this limitation and provide an open source solution that could be run on a single small watershed in minutes to hours without significant manual input.

# The `nsink` package

## Package Installation
The `nsink` package is available from <https://github.com/usepa/nsink> and may be installed in R with the following:

```r
# If not installed, install remotes
install.packages("remotes")
# Install nsink from GitHub
remotes::install_github("USEPA/nsink")
```

## Package Details

The `nsink` package is designed around the major steps in running a flowpath based analysis with the following major steps:

1. Prepare for analysis
    - Get required data
    - Prepare that data for analysis
    - Calculate relative N removal layer
2. Run an interactive analysis
    - Calculate a flowpath 
    - Summarize relative N removal along a flowpath
3. Run a watershed based analysis
    - Develop static maps
    - Generate output datasets

### Required Data

The ability to run an `nsink` analysis relies on several national scale dataset for the United States.  By limiting our approach to these national datasets we are ensuring scalability of the application of `nsink` because the datasets will be available for nearly all locations in the United States.  The four datasets that `nsink` uses are the National Hydrography Dataset Plus (NHDPlus), Soil Survey Geographic Database (SSURGO), the National Land Cover Dataset (NLCD) land cover, and the National Land Cover Dataset (NLCD) impervious surface [ADD CITATION FOR DATASETS]. These datasets are all available via either an Application Programming Interface (API) or via direct download (e.g. FTP).   

### Dependencies

To acquire the datasets and run the analysis, `nsink` depends on several existing R packages to facilitate spatial data handling, data acquisition, data management, data analysis and data processing.  These are detailed in Table 1.  

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

Table 1. R Package Dependencies for the `nsink` package

### Functionality

Currently, `nsink` provides 10 exported functions to facilitate a flowpath based analysis of relative N removal. 

- `nsink_get_huc_id()`: A function for searching the name of a USGS Watershed Boundary Dataset Hydrologic Unit (<https://www.usgs.gov/core-science-systems/ngp/national-hydrography/watershed-boundary-dataset>) and retrieving it's 12-digit Hydrologic Unit Code (HUC).  Partial and exact matching of names are enabled.
- `nsink_get_data()`: Many data sources are required for an `nsink` analysis.  This function uses any acceptable (e.g. 2-digit to 12-digit) HUC ID to download the following datasets: NHDPlus, SSURGO, NLCD Land Cover, and the NLCD Impervious.   
- `nsink_prep_data()`: An `nsink` analysis requires data to be in a standard coordinate reference system combined mutliple NHDPlus tables and merges information from different parts of SSURGO.  This function completes these data preparation steps and outputs all data, clipped to the HUC boundary and outputs a list containing all the required data for an `nsink` analysis.
- `nsink_calc_removal()`: Quantifying relative N removal across a landscape is one of the key aspects of an `nsink` analysis and is the sole purpose of this function. The `nsink_calc_removal()` function takes the list object that is returned from `nsink_prep_data()` and calculates N removal for each landscape sink.  This includes relative N removal for hydric soils, lakes, ponds, and reservoirs, and streams.  See Kellogg et al [-@kellogg2010geospatial] for details on how relative N removal is estimated for each landscape sink.
- `nsink_generate_flowpath()`: In addition to quantifying relative N removal, the other key aspect of an `nsink` analysis is the flowpath.  This function uses a combination of flow determined by topography (e.g. via a flow-direction raster) for portions of a flowpath that are on land and of downstream flow along the NHDPlus stream network.   
- `nsink_summarize_flowpath()`: The `nsink_summarize_flowpath()` function uses the relative nitrogren removal layer and a generated flowpath to provide and sink-by-sink (e.g. hydric soils, lakes, streams, etc.) summary of relative N removal along a flowpath. 
- `nsink_generate_static_maps()`: An `nsink` analysis can be run at the watershed scale by calculating and summarizing the results of multiple flowpaths.  The `nsink_generate_static_maps()` function does this. Four static maps are returned as a list of rasters that include 1) N removal efficiency; 2) N loading index; 3) N transport index; 4) N delivery index.  The N removal efficiciency is a raster version of what is created by `nsink_calc_removal()` and is the estimated relative N removal capacity of the different landscape sinks.  The N loading index is a heat map with the cumulative relative N removal along flowpaths originating from a grid of points (density set by the user) across a watershed, highlighting “leaky” areas with less downstream N retention vs. those with higher downstream retention.  The N loading indes is all N sources based on NLCD categories and expressed as an N index ranging from 0 to 1.  Lastly, the N delivery index is the result of multiplying the N loading index and the N transport index and shows potential N delivery (again, as an index) from different sources, taking into account the potential relative N removal as water moves downstream. 
- `nsink_plot()`: A plot function that plot's each of the rasters in the list returned from `nsink_generate_static_maps()`.   
- `nsink_build()`: One of the drivers behind the development of the `nsink` package was to provide `n-sink` analysis output that could be used more broadly (e.g. within a GIS or as part of a web application).  The `nsink_build()` function is a wrapper function for an `nsink` analysis that get and prepare data, calculate N removal, and then generate static maps.  In addition to the R objects output by each function, the resultant datasets are saved as shapefiles or TIFF's for use in an external GIS.
- `nsink_load()`: This function is essentially the inverse of the `nsink_build()` function.  It takes a folder of files, likely created with `nsink_build()`, and reads them in to R.

## Example workflow


The R package documentation contains a detailed description of each function.  A detailed workflow is also described in the R package vignette. 

# Acknowledgements
    
We acknowledge contributions from...
    
# References
