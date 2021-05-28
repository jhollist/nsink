---
title: 'nsink: An R package for flow path based nitrogen removal estimation'
tags:
- R
- nitrogen
- nitrogen sinks
- landscape
- gis
date: "28 May 2021"
output: word_document
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

The `nsink` package was written to estimate cumulative nitrogen (N) removal along a specified flowpath and is based of the methodologies outlined in Kellogg et al. [ -@kellogg2010geospatial].  `nsink` accesses and downloads all required datasets from public datasets in the United States, prepares that data for use, summarizes N removal along a flowpath and creates a set of static watershed maps that summarize flowpath based nitrogen removal.  Additionally, the results of an `nsink` analysis may be exported to standard geospatial files (e.g. shapefiles and tiff images) for use in a Geographic Information System or other application.  

# Statement of need

Excess nitrogen (N) delivery via surface water to downstream aquatic resources such as coastal estuaries contributes to impaired water quality evidenced by several ecosystem impacts including harmful algal blooms (HABs) and hypoxia [@rabalais2002beyond]. Identifying landscape N sinks (areas where dissolved N is transformed to gaseous N and effectively removed from the aquatic system) and their effect on N delivery at the watershed scale is helpful to watershed managers, land use planners and conservation organizations.  The theoretical underpinnings rely on decades of research and are explained in Kellogg et al. [ -@kellogg2010geospatial]. The landscape nitrogen sinks considered in the approach implemented in `nsink` include wetlands (as hydric soils), lakes, and streams, which are all areas were water flow is slowed and N transformations have time to occur.

The first implementation of this type of approach was done on a case by case basis.  Data acquisition and manipulation were mostly manual processes and the end result could take weeks to months for a single small watershed to be completed (C. Damon, pers. comm.).  Thus, the effort required for the analysis limited it's application and scaling beyond a few pilot studies was not feasible.  The goal of `nsink` was to address this limitation and provide an open source solution that could be run on a single small watershed in minutes to hours without significant manual input.

# The `nsink` package

## Package Installation
The `nsink` package is available for install from <https://github.com/usepa/nsink> and may be installed in R with the following:

```r
# If not installed, install remotes
install.packages("remotes")
# Install nsink from GitHub
remotes::install_github("USEPA/nsink")
```

## Package Details

`nsink` relies on several national scale dataset for the United States.  By limiting our approach to these national  datasets we are ensuring scalability of the application of `nsink` because the datasets will be available for nearly all locations in the United States, the datasets are all available via either an Application Programming Interface (API) or via direct download, and lastly will be consistently formatted across locations. The four datasets that `nsink` uses are the National Hydrography Dataset Plus (NHDPlus), Soil Survey Geographic Database (SSURGO), the National Land Cover Dataset (NLCD) land cover, and the National Land Cover Dataset (NLCD) impervious surface.  

We use all these packages (list em)

Currently, `nink` provides 10 exported functions to facilitate a flowpath based analysis of relative nitrogen removal. 

- `nsink_get_huc_id()`: A function for
- `nsink_get_data()`
- `nsink_prep_data()`
- `nsink_calc_removal()`
- `nsink_generate_flowpath()`
- `nsink_summarize_flowpath()`
- `nsink_generate_static_maps()`
- `nsink_plot()`
- `nsink_build()`
- `nsink_load()`

These static maps include 1) a color-coded map of the estimated N removal capacity of the different landscape sinks; 2) a heat map with the cumulative N removal along flowpaths originating from a grid of points across a watershed, highlighting “leaky” areas with less downstream N retention vs. those with higher downstream retention; c) N sources based on NLCD categories and expressed as an N index ranging from 0 to 1; and d) the result of combining b) and c) showing potential N delivery (again, as an index) from different sources, taking into account the potential N removal as water moves downstream.  As of 

## Example workflow


# Acknowledgements
    
We acknowledge contributions from...
    
# References
