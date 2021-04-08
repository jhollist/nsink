---
title: 'nsink: An R package for flow path based nitrogen removal estimation'
tags:
  - R
  - nitrogen
  - nitrogen sinks
  - landscape
  - gis
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
affiliations:
  - name: U. S. Environmental Protection Agency, Atlantic Coastal Environemntal Sciences Division
    index: 1
  - name: University of Rhode Island, Department of Natural Resources Science
    index: 2
  - name: University of Connecticut, Center for Land Use Education and Research
    index: 3
date: 13 August 2017
bibliography: paper.bib
---
  
# Summary

The `nsink` package was written to simplify the acquisition and management of the spatial data necessary to estimate nitrogen (N) removal within landscape sinks, estimate cumulative N removal along a specified flowpath, and create a set of static watershed maps that summarize flowpath based nitrogen removal. 

# Statement of need

Excess nitrogen (N) delivery via surface water to coastal estuaries 
contributes to impaired water quality evidenced by excess algal blooms 
and hypoxia [@Rabalais:2002]. Identifying landscape N sinks (areas where 
dissolved N is transformed to gaseous N and effectively removed from 
the aquatic system) and their effect on N delivery at the watershed 
scale is helpful to watershed managers, land use planners and conservation 
organizations.  The theoretical underpinnings rely on decades of research and 
are explained in @Kellogg:2009. The landscape nitrogen sinks considered in the
NSink approach include wetlands, streams and ponds, which are all areas were 
water flow is slowed and N transformations have time to occur.

# Package overview

The `nsink` package currently provides 10 exported functions that facilitates a flowpath based analysis of relative nitrogen removal

- `nsink_get_huc_id()`
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


# Acknowledgements
    
We acknowledge contributions from...
    
# References
