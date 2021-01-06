---
  title: 'N_sink: An R package for mapping and evaluating landscape N sinks'
tags:
  - R
- nitrogen
- nitrogen sinks
- landscape
- gis
authors:
  - name: Jeff Hollister
orcid: XXXX-XXXX-XXXX-XXXX
affiliation: "1, 2" # (Multiple affiliations must be quoted)
- name: Author Without ORCID
affiliation: 2
affiliations:
  - name: D. Q. Kellogg
index: 1
- name: Institution 2
index: 2
date: 13 August 2017
bibliography: paper.bib
---
  
  # Summary
Excess nitrogen (N) delivery via surface water to coastal estuaries 
contributes to impaired water quality evidenced by excess algal blooms 
and hypoxia [@Rabalais:2002]. Identifying landscape N sinks (areas where 
dissolved N is transformed to gaseous N and effectively removed from 
the aquatic system) and their effect on N delivery at the watershed 
scale is helpful to watershed managers, land use planners and conservation 
organizations. N-Sink was developed as a web-based tool to visualize and 
explore landscape N sinks at the HUC-12 scale, and makes extensive use 
of widely available GIS data. The theoretical underpinnings rely on 
decades of research and are explained in @Kellogg:2009. The 
landscape N sinks considered in this approach include wetlands, streams 
and ponds, which are all areas were water flow is slowed and N transformations 
have time to occur.

The N-Sink package was written to simplify the acquisition and 
management of the spatial data necessary to estimate N removal 
within each identified landscape sink, estimate cumulative N removal 
along a specified flowpath, and create a set of static watershed maps. 
These static maps include 1) a color-coded map of the estimated N removal 
capacity of the different landscape sinks; 2) a heat map with the 
cumulative N removal along flowpaths originating from a grid of points 
across a watershed, highlighting “leaky” areas with less downstream 
N retention vs. those with higher downstream retention; c) N sources 
based on NLCD categories and expressed as an N index ranging from 0 to 1; 
and d) the result of combining b) and c) showing potential N delivery 
(again, as an index) from different sources, taking into account 
the potential N removal as water moves downstream.

   
    # Figures
    
    Figures can be included like this: ![Example figure.](figure.png)
    
    # Acknowledgements
    
    We acknowledge contributions from...
    
    # References
