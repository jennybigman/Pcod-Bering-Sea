# Pcod-Bering-Sea 

[![DOI](https://zenodo.org/badge/387891283.svg)](https://zenodo.org/doi/10.5281/zenodo.10552667)

This repo houses code for the following paper

Bigman, J. S., B. J. Laurel, K. Kearney, A. J. Hermann, W. Cheng, K. K. Holsman, and L. A. Rogers. "Predicting Pacific cod thermal spawning habitat in a changing climate." ICES Journal of Marine Science (2023): fsad096. 

Open access link is here: <https://academic.oup.com/icesjms/advance-article/doi/10.1093/icesjms/fsad096/7204287>

## Author information:
Jennifer Bigman - NOAA Alaska Fisheries Science Center (AFSC), jennifersbigman@gmail.com;
Ben Laurel - AFSC, ben.laurel@noaa.gov;
Kelly Kearney - AFSC, kelly.kearney@noaa.gov;
Al Hermann - Pacific Marine Environmental Laboratory (NOAA/PMEL), albert.j.hermann@noaa.gov;
Wei Cheng - PMEL & Cooperative Institute for Climate, Ocean, & Ecosystem Studies (CICOES), University of Washington, wei.cheng@noaa.gov;
Kirstin Holsman - AFSC, kirstin.holsman@noaa.gov;
Lauren Rogers - AFSC, lauren.rogers@noaa.gov;

## Date of data collection:
Bering 10k ROMS: ongoing, see <https://github.com/kholsman/ACLIM2>;
Code creation: 2021 - 2023

## Geographic location: 
Eastern Bering Sea

## Funders: 
North Pacific Research Board (see <https://projects.nprb.org/>)


## Sharing/Access Information

Licensing information: This article is a work of the United States government. Such works are not entitled to domestic copyright protection under U.S. law and are therefore in the public domain. The findings and conclusions in the paper are those of the author(s) and do not necessarily represent the views of the National Marine Fisheries Service.


# Description of files:

## data preprocessing 

00a - download and transform level 2 bottom temp data: code to download and concatenate Bering 10k ROMS model (Bering 10k) bottom temperature hindcast 

00b - accessing ROMS grid cell area, depth, and domain: code to download grid cell metadata (depth, area, domain) for Bering 10k  

00c - merge are, depth, domain with bottom temps & trim to grid: code to trim Bering 10k domain to area of interest for spawning

00d -  calculate hatch success for hindcasts: code to combine thermal response curve from Laurel & Rogers 2020 (see citation in paper above) with Bering 10k bottom temperature hindcast timeseries

00e - read projected temp data into R: code to import Bering 10k ROMS projections timeseries into R (projections can be found downloaded here: <https://github.com/kholsman/ACLIM2>)

00f - bias correct projected temps (at domain level): code to bias-correct output from Bering 10k (see <https://github.com/kholsman/ACLIM2> for more information on bias correction)

00g - calculate hatch success for projected temps: code to combine thermal response curve from Laurel & Rogers 2020 (see citation in paper above) with Bering 10k bottom temperature timeseries projections


## main analysis 

01 - load each time: code to load libraries, helpful functions, data (hatch success and habitat suitability)

02 - calculating spawning habitat suitability indices: code to calculate the basis for three metrics of spawning habitat suitability (see paper above)

03 - calculating mean latitude metrics: code to calculate the first of three metrics of spawning habitat suitability (see paper above)

04 - calculating area metrics: code to calculate the second of three metrics of spawning habitat suitability (see paper above)

05 - calculating consistency of spawning habitat suitability: code to calculate the third of three metrics of spawning habitat suitability (see paper above)

06 - examining correlations of recruitment & spawning habitat suitability metrics: code to examine whether any relationship exists between spawning habitat suitability and recruitment of Pacific cod

07 - mapping newly-hatched larval distribution: code to map the distribution of Pacific cod larvae from EcoFOCI surveys (see <https://www.ecofoci.noaa.gov/field-operations> for more information)

08 - trends of habitat suitability metrics: code to example whether trends in habitat suitability metrics exist over time

09 - sensitivity analyses: code to examine whether variability in thermal response curve would affect estimates of spawning habitat suitability

The folder "manuscript figure scripts" has all of the code for the figures in the main paper*

R version used for project: 4.1.3, 4.2.1, 4.3
libraries used for project: here; naturalearth; tidyverse; data.table; sd; forcats; lubridate; patchwork; marmap; zoo; RcolorBrewer; beepr; mapdata; runner; tidync; cowplot; scales; glue; MuMin

## Disclaimer
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
