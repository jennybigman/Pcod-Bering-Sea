# Pcod-Bering-Sea 

This repo houses code for the EBS Pacific Cod habitat suitability project - now published: Bigman, J. S., B. J. Laurel, K. Kearney, A. J. Hermann, W. Cheng, K. K. Holsman, and L. A. Rogers. "Predicting Pacific cod thermal spawning habitat in a changing climate." ICES Journal of Marine Science (2023): fsad096. Open access link is here: https://academic.oup.com/icesjms/advance-article/doi/10.1093/icesjms/fsad096/7204287

key to files:

data preprocessing -- do not need to run

00a - download and transform level 2 bottom temp data

00b - accessing ROMS grid cell area, depth, and domain 

00c - merge are, depth, domain with bottom temps & trim to grid

00d -  calculate hatch success for hindcasts

00e - read projected temp data into R 

00f - bias correct projected temps (at domain level)

00g - calculate hatch success for projected temps


****start with these files:****

01 - run this script to load libraries, helpful functions, data (hatch success and habitat suitability)

02 - calculating spawning habitat suitability indices

03 - calculating mean latitude metrics

04 - calculating area metrics

05 - calculating consistency of spawning habitat suitability 

06 - examining correlations of recruitment & spawning habitat suitability metrics

07 - mapping newly-hatched larval distribution

08 - trends of habitat suitability metrics

09 - sensitivity analyses 

* The folder "manuscript figure scripts" has all of the code for the figures in the main paper*



