# Wildfowl SLR

This repository contains the code to make all figures for a report comissioned by Natural England containing two systematic reviews of the impacts of disturbance from wildfowling and recreational activities and mortality from wildfowling on waterbirds. It also contains code to make figures summarising population trends of UK waterbirds using data from the 2019/20 Wetland Bird Survey report from the British Trust for Ornithology (Frost et al. 2021)

## Authors:

- Liam Langley
- Aimée McIntosh
- Stuart Bearhop

## Repository Overview

### R Scripts

A folder containing all R scripts used to create figures for the report.

- Scripts with titles starting with "disturbance" are used to create figures for the systematic review of disturbance impacts on waterbird populations.
- Scripts with titles starting with "mortality" are used to create figures for the systematic review of mortality impacts on waterbird populations.
- Script "Population_trends_figures_Rscript" is used to create figures for the summary of population trends of English waterbirds. 

## Data Description

### Systematic Reviews

- Data/Disturbance_final_data_cleaned.csv - File containing information extracted from our final sample of papers read for the Disturbance SLR. This includes some papers which were ultimately removed from the final sample.
- Data/DMortality_final_data_cleaned.csv - File containing information extracted from our final sample of papers read for the Mortality SLR. This includes some papers which were ultimately removed from the final sample.

### Population Trends summary

All data for the population trends summary was downloaded from the BTO 2019/20 online WeBS report on 23/03/2023 - https://www.bto.org/our-science/projects/wetland-bird-survey/publications/webs-annual-report

- Data/Pop_trends - A folder containing separate .csv files of smoothed annual population indices for all waterbird populations assessed in our population trend summary. 
- Data/Pop_trends_metadata.csv - a file containing metadata information for each waterbird population assessed in our population trends summary. 

## Data Attribution

This repository contains Wetland Bird Survey (WeBS) data from Waterbirds in the UK 2019/20 © copyright and database right 2021. WeBS is a partnership jointly funded by the BTO, RSPB and JNCC, in association with WWT, with fieldwork conducted by volunteers.
