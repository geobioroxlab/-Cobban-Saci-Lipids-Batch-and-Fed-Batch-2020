# Read Me Cobban Batch Experiments 2020

The following describes the files located herein and their functions related to the manuscript tilted "Multiple environmental parameters impact core lipid cyclization in Sulfolobus acidocaldarius." 

All code and data was contributed by Alec Cobban in or before February 2020 unless otherwise recorded.

# Files:

## app.R-
Contains code for an Rshiny app that allows for selecting subsets of the data to visualize in NMDS/CAP. Can either be uploaded onto a shiny server or run via RStudio. Only contains Sulfolobus acidocaldarius data gathered in the Leavitt Lab

## AZData.csv-
Contains data from the chemostatic experiments of Alice Zhou, 2019. https://doi.org/10.1111/1462-2920.14851. Data was recovered both from the supplemental information of that paper, as well as personal correspondence with the first author. 

## CobbanSaciFinalRscripts.Rproj-
An Rproject file, allowing the project to be easily opened in Rstudio. 
## ConsolidatingData.R-
A set of helper methods written to consolidate and monitor data, including growth curves, growth rates, and some lipid information. Run this entire file to get all methods for use in the following file.

## ConsolidatingSaciDataForPublication.R-
All of the commands that were used to consolidate, graph and run statistical analyses on data in this study. 

## dataframe1combined.csv-
All of the batch culture data, including growth curves, rates, and lipid data.

## dataframefornmds.rda-
Data for app.R functionality. Contains most data present here in a consolidated and condensed format that is better suited for NMDS

## dOdf.csv-
All data from the oxygen sparge control experiments. Includes growth curves, rates and lipid data. 

## LiteratureDataCompilation.R-
Code that was run on literature data to consolidate it, and process it in ways that allowed for comparisons with the results of this study. 


# Folders:
## Literature Lipid Data-
Contents are 3 subfolders that contain data from a couple studies of GDGT distribution in archaea

For detailed descriptions of the contents of these folders please email the corresponding author.

### Boyd
The results of GDGT distribution in Picrophilus from 10.3389/fmicb.2016.01323


### Elling
The results of GDGT distribution experiments in Nitrosopumilus in https://doi.org/10.1016/j.gca.2014.07.005

### Hurley
The results of chemostatic control GDGT distribution experiments in Nitrosopumilus in https://doi.org/10.1073/pnas.1518534113