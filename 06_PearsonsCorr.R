## Species Environmental Variables/Predictors
## Modified from and cite: https://github.com/mbelitz/Odo_SDM_Rproj and https://github.com/soltislab/BotanyENMWorkshops
## 03-16-2022

library(raster)
library(gtools)
library(dplyr)
library(rgdal)
library(sp)
library(rangeBuilder)
library(sf)
library(caret)
library(usdm)
library(dismo)
library(stringr)
library(rJava)
library(viridis)

#set working directry 
setwd("/blue/soltis/share/CWR_Proj")


## Read in downloaded raw data frame for each species of interest
args = commandArgs(trailingOnly=TRUE)
dir <- args[1] #example : $(pwd)
crop <- args[2] #example: Brassica_oleracea

## Read in downloaded raw data frame for each species of interest
# dir = "/home/tori.ford/CWR_Proj/data/"
# crop = "Solanum_melongena"


############################################################################
################# 1. Pearson's correlation for later use ###################
############################################################################
clippedlist <- list.files(paste0(dir, "/", crop, "/rasters/", "rasters_", spec ), pattern = "*.asc", full.names = TRUE)
clippedlist2 <- mixedsort(sort(clippedlist))
clippedstack <- raster::stack(clippedlist2)

## Start pearsons' - This can then be used after running ENMs to refer to other correlated variables
corr <- layerStats(climstack, 'pearson', na.rm=TRUE)
print("correlation has completed")

### Isolate only the pearson correlation coefficient and take absolute value
c <- abs(corr$`pearson correlation coefficient`)
print("absolute value has completed")

## write file
write.csv(c, paste0(dir, "/", crop, "/logfiles/Pearson_correlations.csv" ), row.names = FALSE)

