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
######################### 1. Read Maxent Ready file ########################
############################################################################

alldf <- read.csv(file = paste0(dir, "/", crop, "/logfiles/", crop, "_maxentready.csv"))

#if wanting to do species by species use the following
#alldf <- dplyr::filter(alldf, name == c("Solanum lichtensteinii")) #"Solanum linnaeanum"


############################################################################
####################### 2. Variable selection ##############################
############################################################################
# Select layers for MaxEnt
## We only want to include layers that are not highly correlated.
## To assess which layers we will include, we will use Variable inflation factors (VIFs) 
## VIF can detect for multicollinearity in a set of multiple regression variables. 
### tutorial which was followed https://rstudio-pubs-static.s3.amazonaws.com/300995_e6e0edf09915431480ed089c3a3e0ff3.html
for(i in  1:length(unique(alldf$name))){
  species <- unique(alldf$name)[i]
  print(species)
  
  ## species name with underscore
  spec <- gsub(" ", "_", species)
  
  ##creat dir
  dir.create(paste0(dir, "/", crop, "/rasters/", "rasters_", spec, "/VIF"))
  
  ### Stack layers for each species
  clippedlist <- list.files(paste0(dir, "/", crop, "/rasters/", "rasters_", spec ), pattern = "*.asc", full.names = TRUE)
  clippedlist2 <- mixedsort(sort(clippedlist))
  clippedstack <- raster::stack(clippedlist2)
  
  #calculate VIFs using a threshold of 10
  stepstack <- vifstep(clippedstack, th=10)
  
  print("VIF calculated")
  # exclude the collinear variables that were identified in the previous step
  v2 <- exclude(clippedstack,stepstack)
  
  print("Variables Excluded")
  ## finally copy the layers we want to a new folder!
  print("Starting to copy the layers")

  ## transfer files into directory per species per method
  for(i in 1:length(names(v2))){
    name <- names(v2)[i]
    print(name)
    
    from <- paste0(dir, "/", crop, "/rasters/", "rasters_", spec, "/", name, ".asc")
    to <- paste0(dir, "/", crop, "/rasters/", "rasters_", spec, "/VIF/", name, ".asc")
    
    file.copy(from, to,
              overwrite = TRUE, recursive = FALSE, 
              copy.mode = TRUE)
  }
}




