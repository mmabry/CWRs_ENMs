## Species Environmental Variables/Predictors
## Modified from and cite: https://github.com/mbelitz/Odo_SDM_Rproj and https://github.com/soltislab/BotanyENMWorkshops
## 03-16-2022

library(raster)
library(gtools)
library(ggplot2)
library(dplyr)
library(rgdal)
library(sp)
library(sf)
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
########################## 2. Make map of Taxa #############################
############################################################################

## Change name to character
alldf$name <- as.character(alldf$name)

## set up spatial dataframe
alldfsp <- alldf

## Set basemap
# https://www.rdocumentation.org/packages/ggplot2/versions/3.3.5/topics/borders
world <- borders(database = "world", colour = "gray70", fill = "gray70")

## Plot 
world_map <- ggplot() +
  world +
  geom_point(alldfsp, 
             mapping = aes(x = long, y = lat, shape = basis, color = name), alpha = 1, size= 2) +
  coord_sf(xlim = c(19, 40), ylim = c(33, 43)) + #only use this line if you want to zoom where your taxa occur
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_viridis(discrete = TRUE)

## Save for the crop species
ggsave(paste0(crop, "_worldmap.png"), plot = world_map , path = paste0(dir, "/", crop), height = 7, width = 14)
