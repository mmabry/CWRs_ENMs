### Project ENM globally
# CITE: https://github.com/soltislab/BotanyENMWorkshops

# Load Packages
library(tidyverse)
library(raster)
library(gtools)
library(dplyr)
library(dismo)
library(ENMeval)
library(devtools)
#install_github("marlonecobos/kuenm")
library(kuenm)
library(ggplot2)
library(ggspatial)
library(viridis)

#setwd
setwd("/blue/soltis/share/CWR_Proj")


args = commandArgs(trailingOnly=TRUE)
dir <- args[1] #example : $(pwd)
crop <- args[2] #example: Brassica_oleracea

# dir = "/home/tori.ford/CWR_Proj/data"
# crop = "Brassica_oleracea"

############################################################################
##################### 1. load maxent species file ##########################
############################################################################
# Load data file just to get species names
alldf <- read.csv(paste0(dir, "/", crop, "/logfiles/", crop, "_maxentready.csv"))
#alldf <- dplyr::filter(alldf, name == c("Solanum rigidum")) #"Solanum incanum"

############################################################################
############## 2. load Global Environmental rasters ########################
############################################################################
biolist <- list.files("/blue/soltis/share/CWR_Proj/rasters/BioClim", pattern = "*.tif", full.names = TRUE)
soillist <- list.files("/blue/soltis/share/CWR_Proj/rasters/SoilGrids", pattern = "*_v2.tif", full.names = TRUE)

climlist <- c(biolist, soillist)

## Order list using gtools
climlist <- mixedsort(sort(climlist))

### Load rasters
climstack <- raster::stack(climlist) 

############################################################################
####################### 3. Project ENMs globally ##########################
############################################################################
for(i in 1:length(unique(alldf$name))){
  species <- unique(alldf$name)[i]
  spec <- gsub(" ", "_", species)
  print(spec)
  spec_subset <- dplyr::filter(alldf, name == species)
  
  # Get names of rasters that were used for species models (using vif)
  specstack <- stack(mixedsort(sort(list.files(path=paste0(dir, "/", crop, "/rasters/rasters_", spec, "/VIF"), full.names = TRUE))))
  layerNames <- names(specstack)
  
  # Get global rasters which match the layers used in your model by the names
  globalRasters <- subset(climstack, layerNames)
  print("Subsetted Global Rasters")
  
  # Load Rdata file of the models
  load(paste0(dir, "/", crop, "/ENM_output/", spec, "_optimalSeq_ENM.RData"))
  
  # Project model to global rasters
  p <- dismo::predict(mod.seq, globalRasters, filename = paste0(dir, "/", crop, "/ENM_output/", spec, "_globalprediction.asc")) #'Brassica_cretica_globalprediction'
  print("Predicting Variable Calculated")
  
  save(p, file = paste0(dir, "/", crop, "/ENM_output/", spec, "_globalpredict.RData"))
  
  ## Test Plottable
  p_df <- rasterToPoints(p); p_df <- data.frame(p_df)
  colnames(p_df) <- c("x","y","Prescence")
  
  ### Visualize
  print("Variable has been made plottable")
  
  ### Plot
  ggplot(data = p_df) + #
    geom_tile(data = p_df, aes(x, y, alpha = Prescence)) +
    scale_fill_gradientn(colours = viridis::viridis(99),
                         na.value = "black") +
    guides(fill = guide_colorbar()) +
    scale_alpha(range = c(0, 0.5)) +
    geom_point(data= spec_subset, 
              mapping = aes(x = long, y = lat), 
              col='red', cex=0.05) +
    coord_quickmap() +
    theme_bw() 
  ### Plot previous
  # ggplot() +
  #   geom_raster(data = p_df, aes(x = x, y = y, fill = layers)) +
  #   geom_point(data= spec_subset, 
  #              mapping = aes(x = long, y = lat), 
  #              col='red', cex=0.05) +
  #   coord_quickmap() +
  #   theme_bw() +
  #   scale_fill_gradientn(colours = viridis::viridis(99),
  #                        na.value = "black")
  
  ### save
  ggsave(paste0(dir, "/", crop, "/ENM_output/", spec, "_ENM_globalProjection.pdf"))

 ## Save Raster 
  writeRaster(x = p, filename = paste0(dir, "/", crop, "/ENM_output/", spec, "_ENM_globalProjection.asc"),
             format = "ascii", NAFlag = "-9999", overwrite = T)
  
}
