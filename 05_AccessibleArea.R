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
####### 2. Make your species specific environmental variables ##############
############################################################################
biolist <- list.files("/blue/soltis/share/CWR_Proj/rasters/BioClim", pattern = "*.tif", full.names = TRUE)
soillist <- list.files("/blue/soltis/share/CWR_Proj/rasters/SoilGrids", pattern = "*_v2.tif", full.names = TRUE)

climlist <- c(biolist, soillist)

## Order list using gtools
climlist <- mixedsort(sort(climlist))

### Load rasters
climstack <- raster::stack(climlist) 

## Mask and crop bioclim layers
dir.create(paste0(dir, "/", crop, "/rasters"))

# Create Species Training Layers
for(i in 1:length(unique(alldf$name))){
  species <- unique(alldf$name)[i]
  print(species)

  # Subset species from data frame
  spp_df <-  alldf %>%
    dplyr::filter(name == species)

  print("Subsetted")
  # Make spatial
  coordinates(spp_df) <- ~ long + lat
  proj4string(spp_df) <- CRS("+proj=longlat +datum=WGS84")

  ## Create alpha hull, if disjunt distrubution use partCount > 1 and fraction < 1
  # From Pascal! https://github.com/ptitle/rangeBuilder/blob/564b4da2ee6409389cc962fb3f2e91ae38d66467/rangeBuilder/R/getDynamicAlphaHull.R
  sphull <- rangeBuilder::getDynamicAlphaHull(x = as.data.frame(spp_df@coords),
                                              coordHeaders = c("long", "lat"),
                                              fraction = 1, # min. fraction of records we want included (e.g. 1 = 100%)
                                              partCount = 1, # number of polygons
                                              initialAlpha = 10, # initial alpha size
                                              clipToCoast = "terrestrial",
                                              proj = "+proj=longlat +datum=WGS84",
                                              verbose = TRUE)

  print("Alpha hull created")
  ### Visualize
  # plot(sphull[[1]], col=transparentColor('gray50', 0.5), border = NA)
  # points(x = spp_df$long, y = spp_df$lat, cex = 0.5, pch = 3)

  ### Transform into CRS related to meters
  sphullTrans <- spTransform(sphull[[1]], "+proj=cea +lat_ts=0 +lon_0=0")
  spp_dfTrans <- spTransform(spp_df, "+proj=cea +lat_ts=0 +lon_0")

  ### Calculate buffer size
  #### Here we take the 80th quantile of the max distance between points
  spbuffDist <- quantile(x = (apply(spDists(spp_dfTrans), 2, FUN = function(x) sort(x)[2])),
                         probs = 0.80, na.rm = TRUE)

  print("Buffer Calculated")
  ### Buffer the hull
  spbuffer_m <- buffer(x = sphullTrans, width = spbuffDist, dissolve = TRUE)
  spbuffer <- spTransform(spbuffer_m, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  print("Buffer in meters")
  ### Visualize
  # plot(spbuffer, col=transparentColor('gray50', 0.5), border = NA)
  # points(x = spp_df$long, y = spp_df$lat, cex = 0.5, pch = 3)

  ### Crop and Mask
  spec <- gsub(" ", "_", species)

  dir.create(paste0(dir, "/", crop, "/rasters/", "rasters_", spec))
  path <- paste0(dir, "/", crop, "/rasters/", "rasters_", spec,"/")

  end <- ".asc"

  for(j in 1:length(names(climstack))){
    # Subset raster layer
    rast <- climstack[[j]]
    # Setup file names
    name <- names(rast)
    out <- paste0(path, name)
    outfile <- paste0(out, end)
    # Crop and mask
    c <- crop(rast, raster::extent(spbuffer))
    c <- mask(c, spbuffer)
    # Write raster
    writeRaster(c, outfile, format = "ascii", overwrite = TRUE)
  }
  print("Rasters clipped")
}

