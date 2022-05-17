## Record Cleaning and spatial thinning 
## Modified from Botany ENM workshop Cite: https://github.com/soltislab/BotanyENMWorkshops

### Load Packages
library(dplyr)
library(tidyr)
library(raster)
library(sp)
library(spatstat)
library(spThin)
library(fields)
library(lubridate)
library(CoordinateCleaner)

##set working directory
setwd("/blue/soltis/share/CWR_Proj")

### Load functions 
#### This is a function Shelly created with Natalie Patten. Cite : https://github.com/mgaynor1/CURE-FL-Plants
#### It will be part of their R package gatoRs (Geographic And Taxonomic Occurrence R-based Scrubbing).
source("scripts/functions/gators.R")

## Read in downloaded raw data frame for each species of interest
args = commandArgs(trailingOnly=TRUE)
dir <- args[1] #example : $(pwd)
crop <- args[2] #example: Brassica_oleracea

############################################################################
######################## 1. Read in csv files ##############################
############################################################################

CWR_Dir <- list.files(paste0(dir,"/", crop), pattern = "*.csv", full.names = TRUE)
print(CWR_Dir)

############################################################################
##################### 2. load all functions ################################
############################################################################

###function to start observation file
setup_obsFile <- function(speciesfile){
  species_df <- read.csv(speciesfile)
  obs_raw <- nrow(species_df)
  obs_raw_idigbio <- length(species_df$prov[species_df$prov == "idigbio"])
  obs_raw_gbif <-length(species_df$prov[species_df$prov == "gbif"])
  obs_raw_bison <-length(species_df$prov[species_df$prov == "bison"])
  obs_raw_df <- data.frame(total = obs_raw, idigbio = obs_raw_idigbio, gbif = obs_raw_gbif, bison = obs_raw_bison)
  species_name <- strsplit(speciesfile, '[/]')[[1]][7]
  species_name2 <- strsplit(species_name, '[.]')[[1]][1]
  write.csv(obs_raw_df, file = paste0(dir, "/", crop, "/logfiles/" ,species_name2, "_observations.csv"))
  return(species_name2)
}


### Function to filter raw data based on accepted synonyms
filtering_Synonyms <- function(speciesfile){
  species_df <- read.csv(speciesfile)
  species_name <- strsplit(speciesfile, '[/]')[[1]][7]
  species_name2 <- strsplit(species_name, '[.]')[[1]][1]
  accepted_name <- gsub('_', ' ',species_name2)
  unique_df <- read.csv(file = paste0(dir, "/", crop, "/logfiles/" ,species_name2, "_uniquenames.csv"), header = FALSE)
  species_search <- as.list(levels(unique_df[,1]))
  species_df2 <- filter_fix_names(species_df, listofsynonyms = species_search, acceptedname = accepted_name)
  return(species_df2)
  #write.csv(species_df2, file = paste0("data/download/raw/",crop, "/logfiles/" ,species_name2, "_filtered2.csv"),row.names = FALSE)

}

### function to append observation file
append_obs <- function(species_df, species_name){
  obs_raw <- nrow(species_df)
  obs_raw_idigbio <- length(df$prov[df$prov == "idigbio"])
  obs_raw_gbif <-length(df$prov[df$prov == "gbif"])
  obs_raw_bison <-length(df$prov[df$prov == "bison"])
  obs_raw_df <- data.frame(total = obs_raw, idigbio = obs_raw_idigbio, gbif = obs_raw_gbif, bison = obs_raw_bison)
  write.table(obs_raw_df, file = paste0(dir, "/", crop, "/logfiles/" ,species_name, "_observations.csv"),col.names = FALSE, append = TRUE, sep = ",")
}


## function to merge the two locality columns
merge_loc <- function(species_df){
  species_df$Latitude <- dplyr::coalesce(as.numeric(species_df$Latitude), as.numeric(species_df$spocc.latitude))
  species_df$Longitude <- dplyr::coalesce(as.numeric(species_df$Longitude), as.numeric(species_df$spocc.longitude))
  species_df$date <- dplyr::coalesce(species_df$date, species_df$spocc.date)
  
  species_df <- species_df %>%
    dplyr::select(ID = ID, 
                  name = new_name, 
                  basis = basis, 
                  coordinateUncertaintyInMeters = coordinateUncertaintyInMeters, 
                  informationWithheld = informationWithheld, 
                  lat = Latitude, 
                  long = Longitude, 
                  date = date)
  return(species_df)
}

### function to append observation file with NAs
append_obs2 <- function(species_df, species_name){
  obs_raw <- nrow(species_df)
  obs_raw_idigbio <- NA
  obs_raw_gbif <-NA
  obs_raw_bison <-NA
  obs_raw_df <- data.frame(total = obs_raw, idigbio = obs_raw_idigbio, gbif = obs_raw_gbif, bison = obs_raw_bison)
  write.table(obs_raw_df, file = paste0(dir, "/", crop, "/logfiles/" ,species_name, "_observations.csv"),col.names = FALSE, append = TRUE, sep = ",")
}

## function to filter NA's, Precision, Remove 00s,Remove Cultivated and Outlier coordinates.
# https://www.rdocumentation.org/packages/CoordinateCleaner/versions/2.0-20
Filter1 <- function(species_df){
  species_df <- species_df %>%
    filter(!is.na(long)) %>%
    filter(!is.na(lat))
  species_df$lat <- round(species_df$lat, digits = 2)
  species_df$long <- round(species_df$long, digits = 2)
  species_df <- species_df %>%
    filter(long != 0.00) %>%
    filter(lat != 0.00)
  species_df <- cc_inst(species_df, 
                        lon = "long", 
                        lat = "lat", 
                        species = "name") ## Should we record data lost in log file?
  species_df <- cc_outl(species_df, 
                        lon = "long", 
                        lat = "lat", 
                        species = "name") ## Should we record data lost in log file?
  return(species_df)
}  


### Function to remove Duplicates, Fix dates, separate into Year/Month/Day,Remove Identical Rows
# https://github.com/tidyverse/lubridate
RemoveDups <- function(species_df){
  species_df$date <- lubridate::ymd(species_df$date)
  species_df <- species_df %>%
    mutate(year = lubridate::year(date), 
           month = lubridate::month(date), 
           day = lubridate::day(date))
  species_df <- distinct(species_df, lat, long, year, month, day, .keep_all = TRUE)
  return(species_df)
}  

################################ Removed below ##################################
### function for Spatial Correction, we will retain only one pt per pixel. 
#SpatialCorrection <- function(species_df){
#bio1 <- raster("rasters/BioClim/wc2.1_30s_bio_1.tif") ## Read in raster file
#rasterResolution <- max(res(bio1)) # Set resolution
#Remove a point which nearest neighbor distance is smaller than the resolution size
#  while(min(nndist(species_df[,6:7])) < rasterResolution){
#    nnD_species <- nndist(species_df[,6:7])
#    species_df <- species_df[-(which(min(nnD_species) == nnD_species) [1]), ] ##weird never ending thing
#  }
#  return(species_df)
#}
################################ Removed below ##################################

### Function of spatial thinning based on nearest neighbor
SpatialThin <- function(species_df, species_name){
  nnDm_species <- rdist.earth(as.matrix(data.frame(lon = species_df$long, lat = species_df$lat)), miles = FALSE, R = NULL)
  diag(nnDm_species) <- NA
  thinpar <- min(nnDm_species, na.rm = TRUE)
  print(thinpar)
  keep_species <- spThin::thin(loc.data =  species_df,
                               verbose = FALSE,
                               long.col = "long",
                               lat.col = "lat",
                               spec.col = "name",
                               thin.par = thinpar,
                               reps = 1,
                               locs.thinned.list.return = TRUE,
                               write.files = FALSE)[[1]]
  
  species_df <- species_df %>%
    filter((lat %in% keep_species$Latitude +
              long %in% keep_species$Longitude) == 2)
  
  write.csv(species_df, file = paste0(dir, "/", crop, "/logfiles/" ,species_name, "_cleaned.csv"),row.names = FALSE)
}


############################################################################
#################### 3. Clean data using functions##########################
############################################################################

for(i in CWR_Dir){
  species_name <- setup_obsFile(i)
  #print(species_name)
  
  df <- filtering_Synonyms(i)
  append_obs(df, species_name)
  
  df <- merge_loc(df)
  append_obs2(df, species_name)
  
  df <- Filter1(df)
  append_obs2(df, species_name)
  
  df <- RemoveDups(df)
  append_obs2(df, species_name)
  
  #df <- SpatialCorrection(df)
  #append_obs2(df, species_name)
  
  SpatialThin(df, species_name)
  append_obs2(df, species_name)
}

############################################################################
######## 4. Make a single maxent ready file from cleaned data ##############
############################################################################

### Make maxent file
## Read in all cleaned files
alldf <- list.files(paste0(dir, "/", crop, "/logfiles/"), full.names = TRUE, 
                    recursive = FALSE, include.dirs = FALSE, pattern = "*cleaned.csv")
alldf <- lapply(alldf, read.csv)
alldf <- do.call(rbind, alldf)

## Select needed columns
alldf <- alldf %>%
  dplyr::select(name, basis, lat, long)

## Save Maxent.csv
write.csv(alldf,file = paste0(dir, "/", crop, "/logfiles/", crop, "_maxentready.csv"),row.names = FALSE)
