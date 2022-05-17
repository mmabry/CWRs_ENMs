## Download Record Data
## Modified from Botany ENM workshop Cite: https://github.com/soltislab/BotanyENMWorkshops
### 2021 06 24

### Load packages 
library(dplyr) 
library(tidyr) 
library(plyr) 
library(spocc) 
library(ridigbio) 
library(tibble) 
library(rbison)


### setwd 
setwd("/blue/soltis/share/CWR_Proj/")

### Load functions 
#### This is a function Shelly created with Natalie Patten. Cite : https://github.com/mgaynor1/CURE-FL-Plants
#### It will be part of their R package gatoRs (Geographic And Taxonomic Occurrence R-based Scrubbing).
source("scripts/functions/DownloadingDataMore.R") ##need to make sure the list_of_wants.csv file can be found. it is hard coded into the function

############################################################################
########################### 1. Make species lists ##########################
############################################################################
Eggplant_list <- c("Solanum aureitomentosum", "Solanum campylacanthum", "Solanum cerasiferum", "Solanum lichtensteinii", "Solanum umtuma", "Solanum usambarense") 
Cabbage_list <- c("Brassica cretica", "Brassica hilarionis") 


############################################################################
########################### 2. Make crop variable ##########################
############################################################################
crop <- "Solanum_melongena"


############################################################################
###### 3. use spocc_combine to pull data from iDigBio, BISON, and GBIF #####
############################################################################
path <- paste0("data/", crop, "/")
end <- ".csv"

# change the list variable below depending on which crop is being downloaded
for(i in Eggplant_list){
  name <-i
  #print(name)
  
  name2 <- gsub(' ', '_',name)
  #print(name2)
  
  out <- paste0(path, name2)
  #print(out)
  
  outfile <- paste0(out, end)
  #print(outfile)
  
  spocc_combine(name, outfile)
}
