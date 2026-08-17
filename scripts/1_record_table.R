#######################################################################
###                   Kernel Density Estimation                     ###
###                     Exploratory analysis                        ###
#######################################################################
knitr::opts_chunk$set(echo = TRUE)
library(vroom) #faster loading of large datasets
library(dplyr) #dplyr is a set of r functions for more streamlined data manipulation and management
library(ggplot2) #a more powerful function for data visualization
library(camtrapR) #set of functions for managing camera trap data, from raw images to detection tables for occupancy analysis
library(secr) #
library(lubridate) #functions for manipulating date-time data
library(unmarked) #statistical models of occupancy and abundance for "unmarked" animals
library(overlap)
library(stringr)
library(patchwork)

##Configuring exiftool
# Check which directories are in PATH (output not shown here)
Sys.getenv("PATH")

# Check if the system can find Exiftool (if output is empty "", system can't find Exiftool)
Sys.which("exiftool")
system("exiftool -ver")

##October: Create record table 
TL_Oct_image_wd <- "E:/tenquille_oct_tagged" # load photos from external hard drive 

length(list.files(TL_Oct_image_wd, pattern = "JPG", recursive = TRUE))

exifTagNames(inDir = TL_Oct_image_wd)

# for October data
TL_Oct_rectable1 <- recordTable(
  inDir = TL_Oct_image_wd,
  IDfrom = "metadata", #Tells camtrapr to look for metadata tag
  minDeltaTime = 5, #minimum time in minutes between detections to be considered independent.
  deltaTimeComparedTo = "lastRecord", #indepndent tags are 5 minutes after last record of species, not after last independent record. 
  timeZone = "Canada/Pacific",
  metadataHierarchyDelimitor = "|",
  metadataSpeciesTag = "Species")

table(TL_Oct_rectable1$Species)

# rename station column to standard format 
TL_Oct_rectable1 <- TL_Oct_rectable1 %>%
  mutate(Station = paste0("tl", sub("tl(\\d+)_oct25", "\\1", Station)))

#Save the new data frame
write.csv(TL_Oct_rectable1, "./data_raw/TL_oct_table.csv", row.names = FALSE)

#Load record table for tenquille lake october
TL_Sept_image_wd <- "E:/tenquille_sept_tagged" # load photos from external hard drive 

length(list.files(TL_Sept_image_wd, pattern = "JPG", recursive = TRUE))

exifTagNames(inDir = TL_Sept_image_wd)

TL_Sept_rectable1 <- recordTable(
  inDir = TL_Sept_image_wd,
  IDfrom = "metadata", #Tells camtrapr to look for metadata tag
  minDeltaTime = 5, #minimum time in minutes between detections to be considered independent.
  deltaTimeComparedTo = "lastRecord", #independent tags are 5 minutes after last record of species, not after last independent record. 
  timeZone = "Canada/Pacific",
  metadataHierarchyDelimitor = "|",
  metadataSpeciesTag = "Species")

table(TL_Sept_rectable1$Species)

# rename station column to standard format 
TL_Sept_rectable1 <- TL_Sept_rectable1 %>%
  mutate(Station = str_to_lower(Station),      # TL-1 → tl-1
         Station = str_replace(Station, "-", ""))  # tl-1 → tl1\

#Load record table for tenquille lake TL 14 july and combine with the rest of TL september
TL14_Sept_image_wd <- "E:/tenquille_jul_tagged" # load photos from external hard drive 

length(list.files(TL14_Sept_image_wd, pattern = "JPG", recursive = TRUE))

exifTagNames(inDir = TL14_Sept_image_wd)

TL14_Sept_rectable1 <- recordTable(
  inDir = TL14_Sept_image_wd,
  IDfrom = "metadata", #Tells camtrapr to look for metadata tag
  minDeltaTime = 5, #minimum time in minutes between detections to be considered independent.
  deltaTimeComparedTo = "lastRecord", #independent tags are 5 minutes after last record of species, not after last independent record. 
  timeZone = "Canada/Pacific",
  metadataHierarchyDelimitor = "|",
  metadataSpeciesTag = "Species")

table(TL14_Sept_rectable1$Species)

# rename station column to standard format 
TL14_Sept_rectable1 <- TL14_Sept_rectable1 %>%
  mutate(Station = str_to_lower(Station),      # TL-1 → tl-1
         Station = str_replace(Station, "-", ""))  # tl-1 → tl1\


#Save the new data frame
write.csv(TL_Sept_rectable1, "./data_raw/TL_sept_table.csv", row.names = FALSE)

#Load the record table for the alex fraser data
AF_image_wd <- "E:/alexfraser_tagged" # load photos from external hard drive 

length(list.files(AF_image_wd, pattern = "JPG", recursive = TRUE))

exifTagNames(inDir = AF_image_wd)


AF_rectable1 <- recordTable(
  inDir = AF_image_wd,
  IDfrom = "metadata", #Tells camtrapr to look for metadata tag
  minDeltaTime = 5, #minimum time in minutes between detections to be considered independent.
  deltaTimeComparedTo = "lastRecord", #indepndent tags are 5 minutes after last record of species, not after last independent record. 
  timeZone = "Canada/Pacific",
  metadataHierarchyDelimitor = "|",
  metadataSpeciesTag = "Species")

table(AF_rectable1$Species)

# rename station name to the standard
AF_rectable1 <- AF_rectable1 %>%
  mutate(Station = sub("_.*", "", Station))

#Save the new data frame
write.csv(AF_rectable1, "./data_raw/AF_table.csv", row.names = FALSE)
