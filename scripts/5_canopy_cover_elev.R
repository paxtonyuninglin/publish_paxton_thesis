#######################################################################
###                        Densiometer Reading                      ###
#######################################################################

#checking to see if packages are loaded
library(ggplot2)
library(vegan)
library(raster)
library(plyr)
library(dplyr)
library(ggpubr)

#### load and process the canopy cover data
# load the raw csv data file
densiometerrawdata <- read.csv("./data_raw/densiometer.csv")

# rename "site" column to "Station" and rename all site names to standard
densiometerrawdata <- densiometerrawdata %>% 
  dplyr::rename(Station = station_ID) %>% 
  mutate(Station = tolower(Station))

# filter na's
densiometerrawdata <- densiometerrawdata %>%
  filter(!is.na(Station), trimws(Station) != "")

# save variables as numeric
densiometerrawdata$canopy_cover_avg <- as.numeric(densiometerrawdata$canopy_cover_avg)

# save data file
write.csv(densiometerrawdata, "./data_processed/canopy_cover_data.csv", row.names = FALSE)

#####
# elevation
cam_data <- read.csv("./data_raw/cam_data.csv")

# rename "site" column to "Station" and rename all site names to standard
cam_data <- cam_data %>% 
  dplyr::rename(Station = Station.id) %>% 
  mutate(Station = tolower(Station))

# rename elevation column
cam_data_formatted <- cam_data %>% 
  dplyr::rename(Elevation = Elevation..m.)

# save formatted cam data
write.csv(cam_data_formatted, "./data_processed/cam_data_formatted.csv", row.names = FALSE)
