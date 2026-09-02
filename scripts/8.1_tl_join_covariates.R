library(dplyr)
library(lubridate)
library(suncalc)
library(tidyr)
library(purrr)
library(ggplot2)

# load files
TL_daily_filled <- readRDS("./data_processed/TL_daily_filled.rds")
tl_avg_max_temp <- readRDS("./data_processed/tl_avg_max_temp_capped.rds")
tl_daily_max_temp <- readRDS("./data_processed/max_daily_temperature_capped.rds")
canopy_cover_data_joined <- read.csv("./data_processed/canopy_cover_data.csv")
cam_data <- read.csv("./data_processed/cam_data_formatted.csv")

#### Join iButton and camera data in filled data table
# canopy density
TL_daily_table <- TL_daily_filled %>%
  left_join(canopy_cover_data_joined %>% 
              dplyr::select(Station, canopy_cover_avg), by = "Station")

# join elevation 
TL_daily_table <- TL_daily_table %>%
  left_join(cam_data %>% 
              dplyr::select(Station, Elevation), by = "Station")

# join max temp the data frame and rename the temperature column
TL_daily_table <- TL_daily_table %>%
  left_join(tl_daily_max_temp, by = c("Station", "Date"))

# join max temp data across all the iButtons averaged
TL_daily_table <- TL_daily_table %>%
  left_join(tl_avg_max_temp, by = "Date")

# filter to just fall equinox
TL_daily_table <- TL_daily_table %>% 
  filter(Date <= as.Date("2025-09-22"))

# save the table to be used for modeling
saveRDS(TL_daily_table, "./data_processed/TL_daily_table_covariates.rds")