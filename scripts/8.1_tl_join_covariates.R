library(dplyr)
library(lubridate)
library(suncalc)
library(tidyr)
library(purrr)
library(ggplot2)

# load files
TL_daily_filled <- readRDS("./PUBLISH!!!/data_processed/TL_daily_filled.rds")
tl_avg_max_temp <- readRDS("./PUBLISH!!!/data_processed/tl_avg_max_temp_capped.rds")
tl_daily_max_temp <- readRDS("./PUBLISH!!!/data_processed/max_daily_temperature_capped.rds")
canopy_cover_data_joined <- read.csv("./PUBLISH!!!/data_processed/canopy_cover_data.csv")
cam_data <- read.csv("./PUBLISH!!!/data_processed/cam_data_formatted.csv")

#### Join iButton data in filled data table
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

# Give day study average max temp data sets different names
tl_avg_max_temp <- tl_avg_max_temp %>%
  rename(day_max_temp = avg_max_temp)

# Give day camera max temp data sets different names
tl_daily_max_temp <- tl_daily_max_temp %>%
  rename(day_daily_max_temp = daily_max_temp)

# filter to just fall equinox
TL_daily_table <- TL_daily_table %>% 
  filter(Date <= as.Date("2025-09-22"))

# save the table to be used for modeling
saveRDS(TL_daily_table, "./PUBLISH!!!/data_processed/TL_daily_table_covariates.rds")
