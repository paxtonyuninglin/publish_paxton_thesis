library(dplyr)
library(lubridate)
library(suncalc)
library(tidyr)
library(purrr)
library(ggplot2)

# load files
AF_daily_filled <- readRDS("./PUBLISH!!!/data_processed/AF_daily_filled.rds")
cam_data_formatted <- read.csv("./PUBLISH!!!/data_processed/cam_data_formatted.csv")
af_avg_max_temp <- readRDS("./PUBLISH!!!/data_processed/af_avg_max_temp.rds")
af_daily_max_temp <- readRDS("./PUBLISH!!!/data_processed/af_daily_max_temp.rds")
canopy_cover_data_joined <- read.csv("./PUBLISH!!!/data_processed/canopy_cover_data.csv")

# make dates as date
af_avg_max_temp <- af_avg_max_temp %>% 
  mutate(Date = as.Date(Date)) 

af_daily_max_temp <- af_daily_max_temp %>% 
  mutate(Date = as.Date(Date))

#### Join iButton data
# rename
AF_daily_table <- AF_daily_filled

# canopy density
AF_daily_table <- AF_daily_table %>%
  left_join(canopy_cover_data_joined %>% 
              dplyr::select(Station, canopy_cover_avg), by = "Station")

# join elevation 
AF_daily_table <- AF_daily_table %>%
  left_join(cam_data_formatted %>% 
              dplyr::select(Station, Elevation), by = "Station")

# join max temp the data frame and rename the temperature column
AF_daily_table <- AF_daily_table %>%
  left_join(af_daily_max_temp, by = c("Station", "Date"))

# join max temp data across all the iButtons averaged
AF_daily_table <- AF_daily_table %>%
  left_join(af_avg_max_temp, by = "Date")

# Give day study average max temp data sets different names
af_avg_max_temp <- af_avg_max_temp %>%
  rename(day_max_temp = avg_max_temp)

# Give day camera max temp data sets different names
af_daily_max_temp <- af_daily_max_temp %>%
  rename(day_daily_max_temp = daily_max_temp)

# filter to just fall equinox
AF_daily_table <- AF_daily_table %>% 
  filter(Date <= as.Date("2025-09-22"))

# save the table to be used for modeling
saveRDS(AF_daily_table, "./PUBLISH!!!/data_processed/AF_daily_table_covariates.rds")
