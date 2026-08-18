library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(scales)
library(suncalc)
library(mgcv)
library(overlap)

# load files
TL_daily_table_raw <- read.csv("./data_processed/TL_table_cleaned.csv")
AF_daily_table_raw <- read.csv("./data_processed/AF_table_cleaned.csv")

#######################################################################
###                      Tenquille Lake                             ###
###                                                                 ###
#######################################################################

# Calculate sun position of detection getSunlightTimes
# convert time to radians
TL_daily_table_raw <- TL_daily_table_raw %>%
  mutate(
    time_hms = hms::as_hms(Time),
    seconds_since_midnight =
      hour(time_hms) * 3600 +
      minute(time_hms) * 60 +
      second(time_hms),
    time_radians =
      seconds_since_midnight / 86400 * 2 * pi,
    date = as.POSIXct(Date),
    date = force_tz(date, tzone = "America/Vancouver")
  )

# bound to just the summer
TL_daily_table_raw <-  TL_daily_table_raw %>% 
  filter(date <= as.Date("2025-09-22"))

# Create a SpatialPoints object with the location
coords <- matrix(c(-123.06155, 50.52989), nrow=1)

# Calculate the solar position corresponding to each detection time.
TL_daily_table_raw <- TL_daily_table_raw %>% 
  mutate(suntime = sunTime(time_radians, date, coords)) %>% 
  dplyr::select(-time_hms, -seconds_since_midnight, -time_radians)

# Save the processed dataset containing the calculated solar-position
saveRDS(TL_daily_table_raw, "./data_processed/TL_daily_suntime.rds")

#######################################################################
###                        Alex Fraser                              ###
###                                                                 ###
#######################################################################

# Calculate sun position of detection getSunlightTimes
# convert time to radians
AF_daily_table_raw <- AF_daily_table_raw %>%
  mutate(
    time_hms = hms::as_hms(Time),
    seconds_since_midnight =
      hour(time_hms) * 3600 +
      minute(time_hms) * 60 +
      second(time_hms),
    time_radians =
      seconds_since_midnight / 86400 * 2 * pi,
    date = as.POSIXct(Date),
    date = force_tz(date, tzone = "America/Vancouver")
  )

# bound to just the summer
AF_daily_table_raw <-  AF_daily_table_raw %>% 
  filter(date <= as.Date("2025-09-22"))

# Create a SpatialPoints object with the location
coords <- matrix(c(-121.78904, 52.46399), nrow=1)

# Calculate the solar position corresponding to each detection time.
AF_daily_table_raw <- AF_daily_table_raw %>% 
  mutate(suntime = sunTime(time_radians, date, coords)) %>% 
  dplyr::select(-time_hms, -seconds_since_midnight, -time_radians)

# Save the processed dataset containing the calculated solar-position
saveRDS(AF_daily_table_raw, "./data_processed/AF_daily_suntime.rds")