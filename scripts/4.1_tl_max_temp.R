library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

# Load the data
tl_temp <- readRDS("./data_processed/tl_temp_capped.rds")

# filter to fall equinox to bound data to the summer
tl_temp <- tl_temp %>% 
  filter(date <= as.Date("2025-09-22")) %>% 
  mutate(
    DateTime = force_tz(DateTime, tzone = "America/Vancouver")
  )

# Calculate daily max temp for camera station for the given day from iButton data
max_daily_temperature_capped <- tl_temp %>% 
  group_by(date, Station) %>%
  dplyr::summarise(daily_max_temp = max(temp, na.rm = TRUE), .groups = "drop") %>% 
  dplyr::rename(Date = date)

#save daily max temp for camera station
saveRDS(max_daily_temperature_capped, "./data_processed/max_daily_temperature_capped.rds")

# Calculate daily max temp for study area for the given day from iButton data
tl_avg_max_temp <- max_daily_temperature_capped %>% 
  group_by(Date) %>%
  dplyr::summarise(avg_max_temp = mean(daily_max_temp, na.rm = TRUE), .groups = "drop") 

#save daily avg max temp across the study area
saveRDS(tl_avg_max_temp, "./data_processed/tl_avg_max_temp_capped.rds")