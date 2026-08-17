library(ggplot2)
library(raster)
library(dplyr)
library(lubridate)

## Load table files
af_temp <- read.csv("./data_raw/af_temp.csv")

# create standardized datetime column
af_temp <- af_temp %>%
  rename(DateTime = datetime) %>%
  mutate(
    DateTime = ymd_hms(DateTime),
    DateTime = force_tz(DateTime, tzone = "America/Vancouver"),
    DateTime = round_date(DateTime, unit = "hour")
  )

# filter to fall equinox to bound data to the summer
af_temp <- af_temp %>% 
  dplyr::rename(Station = site) %>% 
  filter(DateTime <= as.Date("2025-09-23", tz = "America/Vancouver")) 

# Create Date and time column
af_temp <- af_temp %>%
  mutate(
    Date = as.Date(DateTime, tz = "America/Vancouver"),
    time = format(DateTime, "%H:%M:%S", tz = "America/Vancouver")
  )

#####
# Calculate daily max temp for camera station for the given day from iButton data
af_daily_max_temp <- af_temp %>% 
  group_by(Date, Station) %>%
  dplyr::summarise(daily_max_temp = max(temp, na.rm = TRUE), .groups = "drop")

#save daily max temp
saveRDS(af_daily_max_temp, "./data_processed/af_daily_max_temp.rds")

# Calculate daily max temp for study area for the given day from iButton data
af_avg_max_temp <- af_daily_max_temp %>%
  group_by(Date) %>%
  dplyr::summarise(avg_max_temp = mean(daily_max_temp, na.rm = TRUE), .groups = "drop")

#save daily avg max temp across the study area
saveRDS(af_avg_max_temp, "./data_processed/af_avg_max_temp.rds")

# save processed temp raw data
saveRDS(af_temp, "./data_processed/af_temp_clean.rds")
