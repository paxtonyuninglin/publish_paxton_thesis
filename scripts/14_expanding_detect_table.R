# load packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

#######################################################################
###                      Tenquille Lake                             ###
###                                                                 ###
#######################################################################

# load detection and cam functional dates
tl_suntime <- readRDS("./data_processed/TL_daily_suntime.rds")
TL_daily_table <- readRDS("./data_processed/TL_daily_table_covariates.rds")

# Target species
target_species <- c("Mule deer", "Black bear", "Snowshoe hare")

# filter to just species of interest
tl_suntime <- tl_suntime %>% 
  filter(Species %in% target_species)

# rename suntime 
tl_suntime <- tl_suntime %>% 
  dplyr::rename(Suntime = suntime)

## Divide the 24-hour cycle into 24 sun-time intervals
# Define the width of each time interval.
step <- 2 * pi / 24

# Round each detection's sun position to the nearest one-hour interval.
tl_suntime <- tl_suntime %>%
  mutate(
    Suntime = round(Suntime / step) * step
  )

# fill in suntime intervals
camera_days_24_tl <- TL_daily_table %>%
  uncount(24) %>%
  group_by(Station, Date, Species) %>%
  mutate(
    interval = 0:23,
    Suntime = interval * step
  ) %>%
  ungroup() %>% 
  dplyr::select(1:3, 9:10)

# obtain max temp of day and night for TL
avg_temp_lookup <- TL_daily_table %>%
  dplyr::select(Date, Species, avg_max_temp) %>%
  distinct()

# Add daily maximum temperature to every hourly interval.
camera_days_24_tl <- camera_days_24_tl %>%
  left_join(avg_temp_lookup, by = c("Date", "Species")) %>%
  mutate(
    temp = if_else(interval == 14, avg_max_temp, NA_real_)
  ) %>%
  arrange(Station, Species, Date, interval) %>%
  group_by(Station, Species) %>%
  fill(temp, .direction = "down") %>%
  ungroup() %>%
  dplyr::select(-avg_max_temp)

# make objects all the same class
camera_days_24_tl <- camera_days_24_tl %>%
  mutate(
    Date = as.POSIXct(
      paste(Date),
      format = "%Y-%m-%d"
    ),
    Date = force_tz(Date, tzone = "America/Vancouver")
  )

#### Join detections and fill missing values with 0
# Count detections in each suntime bucket
det_counts <- tl_suntime %>%
  dplyr::group_by(Station, Date, Species, Suntime) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

# make objects all the same class
det_counts <- det_counts %>%
  mutate(
    Date = as.POSIXct(
      paste(Date),
      format = "%Y-%m-%d"
    ),
    Date = force_tz(Date, tzone = "America/Vancouver")
  )

## Join detection counts to the complete 24-hour grid
camera_days_24_tl <- camera_days_24_tl %>%
  left_join(
    det_counts,
    by = c("Station", "Date", "Species", "Suntime")
  ) %>%
  mutate(
    n = replace_na(n, 0)
  ) %>% 
  filter(!is.na(temp))

## Scale temperature within species
camera_days_24_tl <- camera_days_24_tl %>%
  group_by(Species) %>%
  mutate(
    temp_scaled = as.numeric(scale(temp))
  ) %>%
  ungroup()

# save for trig GLMM model
saveRDS(camera_days_24_tl, "./data_processed/full_grid_tl_new.rds")

#######################################################################
###                        Alex Fraser                              ###
###                                                                 ###
#######################################################################

# load detection and cam functional dates
af_suntime <- readRDS("./data_processed/AF_daily_suntime.rds")
AF_daily_table <- readRDS("./data_processed/AF_daily_table_covariates.rds")

# Target species
target_species <- c("Mule deer", "Black bear", "Snowshoe hare", "Red squirrel")

# filter to just species of interest
af_suntime <- af_suntime %>% 
  filter(Species %in% target_species)

# rename suntime 
af_suntime <- af_suntime %>% 
  dplyr::rename(Suntime = suntime)

## Divide the 24-hour cycle into 24 sun-time intervals
# Define the width of each time interval.
step <- 2 * pi / 24

# Round each detection's sun position to the nearest one-hour interval.
af_suntime <- af_suntime %>%
  mutate(
    Suntime = round(Suntime / step) * step
  )

# fill in suntime intervals
camera_days_24_af <- AF_daily_table %>%
  uncount(24) %>%
  group_by(Station, Date, Species) %>%
  mutate(
    interval = 0:23,
    Suntime = interval * step
  ) %>%
  ungroup() %>% 
  dplyr::select(1:3, 9:10)

# obtain max temp of day and night for AF
avg_temp_lookup <- AF_daily_table %>%
  dplyr::select(Date, Species, avg_max_temp) %>%
  distinct()

# Add daily maximum temperature to every hourly interval.
camera_days_24_af <- camera_days_24_af %>%
  left_join(avg_temp_lookup, by = c("Date", "Species")) %>%
  mutate(
    temp = if_else(interval == 14, avg_max_temp, NA_real_)
  ) %>%
  arrange(Station, Species, Date, interval) %>%
  group_by(Station, Species) %>%
  fill(temp, .direction = "down") %>%
  ungroup() %>%
  dplyr::select(-avg_max_temp)

# make objects all the same class
camera_days_24_af <- camera_days_24_af %>%
  mutate(
    Date = as.POSIXct(
      paste(Date),
      format = "%Y-%m-%d"
    ),
    Date = force_tz(Date, tzone = "America/Vancouver")
  )

#### Join detections and fill missing values with 0
# Count detections in each suntime bucket
det_counts <- af_suntime %>%
  dplyr::group_by(Station, Date, Species, Suntime) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

# make objects all the same class
det_counts <- det_counts %>%
  mutate(
    Date = as.POSIXct(
      paste(Date),
      format = "%Y-%m-%d"
    ),
    Date = force_tz(Date, tzone = "America/Vancouver")
  )

## Join detection counts to the complete 24-hour grid
camera_days_24_af <- camera_days_24_af %>%
  left_join(
    det_counts,
    by = c("Station", "Date", "Species", "Suntime")
  ) %>%
  mutate(
    n = replace_na(n, 0)
  ) %>% 
  filter(!is.na(temp))

## Scale temperature within species
camera_days_24_af <- camera_days_24_af %>%
  group_by(Species) %>%
  mutate(
    temp_scaled = as.numeric(scale(temp))
  ) %>%
  ungroup()

# save for trig GLMM model
saveRDS(camera_days_24_af, "./data_processed/full_grid_af_new.rds")
