library(ggplot2)
library(vegan)
library(raster)
library(dplyr)
library(lubridate)

## Load tenquille lake temp table files
tl_temp <- read.csv("./data_raw/tl_temp.csv")

## Clean station names
tl_temp <- tl_temp %>% 
  dplyr::rename(Station = site) %>%
  mutate(Station = sub("^tl0+", "tl", Station))

## create datetime column
tl_temp <- tl_temp %>% 
  dplyr::rename(DateTime = datetime) %>% 
  dplyr::mutate(DateTime = ymd_hms(DateTime))   %>%
  dplyr::mutate(DateTime = force_tz(DateTime, tzone = "America/Vancouver")) %>% 
  mutate(DateTime = round_date(DateTime, unit = "hour"))

## Identify stations where the same DateTime value occurs for
# 5 or more consecutive observations
problem_stations <- tl_temp %>%
  group_by(Station) %>%
  filter(any(ave(DateTime, cumsum(DateTime != lag(DateTime,default = first(DateTime))), FUN = length) >= 5)) %>%
  distinct(Station)

problem_stations

## Calculate repeated-timestamp runs
# For each station, identify consecutive runs of identical DateTime values.
tl_temp <- tl_temp %>%
  group_by(Station) %>%
  
  # TRUE when the current timestamp differs from the previous timestamp
  mutate(
    dt_change = DateTime != lag(DateTime, default = first(DateTime)),
    
    # Assign a unique ID to each consecutive timestamp run
    run_id = cumsum(dt_change),
    
    # Calculate the number of observations in each timestamp run
    run_length = ave(DateTime, run_id, FUN = length)
  ) %>%
  ungroup()

## Determine where timestamp corrections should begin
# For each station, identify the first problematic timestamp run.
breaks <- tl_temp %>%
  group_by(Station) %>%
  mutate(row_id = row_number()) %>%
  
  # Find the first row belonging to a repeated timestamp run
  # of 5 or more observations.
  summarise(
    first_bad = if (any(run_length >= 5)) {
      min(row_id[run_length >= 5])
      
      # Identify the last row before the problematic section
    } else NA_integer_,
    last_good = if (!is.na(first_bad)) first_bad - 1 else NA_integer_,
    .groups = "drop"
  )

## Determine the last reliable timestamp
breaks <- breaks %>%
  rowwise() %>%
  
  # Get the timestamp five rows before the first problematic section.
  # This is used as the starting point for reconstructing the timestamps.
  mutate(
    last_good_time = if (!is.na(first_bad))
      ymd_hms(tl_temp$DateTime[last_good-5])
    else NA
  ) %>%
  ungroup()

## subtract 5 from breaks
# This accounts for the five repeated/problematic observations that occur
# before the timestamp sequence needs to be reconstructed.
breaks <- breaks %>% 
  mutate (first_bad = first_bad -5,
          last_good = last_good -5)

## Reconstruct problematic timestamps
tl_temp_fixed <- tl_temp %>%
  # Sort observations by station
  arrange(Station, row_number()) %>%
  group_by(Station) %>%
  mutate(
    
    # Add the station-specific first problematic row
    first_bad = breaks$first_bad[match(Station, breaks$Station)],
    
    # Add the station-specific last reliable timestamp
    last_good_time = breaks$last_good_time[match(Station, breaks$Station)],
    
    # Create sequential row numbers within each station
    idx = row_number(),
    
    # Correct timestamps only after the problematic section
    DateTime = case_when(
      is.na(first_bad) ~ DateTime,  
      idx < first_bad ~ DateTime,
      
      # Reconstruct problematic timestamps by assigning hourly intervals
      # beginning from the last reliable timestamp
      TRUE ~ last_good_time + lubridate::hours(idx - first_bad + 1)  
    )
  ) %>%
  ungroup()

## Keep relevant columns
tl_temp_fixed <- tl_temp_fixed %>% 
  dplyr::select(Station, DateTime, temp)

## Cap stations with unreaosnably high temperature readings by the next warmest stations
# tl2 and tl17 have unreasonably high temperatures and will be capped by tl1 and tl20
# Extract correct reference temperatures from tl1 and tl20 by DateTime
tl1_temps  <- tl_temp_fixed %>% filter(Station == "tl1")  %>% 
  dplyr::select(DateTime, tl1_temp = temp)
tl20_temps <- tl_temp_fixed %>% filter(Station == "tl20") %>% 
  dplyr::select(DateTime, tl20_temp = temp)

# Cap the problematic sites tl2 and tl17 by the reference stations
tl_temp_capped <- tl_temp_fixed %>%
  left_join(tl1_temps, by = "DateTime", Station) %>%
  left_join(tl20_temps, by = "DateTime", Station) %>%
  mutate(
    temp = case_when(
      # Before July 2025 (tl20 deployed):
      # Cap tl2 and tl17 temperatures at the corresponding tl1 temperature
      Station %in% c("tl2", "tl17") & DateTime < as.Date("2025-07-02") & temp > tl1_temp  ~ tl1_temp,
      
      # From July 2025 onward (tl20 is deployed):
      # Cap tl2 and tl17 temperatures at the corresponding tl20 temperature   
      Station %in% c("tl2", "tl17") & DateTime >= as.Date("2025-07-01") & temp > tl20_temp ~ tl20_temp,
      
      # All other observations remain unchanged
      TRUE ~ temp)) %>%
  dplyr::select(-tl1_temp, -tl20_temp)  # remove reference temp columns

## Add date and time column to tempearture data
tl_temp_capped <- tl_temp_capped %>%
  mutate(
    date = lubridate::as_date(DateTime),   # no timezone mutation
    time = format(DateTime, "%H:%M:%S")
  )

## tl16 is off by 12 hours
# Shift all tl16 timestamps forward by 12 hours.
tl_temp_capped <- tl_temp_capped %>%
  mutate(
    DateTime = case_when(
      Station == "tl16" ~ DateTime + hours(12),
      TRUE ~ DateTime
    ),
    date = lubridate::as_date(DateTime),
    time = format(DateTime, "%H:%M:%S")
  )

## save data
saveRDS(tl_temp_capped, "./data_processed/tl_temp_capped.rds")
