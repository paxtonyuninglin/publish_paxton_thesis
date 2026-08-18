#######################################################################
###                        Detection Table                          ###
###                          Alex Fraser                            ###
#######################################################################
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(ggplot2)

## Load data files
AF_rectable1 <- read.csv("./data_raw/AF_table.csv")

# Camera data files to obtain deployment date and time
AF_cam <- read.csv("./data_raw/cam_workflow_af.csv")

#####
# Correct date and time 
## AF11 Time error: says 04:43 but reference says 16:43, off by 12 hours
AF_rectable1 <- AF_rectable1 %>%
  mutate(DateTimeOriginal = ymd_hms(DateTimeOriginal))

AF_rectable1 <- AF_rectable1 %>%
  mutate(DateTimeOriginal = if_else(
    Station == "af11",
    DateTimeOriginal + hours(12),
    DateTimeOriginal)) %>% 
  mutate(
    Date = as.Date(DateTimeOriginal),                 
    Time = format(DateTimeOriginal, "%H:%M:%S"))

## Extract deployment or servicing start date and times, then create a datetime column
AF_cam  <- AF_cam %>% 
  dplyr::select(camera, start_date, start_time) %>% 
  dplyr::rename(Station = camera) %>%
  mutate(Station = tolower(Station))

AF_cam  <- AF_cam %>% 
  mutate(start_datetime = dmy_hm(paste(start_date, start_time))) %>% 
  dplyr::select(-start_date, -start_time)

# Get rid of any detection within 24 hours of the set up date and time
AF_rectable1_capped <- AF_rectable1 %>% 
  left_join(AF_cam, by = 'Station') %>% 
  filter(DateTimeOriginal > start_datetime + hours(24)) %>% 
  dplyr::select(-start_datetime)

#### Make daily table
AF_daily_table_raw <- AF_rectable1_capped %>%
  dplyr::select(Station, Species, DateTimeOriginal, Date, Time)

# correct camera date timezone
AF_daily_table_raw <- AF_daily_table_raw %>%
  mutate(
    DateTimeOriginal = force_tz(DateTimeOriginal, tzone = "America/Vancouver")
  )

# filter to just fall equinox
AF_daily_table_raw <- AF_daily_table_raw %>% 
  filter(Date <= as.Date("2025-09-22"))

# save the table to be used for 24 hour analysis
write.csv(AF_daily_table_raw, "./data_processed/AF_table_cleaned.csv", row.names = FALSE)