#######################################################################
###                        Detection Table                          ###
###                        Tenquille Lake                           ###
#######################################################################
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(ggplot2)

## Load data files
TL_Oct_rectable1 <- read.csv("./data_raw/TL_oct_table.csv")
TL_Sept_rectable1 <- read.csv("./data_raw/TL_sept_table.csv")

#####
# Correct date and time 
## TL8 Oct: Date error: cam says 23/04/25 but reference says 21/09/25
TL_Oct_rectable1 <- TL_Oct_rectable1 %>%
  mutate(DateTimeOriginal = ymd_hms(DateTimeOriginal))

TL_Oct_rectable1 <- TL_Oct_rectable1 %>%
  mutate(Date = as.Date(Date))

TL_Oct_rectable1 <- TL_Oct_rectable1 %>%
  mutate(DateTimeOriginal = if_else(
    Station == "tl8",
    DateTimeOriginal + days(151),
    DateTimeOriginal))  %>%
  mutate(Date = if_else(
    Station == "tl8",
    Date + days(151),
    Date))

## TL8 Sept: Date error: reference says 19/06/2025 but cam says 19/01/2025 (off by 5 months)
TL_Sept_rectable1 <- TL_Sept_rectable1 %>%
  mutate(DateTimeOriginal = ymd_hms(DateTimeOriginal))

TL_Sept_rectable1 <- TL_Sept_rectable1 %>%
  mutate(Date = as.Date(Date))

TL_Sept_rectable1 <- TL_Sept_rectable1 %>%
  mutate(DateTimeOriginal = if_else(
    Station == "tl8",
    DateTimeOriginal + days(151),
    DateTimeOriginal)) %>%
  mutate(Date = if_else(
    Station == "tl8",
    Date + days(151),
    Date))

#### Remove any detection within 24 hours of deployment or servicing
# Camera data files to obtain deployment date and time
TL_Oct_cam <- read.csv("./data_raw/cam_workflow_tl_oct.csv")
TL_Sept_cam <- read.csv("./data_raw/cam_workflow_tl_sept.csv")

# Extract deployment or servicing start date and times, then create a datetime column
TL_Oct_cam  <- TL_Oct_cam %>% 
  dplyr::select(camera, start_date, start_time) %>% 
  dplyr::rename(Station = camera) %>%
  mutate(Station = tolower(Station))

TL_Oct_cam  <- TL_Oct_cam %>% 
  mutate(start_datetime = dmy_hm(paste(start_date, start_time))) %>% 
  dplyr::select(-start_date, -start_time)

TL_Sept_cam <- TL_Sept_cam %>% 
  dplyr::select(Camera, start_date, start_time) %>% 
  dplyr::rename(Station = Camera) %>%
  mutate(Station = tolower(Station))

TL_Sept_cam  <- TL_Sept_cam %>% 
  mutate(start_datetime = dmy_hm(paste(start_date, start_time))) %>% 
  dplyr::select(-start_date, -start_time)

## Get rid of the detection within 24 hours of the set up date and time
TL_Oct_rectable1_capped <- TL_Oct_rectable1 %>% 
  left_join(TL_Oct_cam, by = 'Station') %>% 
  filter(DateTimeOriginal > start_datetime + hours(24)) %>% 
  dplyr::select(-start_datetime)

TL_Sept_rectable1_capped <- TL_Sept_rectable1 %>% 
  left_join(TL_Sept_cam, by = 'Station') %>% 
  filter(DateTimeOriginal > start_datetime + hours(24)) %>% 
  dplyr::select(-start_datetime)

## Join the record tables TL record tables
TL_rectable1 <- bind_rows(
  TL_Oct_rectable1_capped,
  TL_Sept_rectable1_capped)

#### 
# Make daily table
TL_daily_table_raw <- TL_rectable1 %>%
  dplyr::select(Station, Species, DateTimeOriginal, Date, Time)

# correct camera datetime to correct timezone
TL_daily_table_raw <- TL_daily_table_raw %>%
  mutate(
    DateTimeOriginal = force_tz(DateTimeOriginal, tzone = "America/Vancouver")
  )

# filter to just fall equinox
TL_daily_table_raw <- TL_daily_table_raw %>% 
  filter(Date <= as.Date("2025-09-22"))

# save the table to be used for 24 hour analysis
write.csv(TL_daily_table_raw, "./data_processed/TL_table_cleaned.csv", row.names = FALSE)