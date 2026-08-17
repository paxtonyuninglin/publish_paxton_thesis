#######################################################################
###                         Daily Table                             ###
#######################################################################
library(dplyr)
library(lubridate)
library(suncalc)
library(tidyr)
library(purrr)
library(ggplot2)

#### Create days for 0 detection for black bear, mule deer, snowshoe hare, and red squirrel
# load camera data
workflow_tl_sept <- read.csv("./PUBLISH!!!/data_raw/cam_workflow_tl_sept.csv")
workflow_tl_oct <- read.csv("./PUBLISH!!!/data_raw/cam_workflow_tl_oct.csv")

# Rename columns and select necessary columns
workflow_tl_sept <- workflow_tl_sept %>% 
  mutate(start_date = dmy(start_date)) %>% 
  mutate(Camera = tolower(Camera)) %>% 
  dplyr::rename(Station = Camera) %>% 
  dplyr::select(Station, start_date) 

workflow_tl_oct <- workflow_tl_oct %>% 
  mutate(start_date = dmy(start_date)) %>% 
  mutate(end_date = dmy(end_date)) %>% 
  mutate(camera = tolower(camera)) %>% 
  dplyr::rename(Station = camera) %>% 
  dplyr::select(Station, start_date, end_date)

# Add in tl14 information and correct station 18 date
workflow_tl_oct <- workflow_tl_oct %>%
  mutate(end_date = if_else(
    Station == "tl14",
    dmy("21-09-2025"),
    end_date)) %>% 
  mutate(end_date = if_else(
    Station == "tl8",
    end_date + days(151),
    end_date
  ))

# merge data to get functional cam dates
workflow_tl <- workflow_tl_sept %>%
  left_join(workflow_tl_oct, by = "Station", suffix = c("_1", "_2")) %>%
  mutate(service_date = if_else(
    !is.na(start_date_2) & start_date_2 > start_date_1,
    start_date_2,
    as.Date(NA)
  )) %>% 
  dplyr::select(-start_date_2) %>% 
  dplyr::rename(start_date = start_date_1)

# reorder columns 
workflow_tl <- workflow_tl[, c(1, 2, 4, 3)]

# turn date and times into datetime
workflow_tl <- workflow_tl %>% 
  mutate(
    start_date = start_date + days(1),
    service_date = service_date + days(1)
  )

# save workflow
write.csv(workflow_tl, "./PUBLISH!!!/data_processed/workflow_tl.csv", row.names = FALSE)

# Create column for daily detections, 
TL_daily_table <- TL_daily_table_raw %>%
  dplyr::select(-Time) %>%
  group_by(Station, Species, Date) %>%
  dplyr::summarise(daily_detections = n())

#### Fill in 0 detection dates for species of interest
# Target species
target_species <- c("Mule deer", "Black bear", "Snowshoe hare")

# Filter detection table for only target species
filt_TL_daily_table <- TL_daily_table %>% 
  filter(Species %in% target_species)

# Expand each camera into a full sequence of operational days
camera_days <- workflow_tl %>%
  mutate(all_days = purrr::map2(start_date, end_date, ~ seq(.x, .y, by = "day"))) %>%
  tidyr::unnest(all_days) %>%
  filter(is.na(service_date) | all_days != service_date) %>%   
  dplyr::rename(Date = all_days) %>% 
  dplyr::select(-(2:4))

# Cross with the three species
full_grid <- camera_days %>%
  crossing(Species = target_species)

# Join detections and fill missing values with 0
TL_daily_filled <- full_grid %>%
  left_join(filt_TL_daily_table, 
            by = c("Station", "Species", "Date")) %>%
  mutate(daily_detections = replace_na(daily_detections, 0))

# save table for joining temperature data
saveRDS(TL_daily_filled, "./PUBLISH!!!/data_processed/TL_daily_filled.rds")
