#######################################################################
###                         Daily Table                             ###
###                          Alex Fraser                            ###
#######################################################################
library(dplyr)
library(lubridate)
library(suncalc)
library(tidyr)

#### Create days for 0 detection for black bear, mule deer, snowshoe hare, and red squirrel
# load camera data
workflow_af <- read.csv("./data_raw/cam_workflow_af.csv")

workflow_af <- workflow_af %>% 
  mutate(start_date = dmy(start_date)) %>% 
  mutate(end_date = dmy(end_date)) %>% 
  mutate(camera = tolower(camera)) %>% 
  dplyr::rename(Station = camera) %>% 
  dplyr::select(Station, start_date, end_date)

# save workflow
write.csv(workflow_af, "./data_processed/workflow_af.csv", row.names = FALSE)

# Create column for daily detections, 
AF_daily_table <- AF_daily_table_raw %>%
  dplyr::select(-Time) %>%
  group_by(Station, Species, Date) %>%
  dplyr::summarise(
    daily_detections = n()
    )

#### Fill in 0 detection dates for species of interest
# Target species
target_species <- c("Mule deer", "Black bear", "Snowshoe hare", "Red squirrel")

# Filter detection table for only target species
filt_AF_daily_table <- AF_daily_table %>% 
  filter(Species %in% target_species)

# Expand each camera into a full sequence of operational days
camera_days <- workflow_af %>%
  mutate(all_days = purrr::map2(start_date, end_date, ~ seq(.x, .y, by = "day"))) %>%
  tidyr::unnest(all_days) %>%   # safe filtering
  dplyr::rename(Date = all_days) %>%
  dplyr::select(-(2:3))

# Cross with the four species
full_grid <- camera_days %>%
  crossing(Species = target_species)

# Join detections and fill missing values with 0
AF_daily_filled <- full_grid %>%
  left_join(filt_AF_daily_table, 
            by = c("Station", "Species", "Date")) %>%
  mutate(daily_detections = replace_na(daily_detections, 0))

# save table for joining temperature data
saveRDS(AF_daily_filled, "./data_processed/AF_daily_filled.rds")
