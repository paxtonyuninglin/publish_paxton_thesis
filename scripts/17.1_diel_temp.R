library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(scales)
library(suncalc)
library(mgcv)
library(overlap)

# load data
tl_temp <- readRDS("./data_processed/tl_temp_capped.rds")
af_temp <- readRDS("./data_processed/af_temp_clean.rds")

# convert time to radians
tl_temp_kde <- tl_temp %>%
  mutate(
    time_hms = hms::as_hms(time),
    seconds_since_midnight =
      hour(time_hms) * 3600 +
      minute(time_hms) * 60 +
      second(time_hms),
    time_radians =
      seconds_since_midnight / 86400 * 2 * pi,
    date = as.POSIXct(date),
    date = force_tz(date, tzone = "America/Vancouver")
  )

af_temp_kde <- af_temp %>%
  mutate(
    time_hms = hms::as_hms(time),
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
tl_temp_kde <-  tl_temp_kde %>% 
  filter(date <= as.Date("2025-09-22"))

af_temp_kde <-  af_temp_kde %>% 
  filter(date <= as.Date("2025-09-22"))

# Create a SpatialPoints object with the location
coords_tl <- matrix(c(-123.06155, 50.52989), nrow=1)
coords_af <- matrix(c(-121.78904, 52.46399), nrow=1)

#####
# Convert clock time to solar time
tl_temp_kde <- tl_temp_kde %>% 
  mutate(suntime = sunTime(time_radians, date, coords_tl)) %>% 
  dplyr::select(-time_hms, -seconds_since_midnight, -time_radians)

af_temp_kde <- af_temp_kde %>% 
  mutate(suntime = sunTime(time_radians, date, coords_af)) %>% 
  dplyr::select(-time_hms, -seconds_since_midnight, -time_radians)

# Fit generalized additive models
# Model temperature as a smooth, circular function of solar time.
gam_fit_tl <- gam(temp ~ s(suntime, bs = "cc"), data = tl_temp_kde)
gam_fit_af <- gam(temp ~ s(suntime, bs = "cc"), data = af_temp_kde)


# Create prediction grids
pred_grid_tl <- tibble(
  suntime = seq(0, 2*pi, length.out = 400)
)
pred_grid_af <- tibble(
  suntime = seq(0, 2*pi, length.out = 400)
)

# Generate model predictions and standard errors
tl_pred <- predict(gam_fit_tl, newdata = pred_grid_tl, se.fit = TRUE)
af_pred <- predict(gam_fit_af, newdata = pred_grid_af, se.fit = TRUE)

# Calculate confidence intervals
pred_grid_tl <- pred_grid_tl %>%
  mutate(
    fit = tl_pred$fit,
    se  = tl_pred$se.fit,
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )

pred_grid_af <- pred_grid_af %>%
  mutate(
    fit = af_pred$fit,
    se  = af_pred$se.fit,
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )

# label study areas
tl_pred <- pred_grid_tl %>%
  mutate(Site = "Tenquille")

af_pred <- pred_grid_af %>%
  mutate(Site = "Alex Fraser")

# combine study areas
temp_pred <- bind_rows(tl_pred, af_pred)

# save dataframe to be plotted with trig plot
saveRDS(temp_pred, "./data_processed/temp_pred.rds")
