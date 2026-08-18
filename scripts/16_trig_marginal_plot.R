#######################################################################
###         This script takes a long time to run!!                  ###
###                                                                 ###
#######################################################################

# Load Libraries
rm(list = ls())
set.seed(129)
library(dplyr)
library(lubridate)
library(GLMMadaptive)
library(mgcv)
library(ggpubr)
library(forcats) 
library(writexl)
library(purrr)
library(ggplot2)

# load models
tl_deer_trig <- readRDS("./data_processed/tl_deer_trig.rds")
tl_bear_trig <- readRDS("./data_processed/tl_bear_trig.rds")
tl_hare_trig <- readRDS("./data_processed/tl_hare_trig.rds")
af_deer_trig <- readRDS("./data_processed/af_deer_trig.rds")
af_bear_trig <- readRDS("./data_processed/af_bear_trig.rds")
af_hare_trig <- readRDS("./data_processed/af_hare_trig.rds")
af_squirrel_trig <- readRDS("./data_processed/af_squirrel_trig.rds")

# load previous data sets
full_grid_temp_tl <- readRDS("./data_processed/full_grid_tl_new.rds")
full_grid_temp_af <- readRDS("./data_processed/full_grid_af_new.rds")

# reusable plot
plot_effect <- function(model, data) {
  
  temp_raw <- quantile(data$temp,
                       probs = c(0.25, 0.5, 0.75),
                       na.rm = TRUE)
  
  temp_scaled <- quantile(data$temp_scaled,
                          probs = c(0.25, 0.5, 0.75),
                          na.rm = TRUE)
  
  newdat <- expand.grid(
    Suntime = seq(0, 2*pi, length.out = 50),
    temp_scaled = temp_scaled,
    Station = unique(data$Station)[1]
  )
  
  ## add raw temperatures for plotting only
  newdat$temp <- rep(temp_raw, each = 50)
  
  print(names(newdat))   # should include temp_scaled
  
  effectPlotData(
    model,
    newdata = newdat,
    marginal = TRUE
  ) %>%
    mutate(
      pred = plogis(pred),
      low = plogis(low),
      upp = plogis(upp),
      temp = newdat$temp
    )
}

# create plots
bear_tl_plot <- plot_effect(tl_bear_trig, full_grid_temp_tl)
deer_tl_plot <- plot_effect(tl_deer_trig, full_grid_temp_tl)
hare_tl_plot <- plot_effect(tl_hare_trig, full_grid_temp_tl)

bear_af_plot <- plot_effect(af_bear_trig, full_grid_temp_af)
deer_af_plot <- plot_effect(af_deer_trig, full_grid_temp_af)
hare_af_plot <- plot_effect(af_hare_trig, full_grid_temp_af)
squirrel_af_plot <- plot_effect(af_squirrel_trig, full_grid_temp_af)

# Add species labels
deer_tl_plot$Species     <- "Mule deer"
bear_tl_plot$Species     <- "Black bear"
hare_tl_plot$Species     <- "Snowshoe hare"
deer_af_plot$Species     <- "Mule deer"
bear_af_plot$Species     <- "Black bear"
hare_af_plot$Species     <- "Snowshoe hare"
squirrel_af_plot$Species <- "Red squirrel"

# combine
all_preds_tl <- bind_rows(deer_tl_plot, bear_tl_plot, hare_tl_plot)
all_preds_af <- bind_rows(deer_af_plot, bear_af_plot, hare_af_plot, squirrel_af_plot)

# save
saveRDS(all_preds_tl, "./data_processed/all_preds_tl.rds")
saveRDS(all_preds_af, "./data_processed/all_preds_af.rds")

