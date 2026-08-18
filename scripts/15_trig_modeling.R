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

# load data
full_grid_temp_tl <- readRDS("./data_processed/full_grid_tl_new.rds")
full_grid_temp_af <- readRDS("./data_processed/full_grid_af_new.rds")

# convert to binary yes or no
full_grid_temp_tl <- full_grid_temp_tl %>%
  mutate(presence = as.integer(n > 0))

full_grid_temp_af <- full_grid_temp_af %>%
  mutate(presence = as.integer(n > 0))

#######################################################################
###                        Tenquille Lake                           ###
###                                                                 ###
#######################################################################

# random intercept-only trigonometric GLMM
trig_model <- function(species_name, data) {
  
  dat <- data %>%
    filter(Species == species_name)
  
  mixed_model(
    fixed =
      presence ~
      cos(Suntime) * temp_scaled  +
      sin(Suntime) * temp_scaled  +
      cos(2 * Suntime) * temp_scaled  +
      sin(2 * Suntime) * temp_scaled ,
    random = ~1 | Station,
    data = dat,
    family = binomial()
  )
}

# save each model for marginal plots
tl_bear_trig <- trig_model("Black bear", full_grid_temp_tl)
tl_deer_trig <- trig_model("Mule deer", full_grid_temp_tl)
tl_hare_trig <- trig_model("Snowshoe hare", full_grid_temp_tl)

# extract coefficients 
extract_results <- function(model, species_name) {
  
  out <- as.data.frame(summary(model)$coef_table)
  out$term <- rownames(out)
  out$species <- species_name
  rownames(out) <- NULL
  
  # Optional: put term first
  out <- out %>%
    dplyr::select(species, term, everything())
  
  out
}

# combine everything
results_clean <- bind_rows(
  extract_results(tl_bear_trig, "Black bear"),
  extract_results(tl_deer_trig, "Mule deer"),
  extract_results(tl_hare_trig, "Snowshoe hare")
)

# export
write_xlsx(
  results_clean,
  "./model_results_tabled/results_species_models.xlsx"
)

# save models
saveRDS(tl_deer_trig, "./data_processed/tl_deer_trig.rds")
saveRDS(tl_bear_trig, "./data_processed/tl_bear_trig.rds")
saveRDS(tl_hare_trig, "./data_processed/tl_hare_trig.rds")

#######################################################################
###                         Alex Fraser                             ###
###                                                                 ###
#######################################################################

# random intercept-only trigonometric GLMM
trig_model <- function(species_name, data) {
  
  dat <- data %>%
    filter(Species == species_name)
  
  mixed_model(
    fixed =
      presence ~
      cos(Suntime) * temp_scaled  +
      sin(Suntime) * temp_scaled  +
      cos(2 * Suntime) * temp_scaled  +
      sin(2 * Suntime) * temp_scaled ,
    random = ~1 | Station,
    data = dat,
    family = binomial()
  )
}

# save each model for marginal plots
af_bear_trig <- trig_model("Black bear", full_grid_temp_af)
af_deer_trig <- trig_model("Mule deer", full_grid_temp_af)
af_hare_trig <- trig_model("Snowshoe hare", full_grid_temp_af)
af_squirrel_trig <- trig_model("Red squirrel", full_grid_temp_af)

# extract coefficients 
extract_results <- function(model, species_name) {
  
  out <- as.data.frame(summary(model)$coef_table)
  out$term <- rownames(out)
  out$species <- species_name
  rownames(out) <- NULL
  
  # Optional: put term first
  out <- out %>%
    dplyr::select(species, term, everything())
  
  out
}

# combine everything
results_clean <- bind_rows(
  extract_results(af_bear_trig, "Black bear"),
  extract_results(af_deer_trig, "Mule deer"),
  extract_results(af_hare_trig, "Snowshoe hare"),
  extract_results(af_squirrel_trig, "Red squirrel")
)

# export
write_xlsx(
  results_clean,
  "./model_results_tabled/results_species_models_af.xlsx"
)

# save models
saveRDS(af_deer_trig, "./data_processed/af_deer_trig.rds")
saveRDS(af_bear_trig, "./data_processed/af_bear_trig.rds")
saveRDS(af_hare_trig, "./data_processed/af_hare_trig.rds")
saveRDS(af_squirrel_trig, "./data_processed/af_squirrel_trig.rds")