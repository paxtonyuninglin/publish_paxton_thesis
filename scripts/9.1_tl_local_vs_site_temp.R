library(dplyr)
library(tidyr)
library(glmmTMB)
library(performance)
library(lubridate)
library(ggplot2)
library(writexl)
library(tibble)

#### Load dataframe
TL_daily_table <- readRDS("./data_processed/TL_daily_table_covariates.rds")

# difference between local and site
TL_daily_table <- TL_daily_table %>% 
  mutate(temp_diff = daily_max_temp - avg_max_temp)

# Scale variables
TL_daily_table <- TL_daily_table %>%
  group_by(Species) %>%
  mutate(
    daily_max_temp_scaled = as.numeric(scale(daily_max_temp)),
    avg_max_temp_scaled   = as.numeric(scale(avg_max_temp)),
    temp_diff_scaled      = as.numeric(scale(temp_diff))
  ) %>%
  ungroup()

# save scaled dataframe
saveRDS(TL_daily_table, "./data_processed/TL_daily_table_scaled_temp.rds")

temp_activity_model <- function(species_name, data = TL_daily_table){
  
  # Filter for chosen species
  species_data <- data %>% 
    filter(Species == species_name)
  
  # Model
  model1 <- glmmTMB(daily_detections ~ avg_max_temp_scaled*temp_diff_scaled  + (1|Station),
                    data = species_data,
                    family = "nbinom2"
  )
  
  #summarize
  return(model1)
}

# assign unique model to each species and summarize 
tl_deer_temp_diff <- temp_activity_model("Mule deer")
tl_bear_temp_diff <- temp_activity_model("Black bear")

# turn results into excel sheet
extract_coef <- function(model, species){
  as.data.frame(summary(model)$coefficients$cond) %>%
    rownames_to_column("Term") %>%
    mutate(Species = species) %>%
    relocate(Species)
}

# combine species results and assign lable
tl_all_results <- bind_rows(
  extract_coef(tl_deer_temp_diff, "Mule deer"),
  extract_coef(tl_bear_temp_diff, "Black bear")
)

# save the excel sheet
write_xlsx(
  tl_all_results,
  "./model_results_tabled/tl_temp_activity_model_results.xlsx"
)

# save models
saveRDS(tl_deer_temp_diff, "./data_processed/tl_deer_temp_diff.rds")
saveRDS(tl_bear_temp_diff, "./data_processed/tl_bear_temp_diff.rds")