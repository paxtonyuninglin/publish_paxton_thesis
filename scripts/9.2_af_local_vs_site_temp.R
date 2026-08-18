library(dplyr)
library(tidyr)
library(glmmTMB)
library(performance)
library(lubridate)
library(ggplot2)
library(writexl)
library(tibble)

#### Load dataframe
AF_daily_table <- readRDS("./data_processed/AF_daily_table_covariates.rds")

# difference between local and site
AF_daily_table <- AF_daily_table %>% 
  mutate(temp_diff = daily_max_temp - avg_max_temp)

# Scale variables
AF_daily_table <- AF_daily_table %>%
  group_by(Species) %>%
  mutate(
    daily_max_temp_scaled = as.numeric(scale(daily_max_temp)),
    avg_max_temp_scaled   = as.numeric(scale(avg_max_temp)),
    temp_diff_scaled      = as.numeric(scale(temp_diff))
  ) %>%
  ungroup()

# save scaled dataframe
saveRDS(AF_daily_table, "./data_processed/AF_daily_table_scaled_temp.rds")

temp_activity_model <- function(species_name, data = AF_daily_table){
  
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
af_deer_temp_diff <- temp_activity_model("Mule deer")
af_bear_temp_diff <- temp_activity_model("Black bear")
af_squirrel_temp_diff <- temp_activity_model("Red squirrel")

# turn results into excel sheet
extract_coef <- function(model, species){
  as.data.frame(summary(model)$coefficients$cond) %>%
    rownames_to_column("Term") %>%
    mutate(Species = species) %>%
    relocate(Species)
}

# combine species results and assign lable
af_all_results <- bind_rows(
  extract_coef(af_deer_temp_diff, "Mule deer"),
  extract_coef(af_bear_temp_diff, "Black bear"),
  extract_coef(af_squirrel_temp_diff, "Red squirrel")
)

# save the excel sheet
write_xlsx(
  af_all_results,
  "./model_results_tabled/af_temp_activity_model_results.xlsx"
)

# save models
saveRDS(af_deer_temp_diff, "./data_processed/af_deer_temp_diff.rds")
saveRDS(af_bear_temp_diff, "./data_processed/af_bear_temp_diff.rds")
saveRDS(af_squirrel_temp_diff, "./data_processed/af_squirrel_temp_diff.rds")