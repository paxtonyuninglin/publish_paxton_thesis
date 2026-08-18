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

# scale canopy and elevation
TL_daily_table_scaled <- TL_daily_table %>% 
  mutate(Elevation_scaled = scale(Elevation),
         canopy_cover_avg_scaled = scale(canopy_cover_avg)
         )

#####
#run model
tl_temp_model <- glmmTMB(daily_max_temp ~ Elevation_scaled*canopy_cover_avg_scaled  + (1|Station),
                  data = TL_daily_table_scaled,
                  family = gaussian()
                  )

summary(tl_temp_model)

# turn results into excel sheet
extract_coef <- function(model){
  as.data.frame(summary(model)$coefficients$cond) %>%
    rownames_to_column("Term")
}

tl_temp_model_co <- extract_coef(tl_temp_model)
             
# save the excel sheet model results 
write_xlsx(tl_temp_model_co, "./model_results_tabled/tl_temp_model.xlsx")

# save models
saveRDS(tl_temp_model, "./data_processed/tl_temp_model.rds")
