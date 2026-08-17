library(dplyr)
library(tidyr)
library(glmmTMB)
library(performance)
library(lubridate)
library(ggplot2)
library(writexl)
library(tibble)

#### Load dataframe
AF_daily_table <- readRDS("./PUBLISH!!!/data_processed/AF_daily_table_covariates.rds")

# scale canopy and elevation
AF_daily_table_scaled <- AF_daily_table %>% 
  mutate(Elevation_scaled = scale(Elevation),
         canopy_cover_avg_scaled = scale(canopy_cover_avg)
  )

#####
#run model
af_temp_model <- glmmTMB(daily_max_temp ~ Elevation_scaled*canopy_cover_avg_scaled  + (1|Station),
                             data = AF_daily_table_scaled,
                             family = gaussian()
)

summary(af_temp_model)

# turn results into excel sheet
extract_coef <- function(model){
  as.data.frame(summary(model)$coefficients$cond) %>%
    rownames_to_column("Term")
}

af_temp_model_co <- extract_coef(af_temp_model)

# save the model results
write_xlsx(af_temp_model_co, "./PUBLISH!!!/data_processed/af_temp_model.xlsx")

# save models
saveRDS(af_temp_model, "./PUBLISH!!!/data_processed/af_temp_model.rds")
