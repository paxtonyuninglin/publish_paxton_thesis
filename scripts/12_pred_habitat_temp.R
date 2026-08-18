library(dplyr)
library(tidyr)
library(glmmTMB)
library(performance)
library(lubridate)
library(ggplot2)
library(patchwork)
library(grid)
library(gtable)

# Load models
tl_temp_model <- readRDS("./data_processed/tl_temp_model.rds")
af_temp_model <- readRDS("./data_processed/af_temp_model.rds")

# Load scaled data
TL_daily_table <- readRDS("./data_processed/TL_daily_table_scaled_temp.rds")
AF_daily_table <- readRDS("./data_processed/AF_daily_table_scaled_temp.rds")

# scale canopy and elevation
TL_daily_table_scaled_temp <- TL_daily_table %>% 
  mutate(Elevation_scaled = scale(Elevation),
         canopy_cover_avg_scaled = scale(canopy_cover_avg))
AF_daily_table_scaled_temp <- AF_daily_table %>% 
  mutate(Elevation_scaled = scale(Elevation),
         canopy_cover_avg_scaled = scale(canopy_cover_avg))

# extract unique values of avg canopy cover
TL_canopy_vals <- sort(unique(TL_daily_table_scaled_temp$canopy_cover_avg_scaled))
AF_canopy_vals <- sort(unique(AF_daily_table_scaled_temp$canopy_cover_avg_scaled))

# Get quantiles of scaled elevations
elev_quants_scaled_tl <- quantile(
  TL_daily_table_scaled_temp$Elevation_scaled,
  probs = c(0.25, 0.50, 0.75)
)
elev_quants_scaled_af <- quantile(
  AF_daily_table_scaled_temp$Elevation_scaled,
  probs = c(0.25, 0.50, 0.75)
)

# Get quantiles of raw elevations
elev_quants_raw_tl <- quantile(
  TL_daily_table_scaled_temp$Elevation,
  probs = c(0.25, 0.50, 0.75)
)
elev_quants_raw_af <- quantile(
  AF_daily_table_scaled_temp$Elevation,
  probs = c(0.25, 0.50, 0.75)
)

# label and create data frame
elev_lookup_tl <- data.frame(
  Elevation_scaled = as.numeric(elev_quants_scaled_tl),
  Elevation = as.numeric(elev_quants_raw_tl),
  Elevation_group = c("Low", "Medium", "High")
)
elev_lookup_af <- data.frame(
  Elevation_scaled = as.numeric(elev_quants_scaled_af),
  Elevation = as.numeric(elev_quants_raw_af),
  Elevation_group = c("Low", "Medium", "High")
)

# Prediction grid
prediction_habitat_temp_tl <- expand.grid(
  canopy_cover_avg_scaled = TL_canopy_vals,
  Elevation_scaled = elev_lookup_tl$Elevation_scaled
)
prediction_habitat_temp_af <- expand.grid(
  canopy_cover_avg_scaled = AF_canopy_vals,
  Elevation_scaled = elev_lookup_af$Elevation_scaled
)

# add raw canopy cover
prediction_habitat_temp_tl <- prediction_habitat_temp_tl %>%
  left_join(
    TL_daily_table_scaled_temp %>%
      dplyr::select(
        canopy_cover_avg_scaled,
        canopy_cover_avg
      ) %>%
      distinct(),
    by = "canopy_cover_avg_scaled"
  )
prediction_habitat_temp_af <- prediction_habitat_temp_af %>%
  left_join(
    AF_daily_table_scaled_temp %>%
      dplyr::select(
        canopy_cover_avg_scaled,
        canopy_cover_avg
      ) %>%
      distinct(),
    by = "canopy_cover_avg_scaled"
  )

# add raw elevation
prediction_habitat_temp_tl <- prediction_habitat_temp_tl %>%
  left_join(
    elev_lookup_tl,
    by = "Elevation_scaled"
  )
prediction_habitat_temp_af <- prediction_habitat_temp_af %>%
  left_join(
    elev_lookup_af,
    by = "Elevation_scaled"
  )

#### Create prediction plots
# A reusable prediction function for any species model
predict_from_model <- function(model, pred_df) {
  
  p <- predict(
    model,
    newdata = pred_df,
    type = "response",
    se.fit = TRUE,
    re.form = NA
  )
  
  pred_df %>%
    mutate(
      pred  = p$fit,
      se    = p$se.fit,
      lower = pred - 1.96 * se,
      upper = pred + 1.96 * se
    )
}

# Apply function to each species model
pred_habitat_plot_tl <- predict_from_model(tl_temp_model,  prediction_habitat_temp_tl)
pred_habitat_plot_af <- predict_from_model(af_temp_model,  prediction_habitat_temp_af)

# round temperature plot and convert canopy cover to percentage
pred_habitat_plot_tl <- pred_habitat_plot_tl %>%
  mutate(pred = round(pred, 1))%>% 
  mutate(canopy_cover_avg = canopy_cover_avg*100)

pred_habitat_plot_af <- pred_habitat_plot_af %>%
  mutate(pred = round(pred, 1))%>% 
  mutate(canopy_cover_avg = canopy_cover_avg*100)

#####
# plot!

pred_habitat_plot_tl <- pred_habitat_plot_tl %>%
  mutate(Site = "Tenquille Lake")

pred_habitat_plot_af <- pred_habitat_plot_af %>%
  mutate(Site = "Alex Fraser")


combined_pred <- bind_rows(
  pred_habitat_plot_af,
  pred_habitat_plot_tl
)

combined_plot <- ggplot(combined_pred,
       aes(x = canopy_cover_avg,
           y = pred,
           colour = Elevation,
           fill = Elevation,
           group = interaction(Site, Elevation))) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.08,
    colour = NA
  )+
  facet_wrap(~Site) +
  scale_colour_viridis_c(
    name = "Elevation (m)",
    option = "C",
    begin = 0.13,
    end = 0.95,
    breaks = c(1000, 1250, 1500)
  ) +
  scale_fill_viridis_c(
    option = "C",
    begin = 0.13,
    end = 0.95,
    guide = "none",   # hide duplicate legend
    breaks = c(1000, 1250, 1500)
  ) +
  labs(
    x = "Canopy cover (%)",
    y = "Max temp (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(colour="black",
                                fill=NA,
                                linewidth=1),
    plot.margin = margin(0, 0, 0, 0, unit = "mm"),
    
    # Axis text (tick labels)
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    
    # Axis titles
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    
    strip.text = element_blank(),
    strip.background = element_blank()
  )

combined_plot

# force each panel to be the same size
combined_plot <- combined_plot +
  force_panelsizes(rows = unit(6, "cm"), cols = unit(9, "cm"))

# save the sized figure
ggsave(file = "./figures/habitat_temp_model.jpg", plot = combined_plot, dpi = 800, units = "mm", width = 290, height = 110)
