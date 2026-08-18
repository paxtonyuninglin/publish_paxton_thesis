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
tl_deer_temp_diff <- readRDS("./data_processed/tl_deer_temp_diff.rds")
tl_bear_temp_diff <- readRDS("./data_processed/tl_bear_temp_diff.rds")

# Load scaled data
TL_daily_table_scaled <- readRDS("./data_processed/TL_daily_table_scaled_temp.rds")

#####
# extract temperature values
temp_vals_day <- seq(
  min(TL_daily_table_scaled$avg_max_temp_scaled, na.rm = TRUE),
  max(TL_daily_table_scaled$avg_max_temp_scaled, na.rm = TRUE),
  length.out = 10
)

raw_temp_vals_day <- seq(
  min(TL_daily_table_scaled$avg_max_temp, na.rm = TRUE),
  max(TL_daily_table_scaled$avg_max_temp, na.rm = TRUE),
  length.out = 10
)

combined_temp_day <- data.frame(
  Temp_scaled = sort(temp_vals_day),
  Temp = sort(raw_temp_vals_day)
)

# compute quantiles
temp_quants_day <- quantile(TL_daily_table_scaled$temp_diff_scaled,
                        probs = c(0.25, 0.50, 0.75),
                        na.rm = TRUE)
temp_diff_quants_raw_day <- quantile(TL_daily_table_scaled$temp_diff,
                                 probs = c(0.25, 0.50, 0.75),
                                 na.rm = TRUE)
# raw quantiles into tibble
temp_quants_day_df <- tibble(
  temp_diff_scaled = as.numeric(temp_quants_day),
  q_label = names(temp_quants_day)
)

temp_quants_day_raw_df <- tibble(
  q_label = names(temp_diff_quants_raw_day),
  temp_diff_raw = as.numeric(temp_diff_quants_raw_day)
)


# build the grid
prediction_temp_diff_tl_day <- expand.grid(
  temp_diff_scaled = as.numeric(temp_quants_day),
  avg_max_temp_scaled = temp_vals_day
)

# join raw quantiles
temp_quants_day_joined <- temp_quants_day_df %>%
  left_join(temp_quants_day_raw_df, by = "q_label")

prediction_temp_diff_tl_day <- prediction_temp_diff_tl_day %>%
  left_join(temp_quants_day_joined, by = "temp_diff_scaled")


# convert back to raw temperatures
temp_mean_day <- mean(TL_daily_table_scaled$avg_max_temp, na.rm = TRUE)
temp_sd_day   <- sd(TL_daily_table_scaled$avg_max_temp, na.rm = TRUE)

prediction_temp_diff_tl_day <- prediction_temp_diff_tl_day %>%
  mutate(
    avg_max_temp =
      avg_max_temp_scaled * temp_sd_day + temp_mean_day
  )


# round the raw temperature 
prediction_temp_diff_tl_day <- prediction_temp_diff_tl_day %>%
  mutate(
    avg_max_temp_rounded = sprintf("%.1f", avg_max_temp),  # always 1 decimal place
    avg_max_temp_rounded = factor(avg_max_temp_rounded),
    avg_max_temp_rounded = factor(avg_max_temp_rounded, levels = sort(unique(avg_max_temp_rounded), decreasing = TRUE))
  )

# round the raw temperature diff
prediction_temp_diff_tl_day <- prediction_temp_diff_tl_day %>%
  mutate(
    temp_diff_round_num = round(temp_diff_raw, 1),
    temp_diff_rounded = factor(
      temp_diff_round_num,
      levels = sort(unique(temp_diff_round_num), decreasing = TRUE)
    )
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
deer_pred     <- predict_from_model(tl_deer_temp_diff,     prediction_temp_diff_tl_day)
bear_pred     <- predict_from_model(tl_bear_temp_diff,     prediction_temp_diff_tl_day)

# Add species labels
deer_pred$Species <- "Mule deer"
bear_pred$Species <- "Black bear"

# Combine into one dataframe
all_pred <- bind_rows(
  bear_pred,
  deer_pred
)

# re-order species
all_pred <- all_pred %>%
  mutate(
    Species = factor(
      Species,
      levels = c(
        "Mule deer",
        "Black bear"
      )
    )
  )

# A reusable prediction function for any species model
temp_cols_day <- c(
  "red",        # highest temp
  "purple",     # middle temp
  "blue"        # lowest temp
)

levs_day <- levels(prediction_temp_diff_tl_day$temp_diff_rounded)

temp_cols_day <- setNames(
  c("red", "purple", "blue")[seq_along(levs_day)],
  levs_day
)

tl_temp_pred <-ggplot(all_pred,aes(x = avg_max_temp,y = pred, color = temp_diff_rounded,
                                          group = temp_diff_rounded
)
) +
  geom_line(size = 1.2) +
  facet_wrap(
    ~ Species, ncol = 1, scales = "free_y",
    axes = "all_x",         # draw x-axis line/ticks on every panel
    axis.labels = "all_x",  # draw x-axis tick labels on every panel
    labeller = labeller(
      Species = c(
        "Mule deer"     = "",
        "Black bear"    = "",
        "Snowshoe hare" = ""
      )
    )
  ) +
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper,
      fill = temp_diff_rounded,
      group = temp_diff_rounded
    ),
    alpha = 0.1,
    colour = NA
  ) +
  labs(
    x = "Study area daily max temp (°C)",
    y = "Predicted daily detections"
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
  scale_colour_manual(
    values = temp_cols_day,
    name = "TL max temp Δ between\ncamera vs study area (°C)"
  ) +
  scale_fill_manual(
    values = temp_cols_day,
    name = "TL max temp Δ between\ncamera vs study area (°C)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_blank(),
    
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    
    plot.margin = margin(0, 0, 0, 0, unit = "mm"),
    
    # Axis text (tick labels)
    axis.text.x  = element_text(size = 13),
    axis.text.y  = element_text(size = 13),
    
    # Axis titles
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    

    legend.position = "top",
    legend.direction = "horizontal",
  )

tl_temp_pred

#save the plot to be combined with alex fraser plot
saveRDS(tl_temp_pred, "./figures/tl_temp_pred.rds")
