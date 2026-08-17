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
library(ggpubr)
library(patchwork)

# load all datasets
all_preds_tl <- readRDS("./PUBLISH!!!/data_processed/all_preds_tl.rds")
all_preds_af <- readRDS("./PUBLISH!!!/data_processed/all_preds_af.rds")
temp_pred <- readRDS("./PUBLISH!!!/data_processed/temp_pred.rds")

# re-order species
all_preds_tl <- all_preds_tl %>%
  mutate(
    Species = factor(
      Species,
      levels = c(
        "Mule deer",
        "Black bear",
        "Snowshoe hare"
      )
    )
  )

all_preds_af <- all_preds_af %>%
  mutate(
    Species = factor(
      Species,
      levels = c(
        "Mule deer",
        "Black bear",
        "Snowshoe hare",
        "Red squirrel"
      )
    )
  )


# factor temperature and round to nearest decimal
all_preds_tl <- all_preds_tl %>%
  mutate(
    temp = factor(
      sprintf("%.1f", temp),
      levels = sprintf("%.1f", sort(unique(temp), decreasing = TRUE))
    )
  )

all_preds_af <- all_preds_af %>%
  mutate(
    temp = factor(
      sprintf("%.1f", temp),
      levels = sprintf("%.1f", sort(unique(temp), decreasing = TRUE))
    )
  )

# add colours
temp_levels <- levels(all_preds_tl$temp)
plot_cols <- setNames(
  c("red", "purple", "blue"),
  temp_levels
)

# plot
tl_trig <- ggplot(
  all_preds_tl, aes( x = Suntime,
                     y = pred,
                     colour = temp,
                     group = temp)) +
  geom_line(size = 1.2) +
  
  geom_ribbon(
    aes(
      ymin = low,
      ymax = upp,
      fill = temp,
      group = temp
    ),
    alpha = 0.08,
    colour = NA
  ) +

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
  )  +
  labs(
    x = "",
    y = "Predicted activity\npattern (probability)"
  ) +
  scale_colour_manual(
    values = plot_cols,
    name = "TL max daily temp (°C)"
  ) +
  scale_fill_manual(
    values = plot_cols,
    name = "TL max daily temp (°C)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 1
    ),
    
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    
    legend.position = "top",
    legend.direction = "horizontal"
  ) + 
  scale_x_continuous(
    limits = c(0, 2*pi),
    breaks = c(0, pi/2, pi, 3*pi/2, 2*pi),
    labels = c("midnight", "sunrise", "noon", "sunset", "midnight")
  )

tl_trig

tl_trig <- tl_trig +
  force_panelsizes(rows = unit(6, "cm"), cols = unit(9, "cm"))

# plot temperature with time of day to be visualized with tenquille lake
diel_temp <- ggplot(temp_pred, aes(x = suntime, y = fit, colour = Site, fill = Site)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2,
    colour = NA
  ) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(
    values = c("Tenquille" = "purple",
               "Alex Fraser" = "orange")
  ) +
  scale_fill_manual(
    values = c("Tenquille" = "purple",
               "Alex Fraser" = "orange")
  ) +
  scale_x_continuous(
    limits = c(0, 2*pi),
    breaks = c(0, pi/2, pi, 3*pi/2, 2*pi),
    labels = c("midnight", "sunrise", "noon", "sunset", "midnight")
  ) +
  labs(
    x = "Time of day (suntime hour)",
    y = "Temperature (°C)",
    colour = "Study area",
    fill = "Study area"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(
      colour = "black", fill = NA, linewidth = 1,
    ),
    
    # Axis text (tick labels)
    axis.text.x  = element_text(size = 13),
    axis.text.y  = element_text(size = 13),
    
    # Axis titles
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line()
  )

diel_temp

diel_temp <- diel_temp +
  force_panelsizes(rows = unit(6, "cm"), cols = unit(9, "cm"))

# combine tenquille lake plot
combined_tl <- tl_trig / diel_temp +
  plot_layout(
    ncol = 1,
    guides = "collect"
  ) &
  theme(
    legend.position = "top",
    legend.box = "vertical"
  )

combined_tl

# add colours
temp_levels <- levels(all_preds_af$temp)
plot_cols <- setNames(
  c("red", "purple", "blue"),
  temp_levels
)
af_trig <- ggplot(
  all_preds_af, aes( x = Suntime,
                     y = pred,
                     colour = temp,
                     group = temp)) +
  geom_line(size = 1.2) +
  geom_ribbon(
    aes(
      ymin = low,
      ymax = upp,
      fill = temp,
      group = temp
    ),
    alpha = 0.08,
    colour = NA
  ) +
  facet_wrap(
    ~ Species, ncol = 1, scales = "free_y",
    axes = "all_x",         # draw x-axis line/ticks on every panel
    axis.labels = "all_x",  # draw x-axis tick labels on every panel
    labeller = labeller(
               Species = c(
                 "Mule deer"    = "",
                 "Black bear"   = "",
                 "Snowshoe hare" = "",
                 "Red squirrel" = "")))  +
  labs(
    x = "Time of day (suntime hour)",
    y = "Predicted activity\npattern (probability)"
  ) +
  scale_colour_manual(
    values = plot_cols,
    name = "AF max daily temp (°C)"
  ) +
  scale_fill_manual(
    values = plot_cols,
    name = "AF max daily temp (°C)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(
      colour = "black", fill = NA, linewidth = 1,
    ),

    # Axis text (tick labels)
    axis.text.x  = element_text(size = 13),
    axis.text.y  = element_text(size = 13),
    
    # Axis titles
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),

    legend.position = "top",
    legend.direction = "horizontal"
  ) + 
  scale_x_continuous(
    limits = c(0, 2*pi),
    breaks = c(0, pi/2, pi, 3*pi/2, 2*pi),
    labels = c("midnight", "sunrise", "noon", "sunset", "midnight")
  )

af_trig

af_trig <- af_trig +
  force_panelsizes(rows = unit(6, "cm"), cols = unit(9, "cm"))

multi_panel <- ggarrange(
  af_trig,
  combined_tl,
  ncol = 2,
  nrow = 1,
  common.legend = FALSE,
  align = "hv"
)

multi_panel

# force each panel to be the same size
# multi_panel <- multi_panel +
#   force_panelsizes(rows = unit(14, "cm"), cols = unit(20, "cm"))

#save the figure
tiff("PUBLISH!!!/figures/multi_panel_trig.tiff",
     width = 55, height = 38, units = "cm", res = 900,
     compression = "lzw")

multi_panel

dev.off()

