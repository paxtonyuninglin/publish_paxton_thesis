# load packages
library(lubridate)
library(ggplot2)
library(vegan)
library(raster)
library(plyr)
library(dplyr)
library(patchwork)

TL_daily_table <- readRDS("PUBLISH!!!/data_processed/TL_daily_table_covariates.rds")

cc_elev_plot <- TL_daily_table %>% 
  distinct(Station, Elevation, canopy_cover_avg) %>% 
  mutate(canopy_cover_avg = canopy_cover_avg*100)

quad_mod <- lm(canopy_cover_avg ~ poly(Elevation, 2, raw = TRUE),
               data = cc_elev_plot)
summary(quad_mod)

mod_summary <- summary(quad_mod)

r2 <- mod_summary$r.squared
p_val <- pf(mod_summary$fstatistic[1],
            mod_summary$fstatistic[2],
            mod_summary$fstatistic[3],
            lower.tail = FALSE)


elev_canopy_tl <- ggplot(cc_elev_plot, aes(x = Elevation, y = canopy_cover_avg)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2, raw = TRUE),
              se = TRUE,
              color = "black",
              linewidth = 1) +
  annotate("text",
           x = Inf, y = Inf,
           label = paste0("R² = ", round(r2, 3),
                          "\np = ", signif(p_val, 3)),
           hjust = 1.1, vjust = 1.5,
           size = 5) +
  labs(
    x = "Elevation (m)",
    y = "Canopy Cover (%)"
  ) +
  theme_minimal(base_size = 14)
elev_canopy_tl

ggsave(file = "./PUBLISH!!!/figures/elev_canopy_tl.jpg", plot = elev_canopy_tl, dpi = 800, units = "mm", width = 250, height = 100)
