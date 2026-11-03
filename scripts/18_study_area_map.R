#######################################################################
###                       Map of study area                         ###
#######################################################################

library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

aeqd <- "+proj=aeqd +lat_0=55 +lon_0=-125"

# Finger-Tatuk Provincial Park: 53°30'34"N, 124°13'29"W
site_points <- st_as_sf(
  data.frame(
    Site = "Finger-Tatuk\nProvincial Park",
    lon  = -124.22472,
    lat  = 53.50944
  ),
  coords = c("lon", "lat"),
  crs = 4326
)

# Provinces and states of the world
provinces <- ne_download(
  scale = 50,
  type = "admin_1_states_provinces",
  returnclass = "sf"
)

bc <- provinces |>
  filter(iso_a2 == "CA", name == "British Columbia")

neighbours <- provinces |>
  filter(
    (iso_a2 == "CA" & name %in% c("Yukon", "Alberta", "Northwest Territories")) |
      (iso_a2 == "US" & name %in% c("Alaska", "Washington", "Idaho", "Montana"))
  )

# Crop to BC's extent (in the map projection) plus a margin
bc_box <- st_bbox(st_transform(bc, aeqd))
pad <- 100000   # 100 km

study_map <- ggplot() +
  geom_sf(data = neighbours, fill = "grey97", color = "grey70", linewidth = 0.3) +
  geom_sf(data = bc, fill = "grey90", color = "grey40", linewidth = 0.4) +
  geom_sf(data = site_points,
          aes(color = Site, shape = Site),
          size = 7) +
  scale_color_manual(values = c("Finger-Tatuk\nProvincial Park" = "darkorange")) +
  scale_shape_manual(values = c("Finger-Tatuk\nProvincial Park" = 17)) +
  
  # Two-line label to the left of the point
  geom_sf_text(data = site_points,
               aes(label = Site),
               hjust = 0.75,        # centred over the point
               vjust = -0.6,       # text sits above the point
               size = 7,
               lineheight = 0.9) +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.4,
    height = unit(0.6, "cm"),
    text_cex = 1.6
  ) +
  coord_sf(
    crs = aeqd,
    xlim = c(bc_box["xmin"] - pad, bc_box["xmax"] + pad),
    ylim = c(bc_box["ymin"] - pad, bc_box["ymax"] + pad),
    expand = FALSE
  ) +
  theme(
    panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed",
                                    linewidth = 0.5),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17),
    panel.border = element_rect(fill = NA),
    legend.position = "none",
    plot.margin = unit(c(0, 5, 0, 0), "mm")
  )

study_map

# Export as a high-resolution TIFF
dir.create("figures", showWarnings = FALSE)
tiff("figures/study_map_globe.tiff",
     width = 2500, height = 2500, res = 300,
     compression = "lzw")
print(study_map)
dev.off()
