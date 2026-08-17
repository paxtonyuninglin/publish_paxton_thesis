#### Load necessary packages
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(osmextract)
library(bcdata)
library(leaflet)
library(viridis)
library(terra)

#### park boundaries
tenquille <- st_read("./PUBLISH!!!/data_raw/tenquille-lake.geojson", quiet = TRUE)
gavin <- st_read("./PUBLISH!!!/data_raw/gavin-lake.geojson", quiet = TRUE)

tenquille$Site <- "Tenquille Lake"
gavin$Site     <- "Gavin Lake"

#### Load camera points
cams  <- read.csv("./PUBLISH!!!/data_raw/cam_data.csv")
cams <- cams %>% 
  mutate(Longitude = Longitude * -1) %>% 
  rename("Elevation (m)" = Elevation..m.)

cams_sf <- cams %>%
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = 4326)   # WGS84


cams_bc      <- st_transform(cams_sf, 3005)
tenquille_bc <- st_transform(tenquille, 3005)
gavin_bc     <- st_transform(gavin, 3005)

cams_tenquille <- st_intersection(cams_bc, tenquille_bc)
cams_gavin     <- st_intersection(cams_bc, gavin_bc)

# Load georeferenced TIFF
bg <- rast("./PUBLISH!!!/data_raw/tenquille_lake_map_simple.tif")


bg_df <- as.data.frame(bg, xy = TRUE)
colnames(bg_df) <- c("x", "y", "red", "green", "blue", "alpha")

cams_tenquille_aligned <- st_transform(cams_tenquille, crs(bg))
tenquille_bc_aligned   <- st_transform(tenquille_bc, crs(bg))

tl <- ggplot() +
  geom_raster(
    data = bg_df,
    aes(
      x = x,
      y = y,
      fill = rgb(red, green, blue, maxColorValue = 255)
    )
  ) +
  scale_fill_identity() +

  # Grey outline
  geom_sf(
    data = cams_tenquille_aligned,
    color = "white",
    shape = 19,
    size = 3.1
  ) +
  
  # Colored triangle
  geom_sf(
    data = cams_tenquille_aligned,
    aes(color = Elevation..m.),
    shape = 19,
    size = 2.4
  ) +
  
  scale_color_gradient(low = "navyblue", high = "plum2") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.position = "right")

tiff("PUBLISH!!!/figures/tl.tiff",
     width = 3300, height = 1980, res = 900,
     compression = "lzw")

tl

dev.off()

# Load georeferenced TIFF
bg <- rast("./PUBLISH!!!/data_raw/gavin_lake_map_simple.tif")


bg_df <- as.data.frame(bg, xy = TRUE)
colnames(bg_df) <- c("x", "y", "red", "green", "blue", "alpha")

cams_gavin_aligned <- st_transform(cams_gavin, crs(bg))
gavin_bc_aligned   <- st_transform(gavin_bc, crs(bg))

af <- ggplot() +
  geom_raster(
    data = bg_df,
    aes(
      x = x,
      y = y,
      fill = rgb(red, green, blue, maxColorValue = 255)
    )
  ) +
  scale_fill_identity() +
  geom_sf(data = gavin_bc_aligned, fill = NA, color = "black", size = 1) +
  
  # Draw points
  geom_sf(
    data = cams_gavin_aligned,
    color = "white",
    shape = 17,
    size = 3.1
  ) +
  
  # Coloured triangle
  geom_sf(
    data = cams_gavin_aligned,
    aes(color = Elevation..m.),
    shape = 17,
    size = 2.3
  ) +
  
  # Legend
  scale_color_gradient(
    low = "red4",
    high = "orange1",
    breaks = c(800, 1000, 1200),
    name = "Elevation (m)"
  ) +
  
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.position = "right")

tiff("PUBLISH!!!/figures/af.tiff",
     width = 4500, height = 2700, res = 900,
     compression = "lzw")

af

dev.off()

