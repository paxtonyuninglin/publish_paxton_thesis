#######################################################################
###                       Map of study area                         ###
###                         Tenquille Lake                          ###
#######################################################################

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

#### Load camera points
cams  <- read.csv("./PUBLISH!!!/data_raw/cam_data.csv")
cams <- cams %>% 
  mutate(Longitude = Longitude * -1)

cams_sf <- cams %>%
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = 4326)   # WGS84

# One point per study site (centroid of all cameras in that site)
w <- cams_sf %>%
  group_by(Site) %>%
  summarise(geometry = st_centroid(st_union(geometry)))

af_pt <- site_points %>% filter(Site == "Alex Fraser")
tl_pt <- site_points %>% filter(Site == "Tenquille Lake")

# Zoom window (50 km buffer — adjust as needed)
af_buf <- st_buffer(af_pt, dist = 50000)
tl_buf <- st_buffer(tl_pt, dist = 50000)



# Download all admin-1 units (states/provinces) for the world
provinces <- ne_download(
  scale = 50,
  type = "admin_1_states_provinces",
  returnclass = "sf"
)

# Filter to Canada, then BC
bc <- provinces |>
  filter(iso_a2 == "CA", name == "British Columbia")

# plot
study_map <- ggplot() +
  geom_sf(data = bc, fill = "grey90", color = "grey40", linewidth = 0.4) +
  geom_sf(data = site_points,
          aes(color = Site, shape = Site),
          size = 7) +
  scale_color_manual(values = c("Alex Fraser" = "darkorange",
                                "Tenquille Lake" = "purple")) +
  scale_shape_manual(values = c("Alex Fraser" = 17,
                                "Tenquille Lake" = 19)) +
  
  # Site labels to the left of points 
  geom_sf_text(data = site_points, 
               aes(label = Site),
               vjust = 0.36,
               hjust = 1.15, 
               size = 7 ) +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.4,      
    height = unit(0.6, "cm"),  
    text_cex = 1.6          
  ) +
  theme(panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17),
        panel.border = element_rect(fill = NA),
        legend.position = "none",
        plot.margin = unit(c(0,5,0,0), "mm"))

study_map

study_map_globe <- study_map +
  coord_sf(
    crs = "+proj=aeqd +lat_0=55 +lon_0=-125",
    expand = FALSE
  )

study_map_globe

tiff("PUBLISH!!!/figures/study_map_globe.tiff",
     width = 2500, height = 2500, res = 300,
     compression = "lzw")

study_map_globe

dev.off()




