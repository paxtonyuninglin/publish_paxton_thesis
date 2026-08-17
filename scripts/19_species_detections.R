library(vroom) #faster loading of large datasets
library(dplyr) #dplyr is a set of r functions for more streamlined data manipulation and management
library(ggplot2) #a more powerful function for data visualization
library(camtrapR) #set of functions for managing camera trap data, from raw images to detection tables for occupancy analysis
library(secr) #
library(lubridate) #functions for manipulating date-time data
library(unmarked) #statistical models of occupancy and abundance for "unmarked" animals
library(overlap)
library(stringr)
library(patchwork)
library(tidyr)

# read detection data
AF_rectable1 <- read.csv("./PUBLISH!!!/data_raw/AF_table.csv")
TL_Oct_rectable1 <- read.csv("./PUBLISH!!!/data_raw/TL_oct_table.csv")
TL_Sept_rectable1 <- read.csv("./PUBLISH!!!/data_raw/TL_sept_table.csv")

# join Tenquille lake october and september
TL_rectable1 <- bind_rows(
  TL_Oct_rectable1,
  TL_Sept_rectable1)

# bind detection tables to just summer
AF_rectable1 <- AF_rectable1 %>% 
  filter(Date <= as.Date("2025-09-22"))

TL_rectable1 <- TL_rectable1 %>% 
  filter(Date <= as.Date("2025-09-22"))

# filter out non-animal detections
non_animals <- c("Nothing", "People", "Unknown", "unknown", "Domestic dog")

AF_animals <- AF_rectable1 %>%
  filter(!Species %in% non_animals)

TL_animals <- TL_rectable1 %>%
  filter(!Species %in% non_animals)

# create count
species_counts_af <- AF_animals %>%
  count(Species, name = "Detections") %>%
  arrange(Detections)

species_counts_tl <- TL_animals %>%
  count(Species, name = "Detections") %>%
  arrange(Detections)

# rename common names as scientific 
sci_names <- tibble::tribble(
  ~Species, ~Scientific,
  "Snowshoe hare", "Lepus americanus",
  "Red squirrel", "Tamiasciurus hudsonicus",
  "Mule deer", "Odocoileus hemionus",
  "Black bear", "Ursus americanus",
  "Flying squirrel", "Glaucomys sabrinus",
  "Red fox", "Vulpes vulpes",
  "Wolf", "Canis lupus",
  "Coyote",  "Canis latrans",
  "Moose", "Alces alces",
  "Canada Lynx", "Lynx canadensis",
  "Striped Skunk", "Mephitis mephitis",
  "Marten", "Martes americana",
  "Rodent - other", "Rodentia spp.",
  "Douglas squirrel", "Tamiasciurus douglasii",
  "White-tailed deer", "Odocoileus virginianus",
  "Mouse",  "Peromyscus spp.",
  "Skunk", "Mephitidae spp.",
  "Short-tailed weasel", "Mustela erminea",
  "Cougar", "Puma concolor",
  "Chipmunk", "Tamias spp.",
  "Canid",  "Canidae spp.",
  "Bobcat", "Lynx rufus",
  "Bird", "Aves spp.",
  "Cattle", "Bos taurus",
  "Grizzly bear", "Ursus arctos",
  "Bear", "Ursus spp.",
  "Elk", "Cervus canadensis"
)

species_counts_af <- species_counts_af %>%
  left_join(sci_names, by = "Species") %>%
  mutate(label = paste0(Species, " (", Scientific, ")"))

species_counts_tl <- species_counts_tl %>%
  left_join(sci_names, by = "Species") %>%
  mutate(label = paste0(Species, " (", Scientific, ")"))

# plot
af_detection <- ggplot(species_counts_af, aes(x = Detections, y = reorder(label, Detections))) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Number of Detections",
    y = "Species",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.text.y = element_text(margin = margin(r = 15)))
af_detection

tl_detection <- ggplot(species_counts_tl, aes(x = Detections, y = reorder(label, Detections))) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Number of Detections",
    y = "Species",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.text.y = element_text(margin = margin(r = 15)))

tl_detection

combined_detections <- (tl_detection | af_detection) +
  plot_layout(guides = "collect")  +
  plot_annotation(tag_levels = "A")

combined_detections

# save
ggsave(file = "./PUBLISH!!!/figures/combined_detections.jpg", plot = combined_detections, dpi = 800, units = "mm", width = 500, height = 150)