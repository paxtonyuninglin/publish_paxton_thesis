library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(patchwork)

# load datasets
AF_rectable1 <- read.csv("./data_raw/AF_table.csv")
TL_Oct_rectable1 <- read.csv("./data_raw/TL_oct_table.csv")
TL_Sept_rectable1 <- read.csv("./data_raw/TL_sept_table.csv")

#Join all the record tables together to analyze min delta between detections
all_sites_rectable1 <- bind_rows(
  TL_Oct_rectable1,
  TL_Sept_rectable1,
  AF_rectable1)

#Filter so only minutes below 60 are considered
all_sites_rectable1_under60 <- all_sites_rectable1 %>%
  filter(delta.time.mins < 60 & !is.na(delta.time.mins))

##### 
#Make histogram of delta between detection
#Snowshoe hare
snowshoe_hare_data <- all_sites_rectable1_under60 %>%
  filter(Species == "Snowshoe hare")

#Plot
p1 <- ggplot(snowshoe_hare_data, aes(x = delta.time.mins)) +
  geom_histogram(binwidth = 1, color = "black", fill = "tan4") +
  geom_vline(xintercept = 5, color = "red", linewidth = 1) +
  coord_cartesian(xlim = c(0, 60)) +
  labs(x = "",
       y = "Frequency",
       title = "") +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(
      colour = "black", fill = NA, linewidth = 1
    ),
    axis.text.x  = element_text(),
    axis.ticks.x = element_line()
  )
p1

#Red squirrel
red_squirrel_data <- all_sites_rectable1_under60 %>%
  filter(Species == "Red squirrel")

#Plot
p2 <- ggplot(red_squirrel_data, aes(x = delta.time.mins)) +
  geom_histogram(binwidth = 1, color = "black", fill = "tan4") +
  geom_vline(xintercept = 5, color = "red", linewidth = 1) +
  labs(x = "",
       y = "",
       title = "") +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(
      colour = "black", fill = NA, linewidth = 1
    ),
    axis.text.x  = element_text(),
    axis.ticks.x = element_line()
  )
p2

#Black bear
black_bear_data <- all_sites_rectable1_under60 %>%
  filter(Species == "Black bear")

#Plot
p3 <- ggplot(black_bear_data, aes(x = delta.time.mins)) +
  geom_histogram(binwidth = 1, color = "black", fill = "tan4") +
  geom_vline(xintercept = 5, color = "red", linewidth = 1) +
  coord_cartesian(xlim = c(0, 60)) +
  labs(x = "Time Between Detections (min)",
       y = "",
       title = "") +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(
      colour = "black", fill = NA, linewidth = 1
    ),
    axis.text.x  = element_text(),
    axis.ticks.x = element_line()
  )
p3

#Mule deer
mule_deer_data <- all_sites_rectable1_under60 %>%
  filter(Species == "Mule deer")

#Plot
p4 <- ggplot(mule_deer_data, aes(x = delta.time.mins)) +
  geom_histogram(binwidth = 1, color = "black", fill = "tan4") +
  geom_vline(xintercept = 5, color = "red", linewidth = 1) +
  labs(x = "",
       y = "",
       title = "") +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(
      colour = "black", fill = NA, linewidth = 1
    ),
    axis.text.x  = element_text(),
    axis.ticks.x = element_line()
  )
p4

#Combine all the plots
hist <- (p1 | p2)/(p3 | p4) +
  plot_layout(guides = "collect")

hist

# save figure and plot
ggsave(file = "./figures/hist1.jpg", plot = hist, dpi = 800, units = "mm", width = 250, height = 170)
