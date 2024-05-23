#Plotting via days with N2, cest-2.1, and glo-1

library(tidyverse)
library(ggplot2)
library(ggtext)
theme_set(theme_classic())


files <- fs::dir_ls(recurse = TRUE, glob = "data/allsosdata.csv")
merged_data <- files %>% purrr::map_df(., readr::read_csv, .id = "filename")

plotColors <- source(file = 'parameters/plotColors.R')


filter_date <- c("2023-05-24", "2023-05-08", "2023-05-09", "2023-06-23", "2023-06-19")

# Filter Data to Date
filtered_data <- merged_data %>%
  filter(Date %in% filter_date, Genotype %in% c("N2", "cest-2.1", "glo-1"))

# Reorder Genotype 
filtered_data$Genotype <- factor(filtered_data$Genotype, levels = c("N2", "cest-2.1", "glo-1")) 


ggplot(filtered_data, aes(x = Genotype, y = Response.time)) +
  stat_summary(geom = "bar", aes(fill = Genotype), width= 0.5) +
  labs(fill = "Genotype") +
  ggbeeswarm::geom_quasirandom(alpha = 0.5, width=0.2) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
  #labs(title = "OA SOS with N2, cest-2.1, and tbh-1 for 30% octanol avoidance") +
  scale_y_continuous(expand = c(0, 0)) +
  geom_text(aes(x = 1, y = 20, label = "Stretch it"), vjust = -1) +
  #scale_fill_manual(values = c("#999999", "#E69F00", "#D55E00")) +
  scale_fill_manual(values = genotype_colors) +
  scale_color_manual(values = genotype_colors) +
  scale_alpha_manual(values = c(0.5, 1)) +
  labs(y = "Time(sec)")
