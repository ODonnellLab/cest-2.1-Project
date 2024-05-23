#Plotting OA SOS with N2, cest-2.1, and tbh-1

#4mM Figure


library(tidyverse)
library(ggplot2)
library(ggtext)
library(dplyr)

theme_set(theme_classic())
files <- fs::dir_ls(recurse = TRUE, glob = "data/oaincubationsos*.csv")
merged_data <- files %>% purrr::map_df(., readr::read_csv, .id = "filename")


plotColors <- source(file = 'parameters/plotColors.R')

# Filter Data to Date
filter_date <- c("2024-01-21", "2024-01-30", "2024-03-02")
filtered_data <- merged_data %>%
  filter(Date %in% filter_date, Condition %in% c("4mMOA", "control"))

# Specific Genotype Order
filtered_data$Genotype <- factor(filtered_data$Genotype, levels = c("N2", "cest-2.1", "tbh-1"))

ggplot(filtered_data, aes(x = Condition, y = Response.time)) +
  stat_summary(geom = "bar", aes(fill = Genotype, alpha = Condition), fun = "mean") +
  labs(fill = "Genotype") +
  ggbeeswarm::geom_quasirandom(alpha = 0.5, width=0.2) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
  #labs(title = "OA SOS with N2, cest-2.1, and tbh-1 for 30% octanol avoidance") +
  facet_grid(~Genotype) +
  scale_x_discrete(labels = c( "4mMOA", "control")) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_text(aes(x = 1, y = 20, label = "Stretch it"), vjust = -1) +
  #scale_fill_manual(values = c("#999999", "#E69F00", "#D55E00")) +
  scale_fill_manual(values = plotColors$value) +
  scale_color_manual(values = plotColors$value) +
  scale_alpha_manual(values = c(0.5, 1)) +
  labs(y = "Time(sec)")
