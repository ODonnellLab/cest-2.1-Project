#Plotting OA SOS with N2, cest-2.1, and tbh-1

#40uM Figure


library(tidyverse)
library(ggplot2)
library(ggtext)
library(dplyr)

theme_set(theme_classic())
files <- fs::dir_ls(recurse = TRUE, glob = "data/allsosdata.csv")
merged_data <- files %>% purrr::map_df(., readr::read_csv, .id = "filename")

#Load set Colors for figure
plotColors <- source(file = 'parameters/plotColors.R')

filter_date <- c( "2024-05-06", "2024-04-30", "2024-05-14" )
filtered_data <- merged_data %>%
  filter(Date %in% filter_date, Condition %in% c("control", "OA")) %>%
  mutate(Genotype = fct_relevel(Genotype, "N2", "cest-2.1", "tbh-1"),
         Condition = fct_relevel(Condition, "control", "OA"))

# Specific Genotype Order
filtered_data$Genotype <- factor(filtered_data$Genotype, levels = c("N2", "cest-2.1", "tbh-1"))

ggplot(filtered_data, aes(x = Condition, y = Response.time)) +
  stat_summary(geom = "bar", aes(fill = Genotype, alpha = Condition), fun = "mean") +
  labs(fill = "Genotype") +
  ggbeeswarm::geom_quasirandom(alpha = 0.5, width=0.2) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
  #labs(title = "OA SOS with N2, cest-2.1, and tbh-1 for 30% octanol avoidance") +
  facet_grid(~Genotype) +
  scale_x_discrete(labels = c( "control", "OA")) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_text(aes(x = 1, y = 20, label = "Stretch it"), vjust = -1) +
  scale_fill_manual(values = plotColors$value) +
  scale_color_manual(values = plotColors$value) +
  scale_alpha_manual(values = c(0.25, 1)) +
  labs(y = "Time(sec)")


