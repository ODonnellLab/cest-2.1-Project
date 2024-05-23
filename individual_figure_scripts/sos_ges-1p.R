#Plotting via days with N2, cest-2.1, and MOY00022/ MOY00023--ges-1p cest-2.1 rescue

library(tidyverse)
library(ggplot2)
library(ggtext)
theme_set(theme_classic())


files <- fs::dir_ls(recurse = TRUE, glob = "data/allsosdata.csv")
merged_data <- files %>% purrr::map_df(., readr::read_csv, .id = "filename")

plotColors <- source(file = 'parameters/plotColors.R')


# Filter Data to Date
filter_date <- c("2024-04-30", "2024-05-07", "2024-05-10")
filtered_data <- merged_data %>%
  filter(Date %in% filter_date, Genotype %in% c("N2", "cest-2.1", "TIBR1", "TIBR2"))

# Reorder Genotype 
filtered_data$Genotype <- factor(filtered_data$Genotype, levels = c("N2", "cest-2.1", "TIBR1", "TIBR2"))

ggplot(filtered_data, aes(x = Genotype, y = Response.time)) +
  stat_summary(geom = "bar", aes(fill = Genotype), width= 0.5) +
  labs(fill = "Genotype") +
  ggbeeswarm::geom_quasirandom(alpha = 0.5, width=0.2) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
  #labs(title = "OA SOS with N2, cest-2.1, and tbh-1 for 30% octanol avoidance") +
  scale_y_continuous(expand = c(0, 0)) +
  geom_text(aes(x = 1, y = 20, label = "Stretch it"), vjust = -1) +
  scale_fill_manual(values = genotype_colors) +
  scale_color_manual(values = genotype_colors) + 
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.text = element_text(face = "italic")) +
  theme(text = element_text(size = 10)) +
  labs(y = "Time(sec)")