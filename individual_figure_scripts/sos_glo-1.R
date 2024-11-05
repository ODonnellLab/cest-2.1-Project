#Plotting via days with N2, cest-2.1, and glo-1

library(tidyverse)
library(ggplot2)
library(ggtext)
theme_set(theme_classic())


files <- fs::dir_ls(recurse = TRUE, glob = "data/allsosdata.csv")
merged_data <- files %>% purrr::map_df(., readr::read_csv, .id = "filename")

plotColors <- source(file = 'parameters/plotColors.R')


filter_date <- c("2024-05-21", "2023-05-08", "2023-05-09", "2023-06-23", "2023-06-19")

# Filter Data to Date
filtered_data <- merged_data %>%
  filter(Date %in% filter_date, Genotype %in% c("N2", "cest-2.1", "glo-1"))

# Reorder Genotype 
filtered_data$Genotype <- factor(filtered_data$Genotype, levels = c("N2", "cest-2.1", "glo-1")) 


plotbar <- source(file = 'parameters/plotbar.R')

print(plotbar$value) 