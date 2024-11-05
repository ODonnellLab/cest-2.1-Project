#Plotting via days with N2, cest-2.1, and tbh-1

library(tidyverse)
library(ggplot2)
library(ggtext)
theme_set(theme_classic())


files <- fs::dir_ls(recurse = TRUE, glob = "data/allsosdata.csv")
merged_data <- files %>% purrr::map_df(., readr::read_csv, .id = "filename")

plotColors <- source(file = 'parameters/plotColors.R')


filter_date <- c("2022-04-05", "2022-04-27", "2022-05-10", "2022-12-14", "2023-02-08", "2023-05-08", "2023-05-10", "2023-06-22", "2023-06-26")

# Filter Data to Date
filtered_data <- merged_data %>%
  filter(Date %in% filter_date, Genotype %in% c("N2", "cest-2.1", "tbh-1"))

# Reorder Genotype 
filtered_data$Genotype <- factor(filtered_data$Genotype, levels = c("N2", "cest-2.1", "tbh-1")) 

plotbar <- source(file = 'parameters/plotbar.R')

print(plotbar$value) 