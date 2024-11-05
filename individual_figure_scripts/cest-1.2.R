#SOS with cest-1.2

library(tidyverse)
library(ggplot2)
library(ggtext)
theme_set(theme_classic())


files <- fs::dir_ls(recurse = TRUE, glob = "data/allsosdata.csv")
merged_data <- files %>% purrr::map_df(., readr::read_csv, .id = "filename")

plotColors <- source(file = 'parameters/plotColors.R')


#filter_date <- c("2024-05-21")

filter_date <- c("2024-05-21")
filtered_data <- merged_data %>%
  filter(Date %in% filter_date, Bacteria %in% c("OP50")) %>%
  filter(Date %in% filter_date, Genotype %in% c("N2", "cest-2.1", "cest-1.2", "glo-1", "cat-1"))  %>%
  mutate(Genotype = fct_relevel(Genotype, "N2", "cest-2.1", "cest-1.2", "glo-1", "cat-1"))
# Filter Data to Date
#filtered_data <- merged_data %>%
  #filter(Date %in% filter_date, Genotype %in% c("N2", "cest-2.1", "cest-1.2", "glo-1", "cat-1"))
# Reorder Genotype 

filtered_data$Genotype <- factor(filtered_data$Genotype, levels = c("N2", "cest-2.1", "cest-1.2", "glo-1", "cat-1")) 

plotbar <- source(file = 'parameters/plotbar.R')

print(plotbar$value) 