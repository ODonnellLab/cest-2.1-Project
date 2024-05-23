#CFA 

library(tidyverse)
library(ggplot2)
library(ggtext)
theme_set(theme_classic())


files <- fs::dir_ls(recurse = TRUE, glob = "data/allsosdata.csv")
merged_data <- files %>% purrr::map_df(., readr::read_csv, .id = "filename")

plotColors <- source(file = 'parameters/plotColors.R')


filter_date <- c("2024-05-21")
       
filtered_data <- merged_data %>%
  filter(Date %in% filter_date, Bacteria %in% c("OP50", "CFA_KO")) %>%
  filter(Genotype %in% c("N2", "fcmt-1")) %>%
  mutate(Genotype = fct_relevel(Genotype, "N2", "fcmt-1"))

filtered_data$Genotype <- factor(filtered_data$Genotype, levels = c("N2", "fcmt-1"))
       
       ggplot(filtered_data, aes(x = Bacteria, y = Response.time)) +
         stat_summary(geom = "bar", aes(fill = Genotype, alpha = Bacteria), fun = "mean") +
         labs(fill = "Genotype") +
         ggbeeswarm::geom_quasirandom(alpha = 0.5, width=0.2) +
         stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
         #labs(title = "OA SOS with N2, cest-2.1, and tbh-1 for 30% octanol avoidance") +
         facet_grid(~Genotype) +
         scale_x_discrete(labels = c( "OP50 CFA_KO", "OP50")) +
         #labs(title = "OA SOS with N2, cest-2.1, and tbh-1 for 30% octanol avoidance") +
         scale_y_continuous(expand = c(0, 0)) +
         geom_text(aes(x = 1, y = 20, label = "Stretch it"), vjust = -1) +
         #scale_fill_manual(values = c("#999999", "#E69F00", "#D55E00")) +
         scale_fill_manual(values = genotype_colors) +
         scale_color_manual(values = genotype_colors) +
         scale_alpha_manual(values = c(0.5, 1)) +
         labs(y = "Time(sec)")
       
       
       
       filter_date <- c("2024-05-21")
       filtered_data <- merged_data %>%
         filter(Date %in% filter_date, Bacteria %in% c("OP50", "CFA_KO")) %>%
         filter(Date %in% filter_date, Genotype %in% c("N2", "fcmt-1"))
       mutate(Genotype = fct_relevel(Genotype, "N2", "fcmt-1") 