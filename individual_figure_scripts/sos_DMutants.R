#Plotting all CAN gene data:
#


library(tidyverse)
library(ggplot2)
library(ggtext)
theme_set(theme_classic())


DMutData <- read_csv(file = 'data/UnSosdata/SOS_double mutant and cat-1.csv', col_names = TRUE)

DMutData_long <- DMutData %>% 
  pivot_longer(cols = -1, values_to = "Response.time") %>%
  rename(Date = 1) %>%
  separate(name, into = c("Genotype", "animal_number"), sep = "\\.{3}") %>%
  mutate(Genotype = case_when(Genotype == "wt" ~ "N2",
                              TRUE ~ Genotype)) %>%
  mutate(Genotype = fct_relevel(Genotype, "N2", "cest-2.1", "tbh-1", "tbh-1; cest-2.1", "tph-1; cest-2.1", "bas-1; cest-2.1", "cat-1"))

meds <- DMutData_long %>%
  group_by(Genotype, Date) %>%
  summarise(Response.time = median(Response.time, na.rm = TRUE))

means <- DMutData_long %>%
  group_by(Genotype, Date) %>%
  summarise(Response.time = mean(Response.time, na.rm = TRUE))

DMutData_long %>% ggplot(aes(x = Genotype, y = Response.time)) +
  stat_summary(geom = "bar", aes(fill = Genotype), width= 0.65) +
  labs(fill = "Genotype") +
  ggbeeswarm::geom_quasirandom(alpha = 0.1, width=0.2) +
  ggbeeswarm::geom_quasirandom(alpha = 1, width = 0.2, data = means) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.1) +
  #labs(title = "SOS with N2, cest-2.1, and tbh-1 for 30% octanol avoidance") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0,20)) +
  scale_fill_manual(values = genotype_colors) +
  scale_color_manual(values = genotype_colors) +
  scale_alpha_manual(values = c(0.5, 1)) +
  labs(y = "Time(sec)")


DMutData_long %>%
  lm(data = ., formula = Response.time ~ Genotype) %>%
  emmeans::ref_grid() %>%
  emmeans::contrast(method = "pairwise")
means %>%
  lm(data = ., formula = Response.time ~ Genotype) %>%
  emmeans::ref_grid() %>%
  emmeans::contrast(method = "pairwise")
