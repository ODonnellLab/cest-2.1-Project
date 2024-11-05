#Plotting all CAN gene data:
#


library(tidyverse)
library(ggplot2)
library(ggtext)
theme_set(theme_classic())


merged_data <- read_csv("data/allsosdata.csv")

merged_data %>% group_by(Genotype) %>% tally()

# get dates that used CAN gene data:
CANdays <- merged_data %>% 
  filter(Genotype %in% 
           c('sqv-7', 'ugt-64')) %>%
  pull(Date) %>% unique()

# now filter with these days
CANdata1 <- merged_data %>%
  filter(Genotype %in% c("N2",'cest-2.1','sqv-7', 'ugt-64'),
                Date %in% CANdays) %>%
  mutate(Genotype = fct_relevel(Genotype, "N2", "cest-2.1"))

CANdata1 %>%
  ggplot(aes(x = Genotype,
             y = Response.time)) + 
  stat_summary(geom = "bar", fun = "mean", aes(fill = Genotype), width = 0.75) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.25) +
  scale_y_continuous(expand = c(0,0))


# 
# CANdata %>%
#   lme4::lmer(data = ., Response.time ~ Genotype + (1|Date)) %>%
#   lmerTest::as_lmerModLmerTest() %>%
#   summary()


CANdata2 <- read_csv(file = 'data/UnSosdata/SOS_CAN transporter mutants.csv', col_names = TRUE)

CANdata2_long <- CANdata2 %>% 
  pivot_longer(cols = -1, values_to = "Response.time") %>%
  rename(Date = 1) %>%
  separate(name, into = c("Genotype", "animal_number"), sep = "\\.{3}") %>%
  mutate(Genotype = case_when(Genotype == "wt" ~ "N2",
                              TRUE ~ Genotype))


AllCANdata <- rbind(select(CANdata1, c(Date, Genotype, Response.time)), 
                    select(CANdata2_long, c(Date, Genotype, Response.time)))

meds <- AllCANdata %>%
  group_by(Genotype, Date) %>%
  summarise(Response.time = median(Response.time, na.rm = TRUE))

means <- AllCANdata %>%
  group_by(Genotype, Date) %>%
  summarise(Response.time = mean(Response.time, na.rm = TRUE))

AllCANdata %>% ggplot(aes(x = Genotype, y = Response.time)) +
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



AllCANdata %>%
  lm(data = ., formula = Response.time ~ Genotype) %>%
  summary()

means %>%
  lm(data = ., formula = Response.time ~ Genotype) %>%
  emmeans::ref_grid() %>%
  emmeans::contrast(method = "pairwise")
                    