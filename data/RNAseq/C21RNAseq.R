library(tidyverse)
theme_set(theme_classic())
CAN_genes <- read_csv('data/RNAseq/GenesExpressed_in_CAN-thrs2.csv') %>%
  filter(`Expression level` > 0) %>%
  mutate(target_id = `Gene name`)
C21RNAseq <- read_csv('data/RNAseq/cest-2.1_DEGEnes_ReAn.csv')

C21sig <- C21RNAseq %>%
  filter(qval <0.1) %>%
  mutate(effect = case_when(b > 0 ~ "Up",
                            b < 0 ~ "Down"))

C21nonSig <- C21RNAseq %>%
  filter(qval > 0.1)

C21sigCAN <- inner_join(C21sig, CAN_genes, by = "target_id")

ggplot(C21nonSig, aes(x = b, y = -log10(pval))) +
  geom_hline(aes(yintercept = 3.6), linetype = 2) +
  geom_point(color = "grey") +
  geom_point(data=C21sig, aes(color = effect), alpha = 0.3) +
  geom_point(data=C21sigCAN, aes(color = effect)) +
  coord_cartesian(xlim = c(-3,3), ylim = c(0,40)) +
  scale_color_manual(values = c("purple","green"))


# library(gprofiler2)
# 
# C21sigUP <- C21RNAseq %>%
#   filter(qval <0.1 & b > 0) %>%
#   mutate(effect = case_when(b > 0 ~ "Up",
#                             b < 0 ~ "Down"))
# 
# 
# gost(C21sig$target_id,
#      organism = "celegans")
#   

C21_cengen <- read_csv('data/RNAseq/GenesExpressing-C21sig-thrs2.csv') %>%
  select(-1,-3,-4) %>%
  pivot_longer(cols = -1, names_to = "neuron", values_to = "expression") %>%
  filter(expression > 0)

C21_cengen %>% 
  group_by(neuron) %>%
  tally() %>%
  arrange(desc(n))

C21_cengen %>%
  filter(neuron == "CAN") %>%
  arrange(desc(expression))


