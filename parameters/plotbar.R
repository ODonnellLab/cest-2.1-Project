
plotbargraph <- ggplot(filtered_data, aes(x = Genotype, y = Response.time)) +
  stat_summary(geom = "bar", aes(fill = Genotype), width= 0.5) +
  labs(fill = "Genotype") +
  ggbeeswarm::geom_quasirandom(alpha = 0.5, width=0.2) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
  #labs(title = "SOS with N2, cest-2.1, and tbh-1 for 30% octanol avoidance") +
  scale_y_continuous(expand = c(0, 0)) +
  geom_text(aes(x = 1, y = 20, label = "Stretch it"), vjust = -1) +
  #scale_fill_manual(values = c("#999999", "#E69F00", "#D55E00")) +
  scale_fill_manual(values = genotype_colors) +
  scale_color_manual(values = genotype_colors) +
  scale_alpha_manual(values = c(0.5, 1)) +
  labs(y = "Time(sec)")