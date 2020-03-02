df <- read.csv("./summary/plot_diversity_and_biomass_forte_2018.csv")

df %>%
  group_by(group) %>%
  summarize(plot.rich = max(species))