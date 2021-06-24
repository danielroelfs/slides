### SIMULATE MANHATTAN FIGURE ########################

#-- Libraries -------------------------

library(tidyverse)
library(normentR)

#-- Load data ------------------------

set.seed(6)

gwas_data_load <- simulateGWAS(nSNPs = 1e6, nSigCols = 4) %>% 
  janitor::clean_names()

#-- Remove redundant non-significant SNPs ------------------------

sig_data <- gwas_data_load %>% 
  subset(p < 0.05)

notsig_data <- gwas_data_load %>% 
  subset(p >= 0.05) %>%
  group_by(chr) %>% 
  sample_frac(0.1)

gwas_data <- bind_rows(sig_data, notsig_data)

#-- Get cumulative base-pair position ------------------------

data_cum <- gwas_data %>% 
  group_by(chr) %>% 
  summarise(max_bp = max(bp)) %>% 
  mutate(bp_add = lag(cumsum(max_bp), default = 0)) %>% 
  select(chr, bp_add)

gwas_data <- gwas_data %>% 
  inner_join(data_cum, by = "chr") %>% 
  mutate(bp_cum = bp + bp_add)

#-- Figure properties ------------------------

ylim <- gwas_data %>% 
  filter(p == min(p)) %>% 
  mutate(ylim = abs(floor(log10(p))) + 2) %>% 
  pull(ylim)

sig <- 5e-8

#-- Label ------------------------

manhplot <- gwas_data %>% 
  mutate(sig = ifelse(p < 5e-8, "sig", "notsig")) %>% 
  ggplot(aes(x = bp_cum, y = -log10(p),  color = sig, size = -log10(p))) +
  geom_point(alpha = 0.75, stroke = 0) +
  scale_y_continuous(expand = c(0,0), limits = c(0, ylim)) +
  scale_color_manual(values = c("#333333", "#21a695")) +
  scale_size_continuous(range = c(2,5)) +
  theme_void() + 
  theme(legend.position = "none")

ggsave("manhattan_plot_psych.png", plot = manhplot, width = 6, height = 3, dpi = 300, bg = "transparent")

