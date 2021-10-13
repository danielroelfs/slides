### SIMULATE MANHATTAN FIGURE ########################

#-- Libraries -------------------------

library(tidyverse)
library(normentR)

#-- Load data ------------------------

set.seed(21)

gwas_data_load_most <- simulateGWAS(nSNPs = 1e6, nSigCols = 10) %>% 
  janitor::clean_names() %>% 
  mutate(method = "mostest")

gwas_data_load_minp <- simulateGWAS(nSNPs = 1e6, AddSigSNPs = FALSE) %>% 
  janitor::clean_names() %>% 
  filter(chr %!in% c(1,9,11)) %>% 
  mutate(method = "minp")

gwas_data_load_most_coi <- gwas_data_load_most %>% 
  filter(chr %in% c(1,9,11)) %>% 
  mutate(method = "minp")

gwas_data_load <- bind_rows(gwas_data_load_most, gwas_data_load_minp, gwas_data_load_most_coi)

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
  group_by(method, chr) %>% 
  summarise(max_bp = max(bp)) %>% 
  mutate(bp_add = lag(cumsum(max_bp), default = 0)) %>% 
  select(chr, bp_add)

gwas_data <- gwas_data %>% 
  inner_join(data_cum, by = c("method","chr")) %>% 
  mutate(bp_cum = bp + bp_add)

#-- Figure properties ------------------------

ylim <- gwas_data %>% 
  group_by(method) %>% 
  filter(p == min(p)) %>% 
  mutate(ylim = abs(floor(log10(p))) + 2) %>% 
  pull(ylim)

sig <- 5e-8

#-- Label ------------------------

miamiplot <- gwas_data %>% 
  mutate(sig = ifelse(p < 5e-8, "sig", "notsig"),
         logp = -log10(p),
         logp = ifelse(method == "minp", logp * -1, logp)) %>% 
  ggplot(aes(x = bp_cum, y = logp,  color = sig, size = abs(logp))) +
  geom_point(alpha = 0.75, stroke = 0) +
  geom_hline(yintercept = 0, size = 1, color = "white", linetype = "longdash") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(ylim[2] * -1, ylim[1])) +
  scale_color_manual(values = c("#333333", "#e5007d")) +
  scale_size_continuous(range = c(2,5)) +
  coord_cartesian(clip = "off") +
  theme_void() + 
  theme(legend.position = "none",
        plot.margin = margin(0, 20, 0, 20, unit = "pt"))

ggsave("manhattan_plots/sim_miami_plot.png", plot = miamiplot, width = 6, height = 4, dpi = 300, bg = "transparent")

