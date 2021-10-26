### SIMULATE MANHATTAN ICA ########################

#-- Libraries -------------------------

library(tidyverse)
library(ggtext)
library(normentR)

#-- Load data ------------------------

for (i in seq(5)) {
  
  set.seed(i + i)
  
  ic_colors <- c("#9874b2","#ef87b5","#68aee1","#96c9bf","#e7e783")
  
  gwas_data_load <- simulateGWAS(nSNPs = 1e6, nSigCols = 6) %>% 
    janitor::clean_names()
  
  sig_data <- gwas_data_load %>% 
    subset(p < 0.05)
  
  notsig_data <- gwas_data_load %>% 
    subset(p >= 0.05) %>%
    group_by(chr) %>% 
    sample_frac(0.1)
  
  gwas_data <- bind_rows(sig_data, notsig_data)
  
  data_cum <- gwas_data %>% 
    group_by(chr) %>% 
    summarise(max_bp = max(bp)) %>% 
    mutate(bp_add = lag(cumsum(max_bp), default = 0)) %>% 
    select(chr, bp_add)
  
  gwas_data <- gwas_data %>% 
    inner_join(data_cum, by = "chr") %>% 
    mutate(bp_cum = bp + bp_add)
  
  ylim <- gwas_data %>% 
    filter(p == min(p)) %>% 
    mutate(ylim = abs(floor(log10(p))) + 2) %>% 
    pull(ylim)
  
  sig <- 5e-8
  
  manhplot <- gwas_data %>% 
    slice_sample(n = 1e5) %>% 
    mutate(sig = ifelse(p < 5e-8, "sig", "notsig")) %>% 
    ggplot(aes(x = bp_cum, y = -log10(p),  color = sig, size = -log10(p))) +
    geom_point(alpha = 0.75, stroke = 0) +
    geom_richtext(data = tibble(), aes(x = 1e6, y = ylim - 2, label = str_glue("**IC{i}**")), 
                  color = ic_colors[i], size = 18, family = "Poppins", hjust = 0, vjust = 1,
                  label.size = 0, inherit.aes = FALSE) + 
    scale_y_continuous(expand = c(0,0), limits = c(0, ylim)) +
    scale_color_manual(values = c("#333333", ic_colors[i])) +
    scale_size_continuous(range = c(2,5)) +
    theme_void() + 
    theme(panel.border = element_rect(color = ic_colors[i], fill = NA, size = 6),
          legend.position = "none")
  
  ggsave(str_glue("manhattan_plots/manhattan_plot_ic{i}.png"), plot = manhplot, width = 6, height = 3, dpi = 300, bg = "white")
  
}

