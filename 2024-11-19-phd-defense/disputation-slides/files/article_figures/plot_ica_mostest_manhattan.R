### PLOT ICA MOSTEST MANHATTAN ########################

#-- Libraries -------------------------

library(tidyverse)
library(ggtext)
library(normentR)

#-- Load data ------------------------

most_file <- "~/Downloads/network_ica_genetics/mostest_ica_noimg.most_orig.sumstats.gz"
most_load <- read_table(most_file, col_types = cols()) |> 
  janitor::clean_names()

mica_genelist <- read_delim("~/Downloads/FUMA_job159474/genes.txt") |> 
  janitor::clean_names()

#-- Prepare data -------------------------

most_sig <- most_load |> 
  filter(pval < 0.01 & pval > 0) |> 
  mutate(method = "mostest")

set.seed(1994)
most_notsig <- most_load |> 
  subset(pval >= 0.01) %>%
  group_by(chr) |> 
  sample_frac(size = 0.05) |> 
  mutate(method = "mostest")

data_small <- bind_rows(most_sig, most_notsig)

#-- Get cumulative BP position -------------------------

data_cum <- data_small |> 
  group_by(chr) |> 
  summarise(max_bp = max(bp)) |> 
  mutate(bp_add = lag(cumsum(max_bp), default = 0)) |> 
  select(chr, bp_add)

gwas_data <- data_small |> 
  inner_join(data_cum, by = "chr") |> 
  mutate(bp_cum = bp + bp_add)

#-- Plot settings -------------------------

axis_set <- gwas_data |> 
  group_by(chr) |> 
  summarize(center = mean(bp_cum))

ylim <- gwas_data |> 
  filter(pval == min(pval)) |> 
  mutate(ylim = abs(floor(log10(pval)) - 3)) |> 
  pull(ylim)

sig <- 5e-8

#-- Add gene names ------------------------

loci_annot <- mica_genelist |> 
  group_by(genomic_locus) |> 
  filter(min_gwas_p == min(min_gwas_p)) |> 
  mutate(snp = word(ind_sig_sn_ps, 1, sep = ";")) %>%
  left_join(gwas_data |> select(chr, snp, pval, bp_cum))

#-- Plot Miami plots ------------------------

manhattan_plot <- gwas_data |> 
  ggplot(aes(x = bp_cum, y = -log10(pval), color = as.factor(chr), size = -log10(pval))) +
  geom_hline(yintercept = -log10(sig), color = "grey40", linetype = "dashed") + 
  geom_point(alpha = 0.75, stroke = 0) +
  ggrepel::geom_text_repel(data = loci_annot, aes(label = symbol), 
                           color = "black", size = 2.75, alpha = 0.8,
                           force = 12, ylim = 10.5, seed = 42,
                           min.segment.length = 0, point.padding = 0.5,
                           segment.size = 0.1, segment.alpha = 0.5) +
  labs(title = NULL,
       x = "Chromosome", 
       y = "-log<sub>10</sub>(_p_)") + 
  scale_x_continuous(label = axis_set$chr, breaks = axis_set$center) +
  scale_y_continuous(expand = c(0,0), limits = c(0, ylim)) +
  scale_color_manual(values = rep(c("#0D423B", "#1D9586"), unique(length(axis_set$chr)))) +
  scale_size_continuous(range = c(0.4, 1.5)) +
  coord_cartesian(clip = "off") + 
  theme_norment(legend = FALSE) +
  theme(
    axis.title.y = element_markdown(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0, size = 6, vjust = 0.5),
    plot.margin = margin(rep(10,4), unit = "pt")
  )

ggsave("ica_mostest_manhattan.png", manhattan_plot, width = 8, height = 4, dpi = 300, bg = "transparent")
