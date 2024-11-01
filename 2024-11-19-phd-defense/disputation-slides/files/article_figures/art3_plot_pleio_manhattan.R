### PLOT PLEIO MANHATTAN PLOTS FOR BOTH FEATURES ########################

#-- Libraries -------------------------

library(tidyverse)
library(ggtext)
library(normentR)

#-- Load data ------------------------

infiles <- list.files(path = "~/Downloads/network_ica_genetics/fuma/", 
                      pattern = "(.*?)_div100k.csv",
                      full.names = TRUE)

gwas_data_load <- infiles |> 
  map_dfr(~ read_delim(.x, delim = "\t", col_types = cols(FDR = col_double())) |> 
            janitor::clean_names() |> 
            mutate(feature = str_extract(basename(.x), "(?<=fuma_)(.*?)(?=_and)")))

gwas_data <- gwas_data_load |> 
  filter(!is.na(fdr)) |> 
  mutate(fdr = fdr * 1e5)

#-- Prepare data -------------------------

sig_data <- gwas_data |> 
  subset(fdr < quantile(fdr, 0.1))

notsig_data <- gwas_data |> 
  subset(fdr >= quantile(fdr, 0.1)) %>%
  group_by(chr) |> 
  slice_sample(prop = 0.1)

gwas_data <- bind_rows(sig_data, notsig_data)

#-- Get cumulative BP position -------------------------

data_cum <- gwas_data |> 
  group_by(chr) |> 
  summarise(max_bp = max(bp)) |> 
  mutate(bp_add = lag(cumsum(max_bp), default = 0)) |> 
  select(chr, bp_add)

gwas_data <- gwas_data |> 
  inner_join(data_cum, by = "chr") |> 
  mutate(bp_cum = bp + bp_add)

#-- Plot settings -------------------------

axis_set <- gwas_data |> 
  group_by(chr) |> 
  summarize(center = (max(bp_cum) + min(bp_cum)) / 2)

ylim <- 8.5
sig <- -log10(0.05)

#-- Get number of loci ------------------------

indir <- "~/Dropbox/NORMENT/R_Scripts/network_ica_genetics"

nloci <- read_table(str_glue("{indir}/files/pleioFDR_nloci.txt")) |> 
  janitor::clean_names() |> 
  right_join(tibble(feature = c("edge", "node"))) |> 
  replace_na(list(nloci = 0)) |> 
  mutate(
    feature_label = case_when(str_detect(feature, "edge") ~ "Functional connectivity",
                              str_detect(feature, "node") ~ "Node variance"),
    nloci_label = str_glue("{nloci} loci"),
    nloci_label_long = case_when(nloci == 1 ~ str_glue("{feature_label}: {nloci} locus"),
                                 TRUE ~ str_glue("{feature_label}: {nloci} loci"))
  )

if (nloci[1,"nloci_label"] == nloci[2,"nloci_label"]) {
  nloci[2,"nloci_label"] <- str_c(nloci[2,"nloci_label"]," ")
}

nloci <- nloci |> 
  arrange(feature) |> 
  mutate(nloci_label = fct_inorder(nloci_label),
         nloci_label_long = fct_inorder(nloci_label_long))

#-- Create plot -------------------------

manhplot <- gwas_data |> 
  left_join(nloci) |> 
  mutate(
    nloci_label_long = fct_rev(nloci_label_long)
  ) |> 
  #slice_sample(prop = 0.2) |> 
  ggplot(aes(x = bp_cum, y = -log10(fdr), 
             color = nloci_label_long, size = -log10(fdr))) +
  geom_point(data = . %>% filter(str_detect(feature, "node")), 
             alpha = 0.5, stroke = 0, key_glyph = "point") +
  geom_point(data = . %>% filter(str_detect(feature, "edge")), 
             alpha = 0.5, stroke = 0, key_glyph = "point") +
  geom_hline(yintercept = sig, color = "grey40", linetype = "dashed") + 
  labs(x = "Chromosome", 
       y = "-log<sub>10</sub>(_q_)",
       color = NULL) +
  scale_x_continuous(label = axis_set$chr, breaks = axis_set$center) +
  scale_y_continuous(expand = expansion(add = 0), limits = c(0, ylim)) +
  scale_color_manual(values = c(norment_colors[["magenta"]], norment_colors[["blue"]]),
                     guide = guide_legend(override.aes = list(size = 6, alpha = 1),
                                          keyheight = unit(0.25,"cm"), reverse = TRUE)) +
  scale_size_continuous(range = c(0.5, 1.5), guide = "none", limits = c(0, ylim)) +
  coord_cartesian(clip = "off") + 
  theme(
    #plot.title.position = "plot",
    legend.position = c(0.15, 0.89),
    axis.title.y = element_markdown(),
    #panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0, size = 6, vjust = 0.5),
    plot.margin = margin(rep(10, 4), unit = "pt")
  )
#print(manhplot)

#-- Save files ------------------------

ggsave("art3_pleio_manhattan_plot.png", manhplot, width = 8, height = 4, bg = "transparent")
