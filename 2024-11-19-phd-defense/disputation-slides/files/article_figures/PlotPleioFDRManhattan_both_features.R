### PLOT PLEIO MANHATTAN PLOTS FOR BOTH FEATURES ########################

#-- Libraries -------------------------

library(tidyverse)
library(ggtext)
library(optparse)
library(normentR)

#-- Parse arguments ------------------------

option_list <- list(
  make_option(c("-d", "--diagnosis"), type = "character", default = NULL, 
              help = "Diagnosis to create the manhattan plot for", metavar = "character")
) 

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

if (is.null(opt$diagnosis)) {
  print_help(opt_parser)
  stop("You didn't specify the diagnosis idiot!\n", call. = FALSE)
} else {
  diag <- opt$diagnosis
  print(str_glue("Creating Manhattan plot for {diag}"))
}

#-- Load data ------------------------

#diag <- "SCZ"
suffix <- "_17networks"
suffix <- ""

indir <- "~/Dropbox/NORMENT/R_Scripts/network_genetics/"

infiles <- list.files(path = str_glue("~/Downloads/network_genetics/fuma{suffix}/"), 
                      pattern = str_glue("{diag}(.*?)_div100k.csv"),
                      full.names = TRUE)

gwas_data_load <- infiles |> 
  map_dfr(~ read_delim(.x, delim = "\t", col_types = cols(FDR = col_double())) |> 
            janitor::clean_names() |> 
            mutate(feature = str_extract(basename(.x), "(?<=fuma_)(.*?)(?=_and)")))

gwas_data <- gwas_data_load |> 
  filter(!is.na(fdr)) |> 
  mutate(fdr = fdr * 1e5)

#-- Prepare data -------------------------

#gwas_data <- gwas_data |> 
#  group_by(sample_thr = fdr < quantile(fdr, 0.1)) |> 
#  mutate(sample_thr = case_when(sample_thr ~ "not_sample",
#                                !sample_thr ~ "sample")) |> 
#  nest() |>            
#  ungroup() |> 
#  mutate(n = ifelse(sample_thr == "sample", 0.1, 1)) |> 
#  mutate(sampled = map2(data, n, sample_frac)) |> 
#  select(-c(data,n)) %>%
#  unnest(sampled) |> 
#  arrange(chr,bp)

sig_data <- gwas_data |> 
  subset(fdr < quantile(fdr, 0.1))

notsig_data <- gwas_data |> 
  subset(fdr >= quantile(fdr,0.1)) %>%
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

ylim <- ifelse(suffix == "_17networks", 7, 5)

sig <- -log10(0.05)

#-- Get number of loci ------------------------

nloci <- read_table(str_glue("{indir}/files/pleioFDR_nloci{suffix}.txt"), col_types = cols()) |> 
  janitor::clean_names() |> 
  filter(str_detect(diagnosis, diag)) |> 
  right_join(tibble(mostest = c(str_glue("edge{suffix}"), str_glue("node{suffix}")))) |> 
  replace_na(list(nloci = 0)) |> 
  mutate(feature = case_when(str_detect(mostest, "edge") ~ "FC",
                             str_detect(mostest, "node") ~ "Node variance"),
         nloci_label = str_glue("{nloci} loci"),
         nloci_label_long = case_when(nloci == 1 ~ str_glue("{feature}: {nloci} locus"),
                                      TRUE ~ str_glue("{feature}: {nloci} loci")))

if (nloci[1,"nloci_label"] == nloci[2,"nloci_label"]) {
  nloci[2,"nloci_label"] <- paste0(nloci[2,"nloci_label"]," ")
}

nloci <- nloci |> 
  arrange(feature) |> 
  mutate(nloci_label = fct_inorder(nloci_label),
         nloci_label_long = fct_inorder(nloci_label_long))

#-- Create plot -------------------------

diag <- ifelse(diag == "ED", "AN", diag)

manhplot <- gwas_data |> 
  slice_sample(prop = 0.1) |> 
  mutate(feature_label = case_when(str_detect(feature,"edge") ~ "Functional connectivity (FC)",
                                   str_detect(feature,"node") ~ "Node variance"),
         feature_label = as_factor(feature_label),
         feature_label = fct_relevel(feature_label, "Functional connectivity (FC)", "Node variance")) |> 
  ggplot(aes(x = bp_cum, y = -log10(fdr), 
             color = feature_label, size = -log10(fdr))) +
  geom_point(data = nloci, aes(x = 0, y = 0, fill = nloci_label), 
             shape = 21, stroke = 0, inherit.aes = FALSE) + 
  geom_point(data = . %>% filter(str_detect(feature,"node")), alpha = 0.75, stroke = 0, key_glyph = "point") +
  geom_point(data = . %>% filter(str_detect(feature,"edge")), alpha = 0.75, stroke = 0, key_glyph = "point") +
  geom_hline(yintercept = sig, color = "grey40", linetype = "dashed") + 
  labs(title = str_glue("{diag} and connectome"),
       #x = "Chromosome", 
       y = "-log<sub>10</sub>(_q_)",
       fill = NULL,
       color = NULL) +
  scale_x_continuous(label = axis_set$chr, breaks = axis_set$center) +
  scale_y_continuous(expand = c(0,0), limits = c(0, ylim)) +
  scale_color_manual(values = c("Functional connectivity (FC)" = norment_colors[["purple"]], 
                                "Node variance" = norment_colors[["light blue"]]),
                     #guide = guide_legend(nrow = 2, reverse = TRUE, override.aes = list(size = 6, alpha = 1),
                     #                     keyheight = unit(0.25,"cm"))) +
                     guide = "none") +
  scale_fill_manual(values = c(norment_colors[["purple"]], norment_colors[["light blue"]]),
                    guide = guide_legend(nrow = 2, override.aes = list(size = 6, alpha = 1),
                                         keyheight = unit(0.25,"cm"))) +
  scale_size_continuous(range = c(0.2,1.5), guide = "none", limits = c(0,ylim)) +
  theme(
    #plot.title.position = "plot",
    legend.position = c(0.9, 0.90),
    axis.title.x = element_blank(),
    axis.title.y = element_markdown(),
    #panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.y = element_blank(),
    #panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 90, size = 6, vjust = 0.5)
  )
#print(manhplot)

#-- Save files ------------------------

ggsave(str_glue("manhattan_plots/pleio_manhattan_fc_{diag}{suffix}_legend.png"), manhplot, width = 7, height = 3.5)

