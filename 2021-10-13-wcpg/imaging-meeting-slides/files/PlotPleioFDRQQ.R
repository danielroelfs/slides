### CREATE PLEIO QQ-PLOT ########################

#-- Libraries -------------------------

library(tidyverse)
library(ggtext)
library(normentR)

#-- Load data -------------------------

infiles <- list.files(path = "~/Dropbox/NORMENT/R_Scripts/network_genetics/files/pleio_qqdata", pattern = ".txt",
                      full.names = TRUE)

data_load <- map_dfr(infiles, ~ read_csv(.x, col_types = cols()) %>% 
                       mutate(diagnosis = str_split_fixed(basename(.x), pattern = "_", n = 7)[4],
                              feature = str_extract(basename(.x), "(?<=vs_)(.*?)(?=.txt)")))

#-- Label ------------------------

plotdata <- data_load %>% 
  mutate(feature_label = case_when(feature == "edge" ~ "Functional Connectivity",
                                   feature == "node" ~ "Node variance"),
         threshold = case_when(str_detect(label, "-3") ~ "<em>p</em> < 10<sup>-3</sup>",
                               str_detect(label, "-2") ~ "<em>p</em> < 10<sup>-2</sup>",
                               str_detect(label, "-1") ~ "<em>p</em> < 10<sup>-1</sup>",
                               TRUE ~ label))

color_palette <- c(norment_colors[["light blue"]],norment_colors[["green"]], norment_colors[["magenta"]], norment_colors[["purple"]])

#-- Create plot ------------------------

qqplot <- plotdata %>% 
  filter(threshold != "Expected") %>% 
  mutate(threshold = fct_relevel(threshold, "All SNPs", after = 0)) %>% 
  ggplot(aes(x = x, y = y, color = threshold, group = threshold)) + 
  geom_segment(aes(x = 0, xend = -log10(5e-8),
                   y = 0, yend = -log10(5e-8)),
               inherit.aes = FALSE,
               color = "grey", linetype = "dashed") +
  geom_line(alpha = 0.75, lineend = "round", key_glyph = "point") + 
  labs(x = "Empirical -log<sub>10</sub>(<em>q</em>)",
       y = "Nominal -log<sub>10</sub>(<em>p</em>)",
       color = NULL) +
  scale_x_continuous(breaks = seq(0,8), limits = c(NA,-log10(5e-8))) + 
  scale_y_continuous(breaks = seq(0,8), limits = c(NA,-log10(5e-8))) +
  scale_color_manual(values = color_palette,
                     guide = guide_legend(override.aes = list(size = 6, alpha = 0.9))) +
  facet_grid(feature_label ~ diagnosis) +
  theme(plot.title.position = "plot",
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        panel.grid.minor = element_blank(),
        strip.text = element_markdown(size = 8, face = "bold"),
        axis.text.x = element_markdown(size = 6),
        axis.text.y = element_markdown(size = 6),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_markdown())

ggsave("qq_pleio_figure.png", qqplot, height = 5, width = 10, dpi = 600)

