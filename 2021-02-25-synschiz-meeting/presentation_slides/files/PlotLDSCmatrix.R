### PLOT LDSC CORRELATION MATRIX ########################

#-- Libraries ------------------------

library(tidyverse)
library(normentR)

#-- Functions ------------------------

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

#-- Load data ------------------------

ldsc <- read.table("~/Dropbox/NORMENT/R_Scripts/ICA_genetics/files/LDSC_stats.txt", header = TRUE) %>%
  mutate(p.fdr = p.adjust(p, method = "fdr"),
         p.bonf = p.adjust(p, method = "bonferroni"))

gwas_names <- read_delim("~/Dropbox/NORMENT/R_Scripts/ICA_genetics/files/GWAS_names.txt", delim = "\t")
ic_defs <- read_delim("~/Dropbox/NORMENT/R_Scripts/ICA_genetics/files/IC_names.txt", delim = "\t")

#-- Prepare data ------------------------

plotdata <- ldsc %>%
  right_join(gwas_names, by = "diagnosis") %>%
  right_join(ic_defs, by = "component") %>%
  mutate(rg_inv = rg * trans,
         comp_new = paste0("IC",parse_number(comp_label)))

plotdata <- plotdata %>%
  mutate(comp_new = fct_reorder(comp_new,parse_number(comp_label)),
         comp_def = fct_reorder(comp_def,parse_number(comp_label)),
         comp_new = fct_drop(comp_new),
         comp_def = fct_drop(comp_def))

#-- Create plots ------------------------

p <- ggplot(plotdata, aes(x = reorder_within(comp_new,-abs(rg), diagnosis_new), 
                          y = 1, fill = rg_inv)) +
  geom_point(shape = 22, aes(size = se), color = "transparent") +
  geom_point(data = . %>% filter(p.fdr < 0.05), aes(size = se), 
             shape = 22, color = "#FB6666", stroke = 1.5) +
  geom_text(aes(label = comp_new),  vjust = -0.5, fontface = "bold") +
  geom_text(aes(label = sprintf("(%s)",round(rg_inv,2))), vjust = 1) +
  labs(x = "Rank",
       y = NULL,
       fill = bquote("Genetic correlation "~(r[g])),
       size = "Standard Error") +
  scale_discrete_identity(
    aesthetics = "label",
    name = NULL,
    breaks = levels(plotdata$comp_new),
    labels = levels(plotdata$comp_def),
    guide = "legend"
  ) +
  scale_x_reordered(position = "top") +
  scale_y_continuous(labels = NULL) +
  scale_fill_norment(discrete = FALSE, palette = "vik", limits = c(-1,1),
                     guide = guide_colorbar(nbin = 100, barheight = 0.75, barwidth = 15, reverse = FALSE, 
                                            direction = "horizontal", 
                                            ticks = FALSE, title.vjust = 1, order = 2)) +
  scale_size_continuous(range = c(6,20), trans = "reverse",
                        guide = guide_legend(order = 3, direction = "horizontal", label.position = "bottom",
                                             title.vjust = 0.6, override.aes = list(fill = "grey80"))) +
  guides(label = guide_legend(order = 1, keywidth = 2, ncol = 2, key.hjust = 1, 
                              override.aes = list(size = 4, color = "white"))) +
  theme_norment(grid = FALSE, base_size = 12, axis_title_size = 14, bg_col = "transparent") +
  coord_cartesian() +
  theme(
    text = element_text(colour = "white"),
    legend.position = "right",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.title.x.top = element_text(size = 16),
    strip.text.y.left = element_text(angle = 0, hjust = 1, color = "white", size = 16),
    panel.spacing.y = unit(0, "lines")
  ) +
  facet_wrap(~ reorder(diagnosis_new, -abs(rg)), scales = "free", 
             strip.position = "left", ncol = 1)
ggsave("~/Dropbox/University/PhD/210225 SYNSCHIZ Meeting/files/LDSC_stats_matrix.png", p, width = 20, height = 8, dpi = 200, bg = "transparent")


