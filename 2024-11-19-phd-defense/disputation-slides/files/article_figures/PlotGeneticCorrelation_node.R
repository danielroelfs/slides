### PLOT GENETIC CORRELATION (NODE) ########################

#-- Libraries -------------------------

library(tidyverse)
library(ggtext)
library(cowplot)
library(normentR)

#-- Load data -------------------------

indir <- "~/Dropbox/NORMENT/R_Scripts/network_genetics"

ldsc_load <- read_table(str_glue("{indir}/files/LDSC_stats_node.txt")) |> 
  mutate(node = parse_number(node))

h2_load <- read_table(str_glue("{indir}/files/LDSC_h2_node.txt")) |> 
  rename(h2_se = se) |> 
  mutate(h2_thres = ifelse(1.96 * h2_se < h2, h2, NA),
         h2_se_thres = ifelse(1.96 * h2_se < h2, h2_se, NA))

brain_images <- list.files(path = "../../../../ukb25_slice_summary/", pattern = "*.png")
brain_images <- tibble(
  image = brain_images[c(1:3,5:22)],
  node = as.factor(seq(length(image)))
)

#-- Prepare data -------------------------

ldsc <- merge(ldsc_load, h2_load, by = "node") |> 
  mutate(diagnosis = as.factor(diagnosis),
         node = as.factor(node),
         rg_thres = ifelse(!is.na(h2_thres), rg, NA),
         se_thres = ifelse(!is.na(h2_thres), se, NA)) |> 
  right_join(brain_images, by = "node")

#-- Get significant ------------------------

ldsc %>% 
  mutate(pbonf = p.adjust(p, method = "bonf", n = 21 * length(unique(ldsc$diagnosis)))) %>% 
  filter(pbonf < 0.05) %>% 
  select(node, diagnosis,rg,se,p,pbonf)

ldsc %>% 
  filter(p < 0.05/21) %>% 
  mutate(pbonf = p.adjust(p, method = "bonf", n = 21)) %>% 
  select(node, diagnosis,rg,se,p, pbonf)

#-- Create plot -------------------------

all_plots <- list()

for (i in seq(length(unique(ldsc$diagnosis)))) {
  
  diagname <- levels(ldsc$diagnosis)[i]
  
  print(str_glue("Creating plot for {diagname}"))
  
  plotdata <- ldsc %>%
    filter(diagnosis == diagname) %>%
    arrange(rg_thres)
  
  brain_order <- plotdata %>%
    mutate(node = as.numeric(node)) %>%
    pull(node)
  
  ldscplot <- ggplot(plotdata, aes(x = rg_thres, y = fct_reorder(node, rg_thres),
                                   color = rg_thres)) +
    geom_vline(xintercept = 0, color = "grey30") + 
    geom_pointrange(aes(xmin = rg_thres - se_thres, xmax = rg_thres + se_thres)) +
    geom_point(data = . %>% filter(p < 0.05/21), shape = 8, color = "white") +
    geom_point(data = . %>% filter(p < 0.05/21/7), shape = 8, color = "seagreen1") +
    #geom_segment(aes(x = -1, xend = rg_thres - se_thres - 0.002, yend = reorder(as.factor(node),rg_thres)), 
    #             color = "grey80") +
    labs(title = str_remove_all(diagname,"[:digit:]"),
         x = "Genetic Correlation (r<sub>g</sub>)",
         y = NULL,
         color = "Genetic Correlation",
         size = "Standard Error") +
    scale_x_continuous() +
    scale_y_discrete() +
    scale_color_norment(palette = "berlin", limits = c(-1,1), na.value = "transparent",
                        guide = guide_colorbar(title.position = "top", title.hjust = 0.5, title.vjust = 1, 
                                               barheight = 0.5, barwidth = 10, ticks = FALSE)) +
    theme_norment() +
    theme(
      plot.title.position = "plot",
      #legend.title = element_markdown()
      axis.title.x = element_markdown(),
      plot.margin = margin(t = 5, r = 0, b = 0, l = 5, unit = "pt"),
    )
  
  print(ldscplot)
  
  #-- Add brains
  
  plot <- ldscplot + theme(legend.position = "none")
  pimage_y <- axis_canvas(plot, axis = 'y')
  
  loi <- list.files(path = "../../../../ukb25_slice_summary/", pattern = "*.png")
  loi.good <- loi[c(1:3,5:22)]
  loi.good_y <- loi.good[brain_order]
  
  for (j in seq(length(loi.good_y))) {
    pimage_y <- pimage_y + draw_image(paste0("../../../../ukb25_slice_summary/", 
                                             loi.good_y[j]), y = j - .5, scale = 1.2)
  }
  
  dist <- 8
  ploty <- insert_yaxis_grob(plot, pimage_y, position = "left")
  plotfinal <- ploty
  
  #-- Collect
  
  all_plots[[i]] <- plotfinal
  
}

#-- Put plots together -------------------------

legend <- get_plot_component(
  ldscplot + theme(legend.box.margin = margin(0, 0, 0, 12)),
  'guide-box-bottom',
  return_all = TRUE
)

p <- plot_grid(all_plots[[1]], all_plots[[2]], all_plots[[3]], all_plots[[4]], 
               all_plots[[5]], all_plots[[6]],all_plots[[7]], all_plots[[8]], 
               rel_heights = c(1, 1), ncol = 4
               #labels = str_glue("B.{seq(length(unique(ldsc$diagnosis)))}")
               )

ggsave("ldsc_nodexdiag.png", p, width = 14, height = 8, dpi = 300, bg = "transparent")


