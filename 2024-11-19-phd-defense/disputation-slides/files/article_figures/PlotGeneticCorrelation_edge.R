### PLOT GENETIC CORRELATION (EDGE) ########################

#-- Libraries -------------------------

library(tidyverse)
library(ggtext)
library(cowplot)
library(normentR)

#-- Load data -------------------------

indir <- "~/Dropbox/NORMENT/R_Scripts/network_genetics"

ldsc_load <- read_table(str_glue("{indir}/files/LDSC_stats_edge.txt")) |> 
  mutate(edge = parse_number(edge))

h2_load <- read_table(str_glue("{indir}/files/LDSC_h2_edge.txt")) |> 
  rename(h2_se = se) |> 
  mutate(h2_thres = ifelse(1.96*h2_se < h2, h2, NA),
         h2_se_thres = ifelse(1.96*h2_se < h2, h2_se, NA))

#-- Prepare data -------------------------

edge_def <- get_upper_half_matrix(seq(210), nnodes = 21) |> 
  reshape2::melt() |> 
  filter(!is.na(value)) |> 
  rename(network1 = Var1,
         network2 = Var2,
         edge = value)

ldsc <- merge(ldsc_load, h2_load, by = "edge") |> 
  mutate(rg_thres = ifelse(!is.na(h2_thres), rg, NA),
         se_thres = ifelse(!is.na(h2_thres), se, NA)) |> 
  merge(edge_def, by = "edge")

ldsc_inv <- ldsc |> 
  rename(network1 = network2,
         network2 = network1)

plotdata <- rbind(ldsc, ldsc_inv) |> 
  mutate(diagnosis = factor(diagnosis))

diagonal_plotdat <- tibble(
  network1 = rep(1:21, times = 21),
  network2 = network1
)

bg_plotdat <- tibble(
  network1 = rep(1:21, each = 21),
  network2 = rep(1:21, times = 21),
  na = NA
) |> 
  filter(!(network1 == network2))

alpha_adjust <- ldsc |> 
  filter(!is.na(rg_thres)) |> 
  group_by(diagnosis) |> 
  summarise(n = n()) |> 
  pull(n) |> 
  unique()

alpha_adjust_diag <- ldsc |>  
  pull(GWAS) |> 
  unique() |> 
  length()

#-- Get significant ------------------------

ldsc |>  
  filter(p < 0.05 / alpha_adjust) |>  
  mutate(pbonf = p.adjust(p, method = "bonf", n = alpha_adjust)) |>  
  select(edge, network1, network2, diagnosis, rg, se, p, pbonf)

#-- Create plots -------------------------

all_plots <- list()

for (i in seq(length(unique(plotdata$diagnosis)))) {
  
  diagname <- levels(plotdata$diagnosis)[i]
  
  print(str_glue("Creating plot for {diagname}"))
  
  plotdata_ind <- plotdata |> 
    filter(diagnosis == diagname)
  
  diagname <- ifelse(diagname == "ED", "AN", diagname)
  
  ldscplot <- ggplot(plotdata_ind, aes(x = as.factor(network1), y = as.factor(network2))) +
    geom_point(data = . %>% filter(is.na(rg_thres)), aes(fill = rg_thres), 
               size = 1, shape = 4, fill = "grey30") +
    geom_point(data = . %>% filter(p < 0.05/alpha_adjust), aes(size = se), 
               shape = 22, fill = "grey40", stroke = 1.5) +
    geom_point(data = . %>% filter(!is.na(rg_thres)), aes(fill = rg_thres, size = se), 
               shape = 22, color = "transparent") +
    #geom_point(data = . %>% filter(p.fdr < 0.05), aes(size = se), 
    #           shape = 22, color = "black", stroke = 1.5) +
    geom_point(data = diagonal_plotdat, shape = 4, size = 3) +
    #geom_point(data = . %>% filter(p < 0.05/alpha_adjust), shape = 8, color = "white") +
    geom_point(data = . %>% filter(p < 0.05/alpha_adjust/alpha_adjust_diag), shape = 8, color = "black") +
    labs(title = str_remove_all(diagname,"[:digit:]"),
         x = NULL,
         y = NULL,
         fill = "Genetic Correlation",
         size = "Standard Error") +
    scale_x_discrete(limits = as.factor(1:21), position = "top") +
    scale_y_discrete(limits = rev(as.factor(1:21))) +
    scale_fill_norment(palette = "vik", limits = c(-1,1) ,#* max(abs(plotdata_ind$rg_thres), na.rm = TRUE),
                       na.value = "grey30",
                       guide = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5, 
                                              title.vjust = 1, barheight = 0.5, barwidth = 7, ticks = FALSE)) +
    scale_size_continuous(range = c(1,4), trans = "reverse",
                          guide = guide_legend(order = 2, title.position = "top", label.position = "bottom", nrow = 1,
                                               override.aes = list(fill = "grey80", stroke = 0))) +
    coord_equal() +
    theme_norment(grid = FALSE) +
    theme(
      plot.title.position = "plot",
      legend.box = "horizontal",
      legend.title = element_text(),
      legend.position = "none",
      plot.margin = margin(t = 5, r = 0, b = 0, l = 5, unit = "pt"),
      axis.text.x.top = element_markdown(size = 7, angle = 60, hjust = 0, vjust = 0),
      axis.text.y = element_markdown(size = 7)
    )
  print(ldscplot)
  
  #-- Create plot
  
  plot <- ldscplot
  pimage_x <- axis_canvas(plot, axis = 'x')
  pimage_y <- axis_canvas(plot, axis = 'y')
  
  loi <- list.files(path = "../../../../ukb25_slice_summary/", pattern = "*.png")
  loi.good_x <- loi[c(1:3,5:22)]
  loi.good_y <- rev(loi.good_x)
  
  for (j in seq_along(1:length(loi.good_x))) {
    pimage_x <- pimage_x + draw_image(paste0("../../../../ukb25_slice_summary/", 
                                             loi.good_x[j]), x = j - .5, scale = 1.2)
  }
  
  for (j in seq(length(loi.good_y))) {
    pimage_y <- pimage_y + draw_image(paste0("../../../../ukb25_slice_summary/", 
                                             loi.good_y[j]), y = j - .5, scale = 1.2)
  }
  
  dist <- 6
  plotx <- insert_xaxis_grob(plot, pimage_x, position = "top", height = unit(dist, "mm"))
  #ploty <- insert_yaxis_grob(plot, pimage_y, position = "left")
  plotxy <- insert_yaxis_grob(plotx, pimage_y, position = "left", width = unit(dist, "mm"))
  #theme(plot.background = element_rect(fill="#1e1e1e", color = NA))
  
  plotfinal <- plotxy
  
  #ggsave(str_glue("subfigures/LDSC_edge_{diagname}.png"), plotxy, width = 5, height = 5)

  #-- Collect
  
  all_plots[[i]] <- plotfinal
  
}

#-- Put plots together -------------------------

#legend <- get_legend(
#  plot + theme(legend.box.margin = margin(0, 0, 0, 12))
#)

p <- plot_grid(plotlist = all_plots, ncol = 4)
               #labels = str_glue("A.{seq(alpha_adjust_diag)}"), ncol = 4)

ggsave("ldsc_edgexdiag.png", p, width = 14, height = 8, dpi = 300, bg = "transparent")

