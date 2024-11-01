### LDSC CORRELATION MATRIX IC x IC (ILLUSTRATOR) ########################

#-- Libraries -------------------------

library(tidyverse)
library(normentR)

#-- Prologue ------------------------

indir <- "~/Dropbox/NORMENT/R_Scripts/ICA_genetics"

#-- Load data ------------------------

ldsc <- read.table(str_glue("{indir}/files/LDSC_stats_ICxIC.txt"), header = TRUE, stringsAsFactors = FALSE) |> 
  mutate(p.fdr = p.adjust(p, method = "fdr"),
         p.bonf = p.adjust(p, method = "bonferroni"))

ica_loadings <- read_csv(str_glue("{indir}/files/ica_loadings_plink.txt"))

ic_defs <- read.table(str_glue("{indir}/files/IC_names.txt"), header = TRUE, sep = "\t", stringsAsFactors = FALSE) |> 
  mutate(IC_new = paste0("IC",parse_number(comp_label)))

ic_trans <- ic_defs |> 
  rename(IC_old = component) |> 
  select(IC_old,IC_new)

#-- Run phenotypic correlation -------------------------

phenocor <- tibble()
for (x in seq(nrow(ic_defs))) {
  for (y in seq(x,nrow(ic_defs))) {
    
    #print(sprintf("Calculating correlation between IC%s and IC%s",x,y))
    r <- nrow(phenocor) + 1
    
    phenocor[r,"ICx"] <- paste0("IC",x)
    phenocor[r,"ICy"] <- paste0("IC",y)
    phenocor[r,"cor"] <- cor.test(ica_loadings |>  pull(paste0("IC",x)),
                                  ica_loadings |>  pull(paste0("IC",y)))$estimate
    phenocor[r,"p"] <- cor.test(ica_loadings |>  pull(paste0("IC",x)),
                                ica_loadings |>  pull(paste0("IC",y)))$p.value
  }
}
rm(list = c("x","y","r"))

#-- Prepare LDSC data ------------------------

ldsc_new <- ldsc |> 
  select(-c(p1,p2,z,p,p.bonf),-starts_with("h2"),-starts_with("gcov")) |> 
  inner_join(ic_trans, by = c("ICx" = "IC_old")) |> 
  rename(IC_new_x = IC_new) |> 
  inner_join(ic_trans, by = c("ICy" = "IC_old")) |> 
  rename(IC_new_y = IC_new) |> 
  mutate(x = parse_number(IC_new_x),
         y = parse_number(IC_new_y))

for (i in seq(nrow(ldsc_new))) {
  if (ldsc_new[i,"x"] > ldsc_new[i,"y"]) {
    ldsc_new[i,"new_x"] <- ldsc_new[i,"y"]
    ldsc_new[i,"new_y"] <- ldsc_new[i,"x"]
  } else {
    ldsc_new[i,"new_x"] <- ldsc_new[i,"x"]
    ldsc_new[i,"new_y"] <- ldsc_new[i,"y"]
  }
}

plotdata_ldsc <- ldsc_new |> 
  select(-starts_with("IC_")) |> 
  mutate(ICx_new = paste0("IC",new_x),
         ICy_new = paste0("IC",new_y)) |> 
  inner_join(ic_defs, by = c("ICx_new" = "IC_new")) |> 
  rename(comp_label_x = comp_label) |> 
  inner_join(ic_defs, by = c("ICy_new" = "IC_new")) |> 
  rename(comp_label_y = comp_label) |> 
  mutate(rg_inv = rg * trans.x * trans.y)

#-- Prepare phenotypic data ------------------------

phenocor_new <- phenocor |> 
  filter(!(ICx == ICy)) |> 
  inner_join(ic_trans, by = c("ICx" = "IC_old")) |> 
  rename(IC_new_x = IC_new) |> 
  inner_join(ic_trans, by = c("ICy" = "IC_old")) |> 
  rename(IC_new_y = IC_new) |> 
  mutate(x = parse_number(IC_new_x),
         y = parse_number(IC_new_y))

for (i in seq(nrow(phenocor_new))) {
  if (phenocor_new[i,"x"] > phenocor_new[i,"y"]) {
    phenocor_new[i,"new_x"] <- phenocor_new[i,"y"]
    phenocor_new[i,"new_y"] <- phenocor_new[i,"x"]
  } else {
    phenocor_new[i,"new_x"] <- phenocor_new[i,"x"]
    phenocor_new[i,"new_y"] <- phenocor_new[i,"y"]
  }
}

plotdata_phenocor <- phenocor_new |> 
  select(-starts_with("IC_")) |> 
  mutate(ICx_new = paste0("IC",new_x),
         ICy_new = paste0("IC",new_y)) |> 
  inner_join(ic_defs, by = c("ICx_new" = "IC_new")) |> 
  rename(comp_label_y = comp_label) |> 
  inner_join(ic_defs, by = c("ICy_new" = "IC_new")) |> 
  rename(comp_label_x = comp_label) |> 
  mutate(cor_inv = cor * trans.x * trans.y,
         rg_inv = cor_inv) 

#-- Final preparations ------------------------

gridlines <- tibble(
  x = seq(1.5, 12.5, 1),
  y = seq(1.5, 12.5, 1)
)

limits <- ic_defs |> 
  arrange(parse_number(comp_label)) |> 
  pull(comp_label)

#-- Create plot ------------------------

ggplot(plotdata_ldsc, aes(x = comp_label_x, y = comp_label_y, fill = rg_inv)) +
  # LDSC
  geom_point(shape = 22, aes(size = se), color = "transparent") +
  geom_point(data = data.frame(comp_label_x = limits, 
                               comp_label_y = limits), 
             size = 16, shape = 22, fill = "grey50", color = "transparent") +
  geom_point(data = . %>% filter(p.fdr < 0.05), aes(size = se), 
             shape = 22, color = "black", stroke = 1.5) +
  geom_text(aes(label = round(rg_inv,2)), size = 3) +
  # Pheno
  geom_tile(data = plotdata_phenocor, size = 0.9) +
  geom_text(data = plotdata_phenocor %>% filter(!is.na(rg_inv)), 
            aes(label = formatC(rg_inv, format = "e", digits = 0)), 
            size = 3) +
  # Add grid lines
  geom_segment(data = gridlines, aes(x = x, xend = x, y = 0.5, yend = y), 
               inherit.aes = FALSE, color = "grey80", linewidth = 0.2) +
  geom_segment(data = gridlines, aes(x = x, xend = 13.5, y = y, yend = y), 
               inherit.aes = FALSE, color = "grey80", linewidth = 0.2) +
  #geom_point(data = plotdata_phenocor, shape = 22, color = "transparent") +
  #geom_point(data = data.frame(comp_label_x = limits, 
  #                             comp_label_y = limits), 
  #           size = 16, shape = 22, fill = "grey50", color = "transparent") +
  #geom_point(data = plotdata_phenocor |>  filter(p.fdr < 0.05), 
  #           shape = 22, color = "black", stroke = 1.5) +
  labs(x = NULL,
       y = NULL,
       #fill = bquote("Genetic"~(r[g])~"or Phenotypic Correlation"),
       #fill = bquote("Genetic Correlation"~(r[g])),
       fill = "Phenotypic Correlation (r)",
       size = bquote("Standard Error of"~r[g])) +
  scale_x_discrete(position = "top", limits = limits) +
  scale_y_discrete(limits = limits) +
  # Genetic correlation legend
  #scale_fill_norment(discrete = FALSE, palette = "vik", limits = c(-1,1),
  #                   guide = guide_colorbar(order = 1, nbin = 100, label.theme = element_text(angle=90),
  #                                          label.hjust = 0.5, label.vjust = 0.5, title.theme = element_text(angle = 180),
  #                                          barwidth = 15, barheight = 0.75, ticks = FALSE, reverse = TRUE,
  #                                          title.position = "bottom", label.position = "top")) +
  # Phenotypic correlation legend
  scale_fill_norment(discrete = FALSE, palette = "vik", limits = c(-1, 1),
                     guide = guide_colorbar(order = 1, nbin = 100, label.theme = element_text(angle=270),
                                            label.hjust = 0.5, label.vjust = 0.5, title.theme = element_text(angle = 180),
                                            barwidth = 15, barheight = 0.75, ticks = FALSE, reverse = FALSE,
                                            title.position = "bottom", label.position = "top")) +
  scale_size_continuous(range = c(4,14), trans = "reverse",
                        #guide = FALSE) +
                        guide = guide_legend(order = 2, title.position = "bottom", override.aes = list(fill = "grey80"),
                                             label.theme = element_text(angle=90), title.theme = element_text(angle = 180),
                                             label.hjust = 0.5, label.vjust = 0.5, label.position = "left")) +
  coord_equal() +
  theme_norment(base_size = 12, axis_title_size = 14, grid = FALSE) +
  theme(
    legend.position = "bottom",
    legend.title.align = 0.5,
    axis.text.y = element_text(margin = margin(0,50,0,0)),
    #axis.text = element_text(colour = "black"),
    axis.text.x = element_text(hjust = 0, angle = 30),
    plot.margin = unit(c(1, 4, 1, 1), "cm")
  )

ggsave("ICxIC_raw.png", height = 9.5, width = 11.5, dpi = 600, bg = "transparent")

