### TITLE ########################

#-- Libraries -------------------------

library(tidyverse)
library(normentR)

#-- Load data ------------------------

set.seed(16)
gwas.dat <- simulateGWAS(nSNPs = 1e5, nSigCols = 20, 
                         SigCHR = c(seq(17),sample(seq(10), 3, replace = TRUE)))

#-- Get parameters ------------------------

nCHR <- length(unique(gwas.dat$CHR))
gwas.dat$BPcum <- NA
s <- 0
nbp <- c()
for (i in unique(gwas.dat$CHR)){
  nbp[i] <- max(gwas.dat[gwas.dat$CHR == i,]$BP)
  gwas.dat[gwas.dat$CHR == i,"BPcum"] <- gwas.dat[gwas.dat$CHR == i,"BP"] + s
  s <- s + nbp[i]
}

axis.set <- gwas.dat %>% 
  group_by(CHR) %>% 
  summarize(center = (max(BPcum) + min(BPcum)) / 2)
ylim <- abs(floor(log10(min(gwas.dat$P)))) + 2 
ylim <- 16
sig <- 5e-8

#-- Create plot ------------------------

default_color <- "#EEEEEE"
accent_color <- "#68aee1"

manhplot <- ggplot(gwas.dat %>% filter(P < 1), aes(x = BPcum, y = -log10(P), size = -log10(P))) +
  geom_point(aes(color = ifelse(P > sig, "nsig", "sig")), alpha = 0.75) +
  scale_x_continuous(label = axis.set$CHR, breaks = axis.set$center) +
  scale_y_continuous(expand = c(0,0), limits = c(-1, ylim)) +
  scale_color_manual(values = c(default_color, accent_color)) + 
  scale_size_continuous(range = c(1,4)) +
  labs(x = NULL, 
       y = NULL) + 
  theme_norment(grid_col = "transparent") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = grid::unit(c(0,0,0,0), "mm")
  )
print(manhplot)
ggsave("manhattan_scz.png", manhplot, width = 10, height = 6, dpi = 600, units = "cm", bg = "transparent")
