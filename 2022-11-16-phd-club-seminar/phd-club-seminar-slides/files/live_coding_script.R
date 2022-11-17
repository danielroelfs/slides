### PHD CLUB SEMINAR LIVE CODING ########################

#-- Libraries -------------------------

library(tidyverse)
library(ggtext)
library(scico)

#-- Load data ------------------------

palmerpenguins::penguins |> 
    ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = body_mass_g)) + 
    geom_point(alpha = 0.6, size = 1.5) + 
    labs(title = "Bill size of brush-tailed penguins _Pygoscelis_",
         caption = "Data: Gorman, Williams & Fraser (2014), _PLoS ONE_", 
         x = "**Bill length** (mm)",
         y = "**Bill depth** (mm)",
         color = "**Body mass** (g)") +
    scale_x_continuous(limits = c(30, 60)) + 
    scale_y_continuous(limits = c(12, 24)) +
    scale_color_scico(palette = "bamako", direction = -1, 
                      guide = guide_colorbar(title.position = "top", title.hjust = .5, 
                                             ticks = FALSE, 
                                             barwidth = 15, barheight = 0.5)) +
    coord_cartesian(expand = FALSE, clip = "off") +
    theme_minimal() +
    theme(plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5),
          plot.caption = element_markdown(margin = margin(t = 15)), 
          #plot.caption.position = "right",
          legend.position = "top",
          legend.title = element_markdown(),
          axis.title.x = element_markdown(),
          axis.title.y = element_markdown(),
          plot.margin = margin(15, 15, 5, 15))
