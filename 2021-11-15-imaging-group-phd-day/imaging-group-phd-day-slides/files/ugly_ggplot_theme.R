### TITLE ########################

#-- Libraries -------------------------

library(tidyverse)

#-- Sample data for plot ------------------------

points <- tibble(
  x = rep(1:10,3), 
  y = rep(1:10,3), 
  z = sort(rep(letters[1:2], 15)),
  w = rep(letters[3:4], 15)
)

#-- ggplot using many theme options ------------------------

ggplot(points, aes(x = x, y = y, col = as_factor(x))) + 
  geom_point(size = 5) + 
  labs(title = "test title",
       subtitle = "test subtitle",
       x = "my x axis",
       y = "my y axis",
       caption = "this is a caption",
       col = "Renamed Legend") +
  facet_grid(w ~ z, switch = "y") +
  theme(
    plot.background = element_rect(fill = "lightyellow"),
    plot.title = element_text(size = 30, hjust = 0.25),
    plot.subtitle = element_text(size = 20, hjust = 0.75, color = "mediumvioletred", family = "serif"),
    plot.caption = element_text(size = 10, face = "italic", angle = 25),
    
    panel.background = element_rect(fill = "lightblue", colour = "darkred", size = 4),
    panel.border = element_rect(fill = NA, color = "green", size = 2),
    panel.grid.major.x = element_line(color = "purple", linetype = 2),
    panel.grid.minor.x = element_line(color = "orange", linetype = 3),
    panel.grid.minor.y = element_blank(),
    
    axis.title.x = element_text(face = "bold.italic", color = "blue"),
    axis.title.y = element_text(family = "mono", face = "bold", size = 20, hjust = 0.25),
    axis.text = element_text(face = "italic", size = 15),
    axis.text.x.bottom = element_text(angle = 180), # note that axis.text options from above are inherited
    
    strip.background = element_rect(fill = "magenta"),
    strip.text.y = element_text(color = "white"),
    strip.placement = "outside",
    
    legend.background = element_rect(fill = "orangered4"), # generally will want to match w plot background
    legend.key = element_rect(fill = "orange"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.justification = "left",
    legend.title = element_text(family = "serif", color = "white"),
    legend.text = element_text(family = "mono", face = "italic", color = "limegreen")
  )

#-- Save plot ------------------------

ggsave(plot = last_plot(), filename = "ugly_ggplot_theme.png", width = 10, height = 6)