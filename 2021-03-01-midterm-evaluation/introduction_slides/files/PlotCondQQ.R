### PLOT PLEIOFDR CONJUNCTIONAL FDR PLOT ########################

#-- Libraries -------------------------

library(tidyverse)

#-- Generate data ------------------------

data <- data.frame(
  x = seq(1,3,0.01)
)

data <- data %>%
  mutate(y = x * x,
         y1 = x * y,
         y2 = x * 0.5*x + 0.5)


#-- Create plot ------------------------

main_color <- "#ffffff"

ggplot(data, aes(x = x)) +
  geom_line(aes(y = x), size = 3, color = main_color, linetype = "dotted", lineend = "round") +
  geom_line(aes(y = y), size = 3, color = main_color, lineend = "round") +
  geom_line(aes(y = y1), size = 3, color = main_color, lineend = "round") +
  geom_line(aes(y = y2), size = 3, color = main_color, lineend = "round") +
  lims(x = c(NA,3),
       y = c(1,3)) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.line.x = element_line(color = main_color, size = 2),
    axis.line.y = element_line(color = main_color, size = 2),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )
ggsave("~/Dropbox/University/PhD/210301 Mid-term evaluation/introduction_slides/files/condqq.png", width = 10, height = 10, units = "cm", bg = "transparent")

