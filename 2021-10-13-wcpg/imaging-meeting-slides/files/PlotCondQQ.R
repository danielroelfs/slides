### SIMULATE PLEIO QQ ########################

#-- Libraries -------------------------

library(tidyverse)
library(normentR)

#-- Load data ------------------------

data <- tibble(
  x = seq(1,3,0.01)
)

#-- Prepare data ------------------------

data_long <- data %>%
  mutate(y = x * 0.5*x + 0.5,
         y1 = x * x,
         y2 = x * y1) %>% 
  pivot_longer(starts_with("y"), names_to = "line", values_to = "y") %>% 
  mutate(line_num = parse_number(line)) %>% 
  replace_na(list(line_num = 0))

#-- Create plot ------------------------

data_long %>% 
  ggplot(aes(x = x, color = line_num, group = line)) +
  geom_line(aes(y = x), size = 2, color = "#333333", 
            lineend = "round", linetype = "dashed") +
  geom_line(aes(y = y), size = 4, lineend = "round") +
  scale_x_continuous(limits = c(NA,3)) + 
  scale_y_continuous(limits = c(1,3)) +
  scale_color_norment(discrete = FALSE, palette = "acton", reverse = TRUE,
                      limits = c(-1,4)) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none",
        axis.line = element_line(size = 2, color = "#333333"),
        plot.margin = margin(10,10,10,10,"pt"))

ggsave("method_pleioqq.png", width = 4, height = 4, bg = "transparent")

