### SIMULATE LDSC MATRIX ########################

#-- Libraries -------------------------

library(tidyverse)
library(ggtext)
library(showtext)
library(normentR)

#-- Generate data ------------------------

font_add_google(name = "Poppins", family = "Poppins")
showtext_auto()

set.seed(16)

data <- simulate_corr_matrix(5) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  rownames_to_column("ic_1") %>% 
  mutate(ic_1 = as.numeric(ic_1)) %>% 
  pivot_longer(starts_with("v"), names_to = "ic_2", values_to = "loading") %>% 
  mutate(ic_1_num = ic_1,
         ic_2_num = parse_number(ic_2),
         ic_1 = str_glue("IC{ic_1_num}"),
         ic_2 = str_glue("IC{ic_2_num}"))

ic_names <- tibble(
  ic_num = seq(5),
  color = c("#9874b2","#ef87b5","#68aee1","#96c9bf","#e7e783")
)

#-- Prepare data ------------------------

plotdata <- data %>% 
  left_join(ic_names, by = c("ic_1_num" = "ic_num")) %>% 
  rename(color_ic_1 = color) %>% 
  left_join(ic_names, by = c("ic_2_num" = "ic_num")) %>% 
  rename(color_ic_2 = color) %>% 
  mutate(ic_1_label = str_glue("<span style='color: {color_ic_1}'>{ic_1}</span>"),
         ic_2_label = str_glue("<span style='color: {color_ic_2}'>{ic_2}</span>"))

diag_data <- plotdata %>% 
  filter(is.na(loading))

#-- Create plot ------------------------

plotdata %>% 
  ggplot(aes(x = reorder(ic_1_label,ic_1_num), y = reorder(ic_2_label,ic_2_num), fill = loading * 2.5)) + 
  geom_tile(color = "transparent") + 
  geom_point(data = diag_data, shape = 4, size = 3, color = "#333333") +
  labs(x = NULL,
       y = NULL) +
  scale_x_discrete(position = "top") +
  scale_y_discrete() +
  scale_fill_norment(palette = "vik", limits = c(-1,1), na.value = "transparent") +
  coord_equal() +
  theme_void() +
  theme(text = element_text(family = "Poppins"),
        axis.text.x = element_markdown(face = "bold", size = 48),
        axis.text.y = element_markdown(color = "#333333", face = "bold", size = 48),
        panel.grid.major = element_blank(),
        plot.margin = margin(0,5,0,5, unit = "pt"),
        legend.position = "none")

#-- Save plot ------------------------

ggsave("simulated_ldsc_matrix.png", width = 1200, height = 1200, units = "px", dpi = 600)
