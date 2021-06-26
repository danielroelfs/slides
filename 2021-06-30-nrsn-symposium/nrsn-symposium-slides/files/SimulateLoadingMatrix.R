### SIMULATE IC LOADING MATRIX ########################

#-- Libraries -------------------------

library(tidyverse)
library(ggtext)
library(showtext)
library(normentR)

#-- Generate data ------------------------

font_add_google(name = "Poppins", family = "Poppins")
showtext_auto()

set.seed(21)

data <- simulate_corr_matrix(10) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  rownames_to_column("question") %>% 
  mutate(question = as.numeric(question)) %>% 
  select(1:6) %>% 
  pivot_longer(starts_with("v"), names_to = "ic", values_to = "loading") %>% 
  replace_na(list(loading = mean(.$loading, na.rm = TRUE)))

ic_names <- tibble(
  ic_num = seq(5),
  ic = str_glue("IC{ic_num}"),
  color = c("#9874b2","#ef87b5","#68aee1","#96c9bf","#e7e783")
)

#-- Create plot ------------------------

data %>% 
  rename(question_no = question) %>% 
  mutate(question = str_glue("Q{question_no}"),
         question_label = ifelse(question_no == max(question_no), "Q<sub>n</sub>", question),
         question_label = fct_relevel(question_label, c(str_glue("Q{seq(9)}"), "Q<sub>n</sub>")),
         question_label = fct_rev(question_label),
         ic = str_replace_all(ic,"v","IC")) %>%
  left_join(ic_names) %>% 
  mutate(ic_label = str_glue("<span style='color: {color}'>{ic}</span>")) %>% 
  ggplot(aes(x = reorder(ic_label,ic_num), y = question_label, fill = ic, size = loading)) + 
  geom_point(shape = 22, color = "transparent") + 
  labs(x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("#9874b2","#ef87b5","#68aee1","#96c9bf","#e7e783")) +
  coord_equal() +
  theme_void() +
  theme(text = element_text(family = "Poppins"),
        axis.text.x = element_markdown(face = "bold", size = 24),
        axis.text.y = element_markdown(color = "#333333", face = "bold", size = 24),
        panel.grid.major = element_blank(),
        plot.margin = margin(0,5,0,5, unit = "pt"),
        legend.position = "none")

#-- Save plot ------------------------

ggsave("simulated_loading_matrix.png", width = 600, height = 1000, units = "px", dpi = 300)



