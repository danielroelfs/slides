### LIVE-CODING SESSION ########################
# From the workshop on ggplot on the 7th of February 2020

#-- Libraries ------------------------

library(tidyverse)
library(normentR)

#-- Load data ------------------------

# Loaded in via File > Import Dataset > From Text (readr)

#-- Create plots ------------------------

ggplot(data = syntheticTOPdata, aes(x = Age, y = MemoryList, 
                                    color = Pasient_Kontroll)) +
  geom_point() +
  geom_smooth(method = "lm")

boxplot <- ggplot(data = syntheticTOPdata, aes(x = Pasient_Kontroll, y = IQwasi_2test,
                                    fill = Pasient_Kontroll)) +
  geom_hline(yintercept = 100) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2) +
  labs(x = NULL,
       y = "IQ",
       fill = NULL) +
  scale_fill_norment(discrete = TRUE, palette = "logo") +
  theme_minimal()
print(boxplot)

ggsave("iqplot.png", plot = boxplot,  dpi = 300)


#-- Convert to long format ------------------------

long <- pivot_longer(data = syntheticTOPdata,
                     cols = c(premorbid_nart, vocabulary, 
                              matrix_reasoning, Colornaming)) %>%
  group_by(Pasient_Kontroll, name) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE))

#-- Create more plots ------------------------

ggplot(data = long, aes(x = name, y = mean, 
                        group = Pasient_Kontroll,
                        color = Pasient_Kontroll)) +
  geom_line() +
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd),
                  position = position_dodge(width = 0.25))

# For any questions, come to B48!



