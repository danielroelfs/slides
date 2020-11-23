
library(tidyverse)
library(ggpubr)
library(normentR)

data <- read_csv2("syntheticdataset.csv")

ggplot(data, aes(x = reorder(Diag_category,IQwasi_2test), y = IQwasi_2test, fill = Gender)) +
  geom_double_violin()

iqdata <- data %>%
  mutate(Diag_category = fct_explicit_na(Diag_category, "control"),
         diagnosis = fct_recode(Diag_category, 
                                HC = "control",
                                BD1 = "bipolar I",
                                BD2 = "bipolar II",
                                BDNOS = "bipolarNOS",
                                SCZ = "schizophrenia",
                                SCZaffective = "schizoaffective",
                                SCZform = "schizophreniform",
                                MDD = "major depressive disorder",
                                OTHER = "other psychosis"),
         diagnosis = fct_relevel(diagnosis, "HC", "BD1", "BD2", "BDNOS", "SCZ",
                                 "SCZaffective", "SCZform", "MDD", "OTHER"))

diag_colordef <- c(SCZ = norment_colors[["blue"]], 
                   SCZaffective = norment_colors[["light blue"]],
                   SCZform = "blue",
                   BD1 = norment_colors[["purple"]],
                   BD2 = norment_colors[["light purple"]],
                   BDNOS = norment_colors[["magenta"]],
                   MDD = norment_colors[["green"]],
                   OTHER = norment_colors[["yellow"]],
                   HC = norment_colors[["light grey"]])

ggplot(iqdata, aes(x = diagnosis, y = IQwasi_2test, fill = diagnosis)) +
  geom_hline(yintercept = 100, color = "grey") +
  geom_violin() +
  geom_boxplot(outlier.shape = 4, width = 0.15, fill = "white") +
  stat_compare_means(method = "anova") +
  scale_fill_manual(values = diag_colordef) +
  theme_norment(legend = FALSE) +
  facet_wrap(~ Gender)

ggplot(data, aes(x = Age, y = IQwasi_2test)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(color = norment_colors["purple"], method = "lm") +
  ggpubr::stat_cor() +
  facet_wrap(~ Gender)

long <- data %>%
  pivot_longer(c(WM_LNS,Symbolcoding,CatFluency,LearningList,MemoryList,RecognitionList,
                 Colornaming,Reading,Inhibition,InhibitionControl)) %>%
  group_by(name) %>%
  mutate(value_noo = ifelse(value > 3 | value < -3, NA, value),
         se_noo = sem(value_noo),
         Diag_category = fct_explicit_na(Diag_category,"control"))

scz_bd <- filter(long, Diag_category %in% c("schizophrenia", "bipolar I", "bipolar II","control"))

bd <- filter(long, Diag_category %in% c("bipolar I", "bipolar II"))
bd <- mutate_if(bd, is.character, as.factor)

plotdata <- scz_bd %>%
  group_by(Diag_category, name) %>%
  summarise(zscore = mean(value_noo, na.rm = TRUE),
            sem = sem(value_noo, na.rm = TRUE),
            sd = sd(value_noo, na.rm = TRUE),
            n = n()) %>%
  ungroup() %>%
  mutate_if(is.character, as.factor)


ggplot(plotdata, aes(x = name, y = zscore, color = Diag_category)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_line(aes(group = Diag_category), position = position_dodge(width = 0.4), size = 1) +
  geom_linerange(aes(ymin = zscore - sd, ymax = zscore + sd), 
                  position = position_dodge(width = 0.4), size = 1) +
  geom_point(size = 4, shape = 18, position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c(norment_colors[["purple"]],norment_colors[["magenta"]],
                                norment_colors[["blue"]],"grey40"))
