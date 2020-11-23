
library(tidyverse)
#library(bnstruct)

data <- read_csv2("syntheticdataset.csv")

data_clean <- data %>%
  filter(IQwasi_2test >= 70)

#data_imp <- knn.impute(as.matrix(data_clean), k = 3)
  
data %>%
  group_by(Diag_category) %>%
  summarise(n = n())

set.seed(12)

data_clean <- data %>%
  filter(IQwasi_2test >= 70) %>%
  slice(sample(1000, replace = FALSE))

data_clean %>%
  group_by(Diag_category) %>%
  summarise(n = n())

write.table(data_clean, file = "syntheticdataset_clean.csv", quote = FALSE, sep = ";")



