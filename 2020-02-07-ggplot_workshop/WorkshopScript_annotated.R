### LIVE-CODING SESSION (ANNOTATED) ########################
# From the workshop on ggplot on the 7th of February 2020

#-- Libraries ------------------------

library(tidyverse) # Load tidyverse package (which includes ggplot2)
library(normentR) # Load normentR package, not available on some computers

#-- Load data ------------------------

syntheticTOPdata <- read_delim("/Users/danielroelfs/Downloads/syntheticTOPdata.csv", delim = ";",
                               escape_double = FALSE, trim_ws = TRUE)

#-- Create plots ------------------------

ggplot(data = syntheticTOPdata, aes(x = Age, y = MemoryList,
                                    color = Pasient_Kontroll)) + # color based on Pasient_Kontroll
  geom_point() + # Add scatter points
  geom_smooth(method = "lm") # Add regression line, with method = "lm" meaning "linear model"

# <- means "assign to", so the ggplot object is "assigned to" a variable called "boxplot"
boxplot <- ggplot(data = syntheticTOPdata, aes(x = Pasient_Kontroll, y = IQwasi_2test,
                                               fill = Pasient_Kontroll)) +
  geom_hline(yintercept = 100) + # Add horizontal line at y=100
  geom_violin(alpha = 0.5) + # Add vertical density layer with transparence of 0.5
  geom_boxplot(width = 0.2) + # Add boxplot layer with width 0.2 to make them thinner
  labs(x = NULL, # Remove x-axis title
       y = "IQ", # Set y-axis title to "IQ"
       fill = NULL) + # Remove fill legend title
  scale_fill_norment(discrete = TRUE, palette = "logo") + # Use NORMENT colors, 
                                  #Pasient_Kontroll is discrete, so discrete = TRUE
  theme_minimal() # Set a different theme

print(boxplot) # Show the boxplot

ggsave("iqplot.png", plot = boxplot,  dpi = 300) # Save the boxplot


#-- Convert to long format ------------------------

# Use the pivot_longer function to collapse some variables in 1 column called
# "name" and the corresponding in a column called "value"
# %>% is called the "pipe" and it basically says "take the output from this 
# function and use it in the next function". In this case: "take the output from
# the "pivot_longer()" function, and use it in the "group_by()" function. And
# the second one after the "group_by()" function basically says: "take the
# output from the "pivot_longer()" and the "group_by()" function and use it in 
# the "summarise()" function.

long <- pivot_longer(data = syntheticTOPdata, # Transform this dataset
                     cols = c(premorbid_nart, vocabulary,
                              matrix_reasoning, Colornaming)) %>%
  group_by(Pasient_Kontroll, name) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE))

# In human terms, the code above transforms the "syntheticTOPdata" dataset into
# long format. The "premorbid_nart", "vocabulary", "matrix_reasoning", and 
# "Colornaming" columns will be put underneath each other in a column called
# "name", the corresponding values are put in a column called "values". You can
# see these by clicking on the "long" variable among the variables on the right.
#
# Then it takes this long-format dataset, and groups it by Pasient_Kontroll, and
# by the values in the "name" column. This means that in the following function,
# the "summarise()" function, all operations are performed by group. So
# calculating the mean for the values on a grouped dataset, will give means for
# each cognitive test in the "name" column, and then also per group in the 
# "Pasient_Kontroll" group. Since we used 4 tests in the "name" column, and
# there's 2 groups in the "Pasient_Kontroll", we\ll get 8 different mean values.
# We also calculate standard deviation by the "sd()" function.
# The "na.rm = TRUE" is necessary to tell R to ignore missing data. If this
# was set to "na.rm = FALSE", then R would say: "I don't have data on some of 
# these participants, so I can't calculate the average" and output "NA".

#-- Create more plots ------------------------

# Now we're going to take our long-format dataset and make a plot with the
# different cognitive tests on the x-axis.

ggplot(data = long, aes(x = name, y = mean, 
                        group = Pasient_Kontroll,
                        color = Pasient_Kontroll)) +
  geom_line() +
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd),
                  position = position_dodge(width = 0.25))

# The "geom_pointrange()" function plots a range on the y-axis, in this case
# we want the lower bound of the range to represent the mean value minus the 
# standard deviation for each test. The top bound of the range represents the 
# mean value plus the standard deviation.

#-- ggplot template ------------------------

#ggplot(data = <your data>, aes(x = <what you want on the x-axis>,
#                               y = <what you want on the y-axis,
#                               fill = <what you want the color to represent>,
#                               linetype = <idem>, size = <idem>)) +
#  geom_<any geom>() +
#  geom_<any other geom>() +
#  geom_<any third geom>() +
#  labs(x = "x-axis title",
#       y = "y-axis title",
#       fill = "title of fill aesthetic",
#       linetype = "title of linetype aestetic") +
#  scale_<aesthetic>_continuous() + # if you have a continuous aestetic
#  scale_<aesthetic>_discrete() + # if you have another discrete aethetic
#  scale_<aesthetic>_manual() + # theme
#  theme_<any theme>() + # set theme
#  theme() # set specific theme element properties


# For any questions, come to B48!


