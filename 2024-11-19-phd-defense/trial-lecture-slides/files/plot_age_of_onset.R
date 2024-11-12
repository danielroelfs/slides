### PLOT AGE OF ONSET ########################

#-- Libraries -------------------------

library(tidyverse)
library(ggtext)

#-- Load data ------------------------

data_aoo <- tibble(
    disorder = c(
        "Any mental disorder", "Neurodevelopmental disorders", "Autism spectrum disorder", "Attention deficit hyperactivity disorder", 
        "Anxiety and fear-related disorders", "Specific phobia/separation anxiety disorder", "Social anxiety disorder", "Panic disorder", 
        "Generalized anxiety disorder", "Obsessive-compulsive related disorders", "Obsessive-compulsive disorder", "Feeding or eating disorders", 
        "Anorexia nervosa", "Bulimia Nervosa", "Binge eating disorder", "Trauma-related disorders", "Post-traumatic stress disorder", 
        "Disorders due to substance use or addictive behaviors", "Disorder due to use of cannabis", "Disorder due to use of alcohol", 
        "Schizophrenia-spectrum and primary psychotic disorders", "Schizophrenia", "Acute and transient psychotic disorder", "Personality disorders", 
        "Mood disorders", "Depressive disorders", "Bipolar or related disorders"
    ),
    num_samples = c(14, 21, 2, 12, 73, 22, 42, 22, 24, 20, 20, 11, 8, 8, 5, 16, 16, 58, 10, 44, 36, 25, 2, 6, 79, 62, 40),
    peak_age_onset = c(14.5, 5.5, 5.5, 9.5, 5.5, 5.5, 14.5, 15.5, 15.5, 14.5, 14.5, 15.5, 15.5, 15.5, 19.5, 15.5, 15.5, 19.5, 19.5, 19.5, 20.5, 20.5, 18.5, 20.5, 20.5, 19.5, 19.5),
    prop_onset_by_14 = c(34.6, 61.5, 72.4, 56.8, 38.1, 72.4, 50.9, 8.2, 8.6, 24.6, 24.6, 15.8, 18.2, 16.0, 12.3, 16.9, 16.9, 2.9, 3.2, 4.2, 3.0, 2.0, 1.8, 1.9, 2.5, 3.1, 5.1),
    prop_onset_by_18 = c(48.4, 83.2, 89.8, 73.0, 51.8, 75.0, 79.1, 22.5, 20.4, 45.1, 45.1, 48.1, 55.2, 45.3, 34.5, 27.6, 27.6, 15.2, 17.5, 18.3, 12.3, 8.2, 6.6, 9.6, 11.5, 13.2, 13.7),
    prop_onset_by_25 = c(62.5, 95.8, 94.8, 91.8, 73.3, 80.4, 87.5, 45.7, 33.0, 64.0, 64.0, 82.4, 78.7, 82.9, 73.5, 43.1, 43.1, 48.8, 64.6, 44.8, 47.8, 47.4, 20.6, 47.7, 34.5, 36.9, 32.0),
    p25 = c(11, 7, 5, 8, 9, 5, 9, 18, 20, 14, 14, 15, 14, 15, 16, 17, 17, 20, 19, 19, 20, 21, 27, 20, 21, 21, 22),
    median = c(18, 12, 9, 12, 17, 8, 13, 26, 32, 19, 19, 18, 17, 18, 20, 30, 30, 25, 22, 27, 25, 25, 35, 25, 31, 30, 33),
    p75 = c(34, 16, 14, 18, 25, 17, 17, 36, 42, 29, 29, 23, 22, 22, 25, 48, 48, 41, 29, 43, 34, 35, 45, 33, 46, 44, 49)
)

#-- Plot age of onset ------------------------

data_aoo |> 
    ggplot(aes(x = median, y = reorder(disorder, -median))) +
    geom_col(fill = "#e67467", width = 0.5) +
    geom_vline(xintercept = 0, linewidth = 1) +
    geom_errorbarh(
        aes(
            xmin = p25,
            xmax = p75
        ),
        height = 0.25
    ) +
    labs(
        title = "Epidemiological estimates of age at onset of mental disorders",
        x = "Age (years)",
        y = NULL,
        caption = "Error bars incidate 25th and 75th percentile<br />Different metric on the same sample has been extracted from multiple studies."
    ) +
    scale_x_continuous(
        position = "top",
        expand = expansion(add = c(0, NA))
    ) +
    theme_minimal() +
    theme(
        text = element_text(family = "Source Sans"),
        axis.text.y = element_markdown(face = "bold"),
        axis.title = element_markdown(),
        plot.title.position = "plot",
        plot.title = element_markdown(size = 16, face = "bold"),
        plot.caption = element_markdown(),
        panel.grid = element_line(color = "#dddddd"),
        panel.grid.major.y = element_blank(),
    )

ggsave("files/age_of_onset_disorders.png", width = 9, height = 4.5)


