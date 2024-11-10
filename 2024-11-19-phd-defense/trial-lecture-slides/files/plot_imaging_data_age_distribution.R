### PLOT AGE DISTRIBUTION OF IMAGING DATASETS ########################

#-- Libraries -------------------------

library(tidyverse)
library(ggtext)

#-- Load data ------------------------

data_imaging_age <- read_csv("files/Table_1_9_bankssts.csv") |> 
    janitor::clean_names() |> 
    group_by(study) |> 
    summarise(
        weighted_mean_age = weighted.mean(mean_age_in_days, sample_size),
        min_age = min(minimum_age_in_days),
        max_age = max(maximum_age_in_days),
        sample_size = sum(sample_size)
    ) |> 
    mutate(
        across(contains("age"), ~ .x / 365.25),
        log = log2(sample_size)
    )

#-- Create barplot ------------------------

data_imaging_age |> 
    ggplot(aes(x = reorder(study, weighted_mean_age), y = weighted_mean_age, fill = log)) +
    geom_col(width = 0.5) +
    geom_hline(yintercept = 0, linewidth = 1) +
    labs(
        title = "Average age within common imaging datasets",
        x = NULL,
        y = "Age (years)",
        fill = "Sample size (log~2~-transformed)"
    ) +
    scale_y_continuous(
        limits = c(0, 82),
        expand = expansion(add = c(0, NA))
    ) +
    scale_fill_gradient(
        low = "white", high = "#e67467",
        limits = c(0, NA),
        breaks = seq(2, 20, 4),
        guide = guide_colorbar(
            barwidth = 0.8, barheight = 8, ticks.colour = NA, limits = c(0, NA)
        )
    ) +
    theme_minimal() +
    theme(
        legend.position = "inside",
        legend.position.inside = c(0.15, 0.6),
        legend.title = element_markdown(hjust = 0.5),
        text = element_text(family = "Source Sans"),
        axis.text.x = element_markdown(angle = 60, hjust = 1),
        axis.title = element_markdown(),
        plot.title.position = "plot",
        plot.title = element_markdown(size = 16, face = "bold"),
        plot.caption = element_markdown(),
        panel.grid = element_line(color = "#dddddd"),
        panel.grid.major.x = element_blank(),
    )

ggsave("files/imaging_age_distribution.png", width = 9, height = 4.5)

#-- Create Gapminder plot ------------------------

data_imaging_age |> 
    ggplot(aes(x = min_age, y = max_age, size = sample_size)) + 
    geom_hline(yintercept = 0, linewidth = 1, color = "grey20") +
    geom_vline(xintercept = 0, linewidth = 1, color = "grey20") +
    geom_point(color = "#e67467") +
    ggrepel::geom_text_repel(aes(label = study), size = 3) +
    labs(
        title = "Age within common imaging datasets",
        x = "Minimum age (years)",
        y = "Maximum age (years)",
        color = "Mean age (years)",
        size = "Sample size"
    ) +
    scale_size_area(max_size = 16, breaks = c(100, 500, 1000, 2000, 5000, 10000, 50000), guide = guide_legend(override.aes = list(color = "#e67467"))) +
    scale_color_gradient(
        low = "grey80", high = "#e67467",
        limits = c(0, NA),
        breaks = seq(0, 80, 20),
        guide = guide_colorbar(
            barwidth = 0.8, barheight = 8, ticks.colour = NA, limits = c(0, NA)
        )
    ) +
    theme_minimal() +
    theme(
        legend.title = element_markdown(hjust = 0),
        text = element_text(family = "Source Sans"),
        axis.title = element_markdown(),
        plot.title.position = "plot",
        plot.title = element_markdown(size = 16, face = "bold"),
        plot.caption = element_markdown(),
        panel.grid = element_line(color = "#dddddd"),
    )

ggsave("files/imaging_age_distribution.png", width = 9, height = 4.5)



