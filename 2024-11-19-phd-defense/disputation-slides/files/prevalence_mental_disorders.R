### PLOT THE PREVALENCE OF PSYCHIATRIC DISORDERS ########################

#-- Libraries -------------------------

library(tidyverse)
library(ggtext)

#-- Load data ------------------------

data_prevalence_europe <- tibble(
    disorder = c(
        "Any mental disorder", "Any mood disorder", "Any anxiety disorder",
        "Any alcohol disorder", "Major depression", "Dysthymia", "GAD",
        "Social phobia", "Specific phobia", "PTSD", "Agoraphobia", "Panic disorder",
        "Alcohol abuse", "Alcohol dependence"
    ),
    lifetime_total = c(25.0, 14.0, 13.6, 5.2, 12.8, 4.1, 2.8, 2.4, 7.7, 1.9, 0.9, 2.1, 4.1, 1.1),
    lifetime_total_ci_low = c(24.2, 13.4, 13.0, 4.8, 12.2, 3.7, 2.5, 2.1, 7.2, 1.7, 0.7, 1.9, 3.7, 0.9),
    lifetime_total_ci_high = c(25.8, 14.6, 14.2, 5.6, 13.4, 4.5, 3.1, 2.7, 8.2, 2.1, 1.1, 2.3, 4.5, 1.3),
    lifetime_male = c(21.6, 9.5, 9.5, 9.3, 8.9, 2.6, 2.0, 1.9, 4.9, 0.9, 0.6, 1.6, 7.4, 1.8),
    lifetime_male_ci_low = c(20.5, 8.7, 8.7, 8.5, 8.2, 2.2, 1.6, 1.5, 4.3, 0.7, 0.4, 1.3, 6.7, 1.4),
    lifetime_male_ci_high = c(22.7, 10.3, 10.3, 10.1, 9.6, 3.0, 2.4, 2.3, 5.5, 1.1, 0.8, 1.9, 8.1, 2.2),
    lifetime_female = c(28.1, 18.2, 17.5, 1.4, 16.5, 5.6, 3.6, 2.9, 10.3, 2.9, 1.1, 2.5, 1.0, 0.4),
    lifetime_female_ci_low = c(27.0, 17.3, 16.6, 1.1, 15.6, 5.1, 3.2, 2.5, 9.5, 2.5, 0.9, 2.1, 0.8, 0.2),
    lifetime_female_ci_high = c(29.2, 19.1, 18.4, 1.7, 17.4, 6.1, 4.0, 3.3, 11.1, 3.3, 1.3, 2.9, 1.2, 0.6),
    month_total = c(9.6, 4.2, 6.4, 1.0, 3.9, 1.1, 1.0, 1.2, 3.5, 0.9, 0.4, 0.8, 0.7, 0.3),
    month_total_ci_low = c(9.1, 3.8, 6.0, 0.8, 3.6, 0.9, 0.8, 1.0, 3.2, 0.7, 0.3, 0.6, 0.6, 0.2),
    month_total_ci_high = c(10.1, 4.6, 6.8, 1.2, 4.2, 1.3, 1.2, 1.4, 3.8, 1.1, 0.5, 1.0, 0.8, 0.4),
    month_male = c(7.1, 2.8, 3.8, 1.7, 2.6, 0.8, 0.5, 0.9, 1.9, 0.4, 0.2, 0.6, 1.3, 0.4),
    month_male_ci_low = c(6.4, 2.3, 3.3, 1.4, 2.2, 0.6, 0.3, 0.7, 1.5, 0.2, 0.1, 0.4, 1.0, 0.2),
    month_male_ci_high = c(7.8, 3.3, 4.3, 2.0, 3.0, 1.0, 0.7, 1.1, 2.3, 0.6, 0.3, 0.8, 1.6, 0.6),
    month_female = c(12.0, 5.6, 8.7, 0.3, 5.0, 1.5, 1.3, 1.4, 5.0, 1.3, 0.6, 1.0, 0.2, 0.1),
    month_female_ci_low = c(11.2, 5.1, 8.0, 0.2, 4.5, 1.2, 1.0, 1.1, 4.5, 1.0, 0.4, 0.8, 0.1, 0.0),
    month_female_ci_high = c(12.8, 6.1, 9.4, 0.4, 5.5, 1.8, 1.6, 1.7, 5.5, 1.6, 0.8, 1.2, 0.3, 0.2)
)

data_norway_young <- readxl::read_excel(
    path = "files/vedlegg-utvalgte-psykiske-lidelser_publisert-fhr-13.09.23.xlsx",
    sheet = "custom",
    range = "A2:Q32"
) |> 
    janitor::clean_names()

data_norway <- tibble(
    disorder = c(
        "Major depressive episode", "Any anxiety disorder", "Generalized anxiety disorder", "Agoraphobia", 
        "Panic disorder", "Social anxiety disorder", "Specific phobia", "Any substance-use disorder", 
        "Alcohol use disorder", "Drug use disorder", "Any disorder"
    ),
    prevalence_30days = c(17.1, 29.9, 16.0, 2.0, 7.4, 10.0, 9.6, 6.0, 5.6, 0.5, 39.7),
    prevalence_30days_ci_low = c(16.2, 28.8, 15.2, 1.65, 6.76, 9.27, 8.96, 5.41, 5.05, 0.39, 38.6),
    prevalence_30days_ci_high = c(17.9, 31.0, 16.9, 2.32, 7.98, 10.7, 10.4, 6.55, 6.14, 0.76, 40.9),
    prevalence_30days_frequency = c(1250, 2092, 1157, 138, 529, 697, 679, 406, 387, 37, 2737),
    lifetime_prevalence = c(46.0, 48.4, 29.0, 3.7, 19.4, 16.2, 14.1, 17.7, 16.1, 3.7, 67.3),
    lifetime_prevalence_ci_low = c(44.9, 47.2, 27.9, 3.32, 18.5, 15.4, 13.3, 16.8, 15.3, 3.27, 66.1),
    lifetime_prevalence_ci_high = c(47.2, 49.6, 30.1, 4.22, 20.3, 17.1, 14.9, 18.6, 17.0, 4.18, 68.3),
    lifetime_prevalence_frequency = c(3359, 3333, 1982, 264, 1396, 1136, 994, 1208, 1119, 252, 4730),
    prevalence_12month = c(35.3, 40.7, 23.8, 2.9, 14.4, 13.6, 11.9, 10.6, 7.7, 3.7, 57.3),
    prevalence_12month_ci_low = c(34.2, 39.5, 22.8, 2.56, 13.6, 12.8, 11.1, 9.90, 7.09, 3.27, 56.1),
    prevalence_12month_ci_high = c(36.4, 41.9, 24.8, 3.37, 15.2, 14.4, 12.7, 11.4, 8.36, 4.18, 58.4),
    prevalence_12month_frequency = c(2572, 2790, 1625, 207, 1034, 949, 838, 724, 535, 252, 3977)
)

#-- Create Europe prevalence plot ------------------------

data_prevalence_europe |>
    ggplot(aes(x = lifetime_total / 100, y = reorder(disorder, lifetime_total))) +
    geom_col(fill = "#e67467", width = 0.5) +
    geom_vline(xintercept = 0, linewidth = 1) +
    geom_text(
        aes(label = str_glue("{round(lifetime_total, 1)}%")),
        hjust = 0, nudge_x = 0.9 / 100
    ) +
    geom_errorbarh(
        aes(
            xmin = lifetime_total_ci_low / 100,
            xmax = lifetime_total_ci_high / 100
        ),
        height = 0.25
    ) +
    labs(
        title = "Lifetime prevalence of mental disorders in Europe",
        x = NULL,
        y = NULL,
        caption = "Percentage (± 95% CI)<br>Data collected between **<span style='color: #e67467'>2001 and 2003</span>**<br>from Belgium, France, Germany, Italy, the Netherlands and Spain"
    ) +
    scale_x_continuous(
        limits = c(0, 0.3),
        labels = scales::label_percent(),
        expand = expansion(add = c(0, NA))
    ) +
    theme_minimal() +
    theme(
        text = element_text(family = "Source Sans"),
        axis.text.y = element_markdown(face = "bold"),
        plot.title.position = "plot",
        plot.title = element_markdown(size = 16, face = "bold"),
        plot.caption = element_markdown(),
        panel.grid = element_line(color = "#dddddd"),
        panel.grid.major.y = element_blank(),
    )

ggsave("files/prevalence_mental_disorders_europe.png", width = 9, height = 4.5)


#-- Create Norway young people plot ------------------------

data_norway_young |>
    select(gender, year = ar, ends_with("percent")) |> 
    pivot_longer(cols = ends_with("percent"), names_to = "category", values_to = "percentage") |> 
    mutate(
        age_cat = str_remove(category, "_andel_percent"),
        age_cat = str_remove(age_cat, "x"),
        age_cat = str_replace(age_cat, "_", " - "),
        age_cat = str_glue("{age_cat} years"),
        age_cat = fct_relevel(age_cat, c("0 - 5 years", "6 - 11 years", "12 - 15 years", "16 - 19 years", "20 - 24 years")),
        gender = ifelse(gender == "jenter", "Girls/Women", "Boys/Men")
    ) |> 
    ggplot(aes(x = year, y = percentage / 100, color = age_cat)) +
    geom_rect(
        mapping = aes(xmin = 2020, xmax = Inf, ymin = -Inf, ymax = Inf),
        color = "transparent",
        fill = "#dddddd",
        alpha = 1/60
    ) +
    geom_hline(yintercept = 0, linewidth = 1) +
    geom_path(
        linewidth = 2,
        lineend = "round",
        key_glyph = "point"
    ) +
    labs(
        title = "Point prevalence of psychiatric symptoms amoung children, adolescents,<br>and young adults in Norway",
        x = NULL,
        y = NULL,
        color = NULL,
        caption = "Data collected between **<span style='color: #e67467'>2008 and 2022</span>** from first-line health services in Norway"
    ) +
    scale_x_continuous(
        expand = expansion(add = c(0.25, 0))
    ) +
    scale_y_continuous(
        limits = c(0, 0.26),
        labels = scales::label_percent(),
        expand = expansion(add = c(0, NA)),
        position = "right"
    ) +
    scale_color_manual(
        values = c("#f2c674", "#8a382d", "#1d4947", "#8ee7fc",  "#ff8274"),
        guide = guide_legend(reverse = TRUE, override.aes = list(size = 6))
    ) +
    facet_wrap(~ gender, ncol = 2, scales = "free_y") + 
    coord_cartesian(clip = "off") +
    theme_minimal() +
    theme(
        text = element_text(family = "Source Sans"),
        plot.title.position = "plot",
        plot.title = element_markdown(size = 16, face = "bold"),
        plot.caption = element_markdown(),
        strip.text = element_markdown(size = 12, face = "bold"),
        panel.grid = element_line(color = "#dddddd"),
    )

ggsave("files/prevalence_mental_disorders_young_norway.png", width = 9, height = 4.5)

#-- Create Norway plot ------------------------

data_norway |>
    ggplot(aes(x = prevalence_30days / 100, y = reorder(disorder, prevalence_30days))) +
    geom_col(fill = "#e67467", width = 0.5) +
    geom_vline(xintercept = 0, linewidth = 1) +
    geom_text(
        aes(label = str_glue("{round(prevalence_30days, 1)}%")),
        hjust = 0, nudge_x = 1.6 / 100
    ) +
    geom_errorbarh(
        aes(
            xmin = prevalence_30days_ci_low / 100,
            xmax = prevalence_30days_ci_high / 100
        ),
        height = 0.25
    ) +
    labs(
        title = "Point prevalence of mental disorders in students in Norway",
        x = NULL,
        y = NULL,
        caption = "Percentage (± 95% CI)<br>Data collected in **<span style='color: #e67467'>2022</span>** from students aged 18 - 35 years"
    ) +
    scale_x_continuous(
        limits = c(0, 0.5),
        labels = scales::label_percent(),
        expand = expansion(add = c(0, 0.01))
    ) +
    theme_minimal() +
    theme(
        text = element_text(family = "Source Sans"),
        axis.text.y = element_markdown(face = "bold"),
        plot.title.position = "plot",
        plot.title = element_markdown(size = 16, face = "bold"),
        plot.caption = element_markdown(),
        panel.grid = element_line(color = "#dddddd"),
        panel.grid.major.y = element_blank(),
    )

ggsave("files/prevalence_mental_disorders_norway.png", width = 9, height = 4.5)

