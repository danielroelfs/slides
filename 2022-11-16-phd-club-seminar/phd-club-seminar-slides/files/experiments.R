### PHD CLUB SEMINAR EXPERIMENTS ########################

#-- Libraries -------------------------

library(tidyverse)
library(ggtext)
library(scico)

#-- From A to B, figure B ------------------------

palmerpenguins::penguins |> 
    ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = body_mass_g)) + 
    geom_point(alpha = .6, size = 3) + 
    ggforce::geom_mark_ellipse(aes(group = species, label = species)) +
    labs(title = "Bill size of brush-tailed penguins _Pygoscelis_",
         caption = "Data: Gorman, Williams & Fraser (2014), _PLoS ONE_", 
         x = "**Bill length** (mm)",
         y = "**Bill depth** (mm)",
         color = "**Body mass** (g)") +
    scale_x_continuous(limits = c(25, 65)) + 
    scale_y_continuous(limits = c(12, 24)) +
    scale_color_scico(palette = "bamako", direction = -1, 
                      guide = guide_colorbar(title.position = "top", title.hjust = .5, 
                                             ticks = FALSE, 
                                             barwidth = 15, barheight = 0.5)) +
    coord_cartesian(expand = FALSE, clip = "off") +
    theme_minimal() +
    theme(plot.title = element_markdown(size = 18, face = "bold"),
          plot.title.position = "plot",
          plot.caption = element_markdown(margin = margin(t = 15)), 
          plot.caption.position = "plot",
          legend.position = "top",
          legend.title = element_markdown(),
          axis.title.x = element_markdown(),
          axis.title.y = element_markdown(),
          plot.margin = margin(15, 15, 5, 15))

#-- Correlation plot ------------------------

corr_matrix <- palmerpenguins::penguins_raw |> 
    janitor::clean_names() |>  # clean up names
    select(culmen_length_mm:body_mass_g) |> # select relevant columns
    cor(use="complete.obs") |> # correlation, ignoring NAs
    as.data.frame() |> # convert to data frame
    rownames_to_column(var = "var1") |> # get row names as columns
    pivot_longer(cols = 2:last_col(), names_to = "var2", values_to = "cor") # make long format

corr_matrix |> 
    mutate(across(c(var1, var2), ~ str_replace_all(.x, "_", " ")), # get back nicer labels
           across(c(var1, var2), ~ str_replace_all(.x, "mm", "(mm)")),
           across(c(var1, var2), ~ str_replace_all(.x, "g$", "(g)")),
           across(c(var1, var2), ~ str_to_sentence(.x))) |> 
    ggplot(aes(x = var1, y = var2, fill = cor)) + 
    geom_tile(data = . %>% filter(var1 != var2)) + # heatmap, ignoring diagonal
    geom_point(data = . %>% filter(var1 == var2), shape = 4, size = 12) + # showing shape for diagonal
    geom_text(data = . %>% filter(var1 != var2), aes(label = round(cor,2)), size = 2.5) + # add correlation coefficient
    labs(title = "Correlation between body dimensions of _Pygoscelis_",
         x = NULL,
         y = NULL,
         fill = "Corr (r~g~)") +
    scale_x_discrete(limits = rev) + # reverse x-axis order
    scale_fill_scico(palette = "vik", limits = c(-1, 1),
                     guide = guide_colorbar(barwidth = 0.75, barheight = 10, ticks = FALSE)) +
    coord_equal() + # make tiles perfectly square
    theme_minimal() +
    theme(panel.grid = element_blank(), # hide grid
          plot.title = element_markdown(size = 12, face = "bold"), # read title as Markdown format
          plot.title.position = "plot", # make title align with y-axis labels instead of heatmap
          legend.title = element_markdown(face = "bold"),
          axis.text.x = element_markdown(angle = 45, hjust = 1),
          axis.title.y = element_markdown(),
          plot.margin = margin(15, 15, 5, 15))

#-- Raincloud plots (horizontal) ------------------------

palmerpenguins::penguins |> 
    ggplot(aes(x = body_mass_g, y = species)) + 
    geom_boxplot(width = 0.2, outlier.shape = NA) + 
    geom_jitter(aes(color = species), height = 0.1, alpha = 0.3, key_glyph = "point") + 
    ggdist::stat_halfeye(aes(fill = species), adjust = 0.5, height = 0.7, .width = 0,
                         justification = -0.2, point_colour = NA) +
    labs(title = "Body mass in grams by species<br>for brush-tailed penguins _Pygoscelis_",
         x = "**Body mass** (_g_)",
         y = NULL,
         color = NULL) +
    scale_x_continuous(labels = scales::label_number(big.mark = "", suffix = " g"), 
                       expand = expansion(add = 500)) + 
    scale_y_discrete(limits = rev) +
    scale_color_brewer(palette = "Dark2", guide = guide_legend(override.aes = list(alpha = 1, size = 6))) + 
    scale_fill_brewer(palette = "Dark2", guide = "none") +
    theme_minimal() +
    theme(plot.title = element_markdown(size = 18, face = "bold"),
          plot.title.position = "plot",
          legend.position = "top",
          legend.text = element_markdown(face = "italic"),
          axis.text.y = element_markdown(face = "bold"),
          axis.title.x = element_markdown(),
          plot.margin = margin(15, 15, 10, 15))

#-- Raincloud plots (vertical) ------------------------

palmerpenguins::penguins |> 
    ggplot(aes(x = species, y = body_mass_g)) + 
    geom_boxplot(width = 0.2, outlier.shape = NA) + 
    geom_jitter(aes(color = species), width = 0.05, alpha = 0.3, key_glyph = "point") + 
    ggdist::stat_halfeye(aes(fill = species), adjust = 0.5, width = 0.7, .width = 0, 
                         justification = -0.2, point_colour = NA) +
    labs(title = "Body mass in grams by species<br>for brush-tailed penguins _Pygoscelis_",
         x = NULL,
         y = "**Body mass** (_g_)",
         color = NULL) +
    scale_y_continuous(labels = scales::label_number(big.mark = "", suffix = " g"), 
                       expand = expansion(add = 500)) + 
    scale_color_brewer(palette = "Dark2", guide = guide_legend(override.aes = list(alpha = 1, size = 6))) + 
    scale_fill_brewer(palette = "Dark2", guide = "none") +
    theme_minimal() +
    theme(plot.title = element_markdown(size = 18, face = "bold"),
          plot.title.position = "plot",
          legend.position = "top",
          legend.text = element_markdown(face = "italic"),
          axis.text.x = element_markdown(face = "bold"),
          axis.title.y = element_markdown(),
          plot.margin = margin(15, 15, 10, 15))

#-- Vagina plots ------------------------

palmerpenguins::penguins |> 
    ggplot(aes(x = species, y = body_mass_g, fill = species)) + 
    geom_violin(alpha = 0.5) +
    geom_boxplot(width = 0.2, outlier.shape = 4) + 
    labs(title = "Body mass in grams by species<br>for brush-tailed penguins _Pygoscelis_",
         x = NULL,
         y = "**Body mass** (_g_)",
         color = NULL) +
    scale_y_continuous(labels = scales::label_number(big.mark = "", suffix = " g"), 
                       expand = expansion(add = 500)) + 
    scale_color_brewer(palette = "Dark2", guide = guide_legend(override.aes = list(alpha = 1, size = 6))) + 
    scale_fill_brewer(palette = "Dark2", guide = "none") +
    theme_minimal() +
    theme(plot.title = element_markdown(size = 18, face = "bold"),
          plot.title.position = "plot",
          legend.position = "top",
          legend.text = element_markdown(face = "italic"),
          axis.text.x = element_markdown(face = "bold"),
          axis.title.y = element_markdown(),
          plot.margin = margin(15, 15, 10, 15))

#-- Scatterplot with shapes ------------------------

palmerpenguins::penguins |> 
    ggplot(aes(x = flipper_length_mm, y = body_mass_g, color = island, shape = species)) + 
    geom_point(size = 3, alpha = 0.7) +
    labs(title = "Body mass by species per Island",
         subtitle = "Data from the brush-tailed penguins _Pygoscelis_",
         x = "**Flipper length** (_mm_)",
         y = "**Body mass** (_g_)",
         color = "Island",
         shape = "Species") +
    scale_x_continuous(labels = scales::label_number(suffix = " mm"),
                       expand = expansion(add = 5)) +
    scale_y_continuous(labels = scales::label_number(big.mark = "", suffix = " g"), 
                       expand = expansion(add = 500)) + 
    scale_color_brewer(palette = "Dark2", 
                       guide = guide_legend(title.position = "top", title.hjust = 0.5,
                                            override.aes = list(alpha = 1, size = 6))) + 
    scale_shape_discrete(guide = guide_legend(title.position = "top", title.hjust = 0.5,
                                              override.aes = list(color = "grey30", alpha = 1, size = 6))) +
    theme_minimal() + 
    theme(plot.title = element_markdown(size = 14, face = "bold"),
          plot.title.position = "plot",
          plot.subtitle = element_markdown(),
          legend.position = "top",
          legend.title = element_markdown(face = "bold"),
          axis.title.x = element_markdown(),
          axis.title.y = element_markdown(),
          plot.margin = margin(15, 15, 10, 15))

#-- Scatterplots with correlations ------------------------

palmerpenguins::penguins |> 
    ggplot(aes(x = flipper_length_mm, y = bill_length_mm, color = species, fill = species)) + 
    geom_point(size = 2, alpha = 0.6) +
    geom_smooth(method = "lm", alpha = 0.2) +
    ggpubr::stat_cor() +
    labs(title = "Association between flipper and bill length",
         subtitle = "Data from the brush-tailed penguins _Pygoscelis_",
         x = "**Flipper length** (_mm_)",
         y = "**Bill length** (_mm_)",
         color = "Species") +
    scale_x_continuous(labels = scales::label_number(suffix = " mm"),
                       expand = expansion(add = 5)) +
    scale_y_continuous(labels = scales::label_number(big.mark = "", suffix = " mm"), 
                       expand = expansion(add = 5)) + 
    scale_color_brewer(palette = "Dark2", 
                       guide = guide_legend(title.position = "top", title.hjust = 0.5,
                                            override.aes = list(alpha = 1, size = 6))) +
    scale_fill_brewer(palette = "Dark2", guide = "none") +
    theme_minimal() +
    theme(plot.title = element_markdown(size = 14, face = "bold"),
          plot.title.position = "plot",
          plot.subtitle = element_markdown(),
          legend.position = "top",
          legend.title = element_markdown(face = "bold"),
          axis.title.x = element_markdown(),
          axis.title.y = element_markdown(),
          plot.margin = margin(15, 15, 5, 15))

#-- Simpsons Paradox and {patchwork} ------------------------

library(patchwork)

p1 <- palmerpenguins::penguins |> 
    ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) + 
    geom_point(size = 2, alpha = 0.6) +
    geom_smooth(color = "black", method = "lm", se = FALSE) +
    ggpubr::stat_cor()

p2 <- palmerpenguins::penguins |> 
    ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) + 
    geom_point(size = 2, alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
    ggpubr::stat_cor(show.legend = FALSE) +
    scale_color_brewer(palette = "Dark2", 
                       guide = guide_legend(title.position = "top", title.hjust = 0.5,
                                            override.aes = list(alpha = 1, size = 6)))
    
p1 + p2 +
    plot_annotation(title = "The _palmerpenguins_ dataset contains<br>an example of Simpson's Paradox",
                    tag_levels = "A", tag_prefix = "Fig. ") & 
    labs(x = "**Bill length** (_mm_)",
         y = "**Bill depth** (_mm_)",
         color = "Species") &
    scale_x_continuous(labels = scales::label_number(suffix = " mm"),
                       expand = expansion(add = 2)) &
    scale_y_continuous(labels = scales::label_number(big.mark = "", suffix = " mm"), 
                       expand = expansion(add = 2)) &
    theme_minimal() &
    theme(plot.title = element_markdown(size = 14, face = "bold"),
          plot.title.position = "plot",
          plot.subtitle = element_markdown(),
          plot.tag = element_markdown(size = 10, face = "bold"),
          legend.position = "bottom",
          legend.title = element_markdown(face = "bold"),
          axis.title.x = element_markdown(),
          axis.title.y = element_markdown(),
          plot.margin = margin(15, 15, 5, 15))
    
