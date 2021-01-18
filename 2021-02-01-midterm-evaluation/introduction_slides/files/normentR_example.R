### EXAMPLE PLOTS NORMENTR ########################

#-- Libraries ------------------------

library(tidyverse)
library(patchwork)
library(ggalluvial)
library(normentR)

#-- FREQUENCY TRIANGLES ------------------------

#-- Load data ------------------------

loaddata <- tibble(
  q = rep(c("Nervousness","Anxiety","Insecurity","Negativity","Brain fog","Avoidant"), each = 3),
  a = rep(c("A bit","Somewhat","A lot"), times = 6),
  yn = c(4,12,8,18,24,9,6,8,2,3,16,4,10,18,2,20,12,0)
)

#-- Prepare data ------------------------

plotdata1 <- loaddata %>%
  mutate(an = fct_relevel(a, `1` = "A bit", `2` = "Somewhat", `3` = "A lot"),
         an = as.integer(an),
         ns = 0)

plotdata <- plotdata1 %>%
  pivot_longer(contains("n"), names_to = "cname", values_to = "y") %>%
  mutate(x = fct_relevel(a, `1` = "A bit", `2` = "Somewhat", `3` = "A lot"),
         x = ifelse(cname == "ns", 0, x),
         y = ifelse(cname == "an", 0, y),
         a = fct_relevel(a, "A bit", "Somewhat", "A lot"),
         a2 = fct_relevel(a, rev(levels(a))))

#-- Create plot ------------------------

p1 <- ggplot(plotdata, aes(x = x, y = y, fill = a2)) +
  geom_polygon(alpha = 0.6, color = "black", size = 0.1) +
  labs(title = "Symptom intensity over an average month",
       fill = NULL,
       caption = "Height indicates frequency") +
  scale_fill_norment(discrete = TRUE, reverse = TRUE, limits = c("A bit", "Somewhat", "A lot")) +
  theme_norment(dark = TRUE, bg_col = "transparent") +
  theme(
    text = element_text(family = "Arial"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(fill = "transparent", colour = "transparent"),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    strip.text.x = element_text(margin = margin(0.1,0,0.2,0, "cm"), colour = "#eeeeee")
  ) +
  facet_wrap(~ q, strip.position = "bottom")


#-- PLOT NORWAY MAP ------------------------

file <- "/Users/dtroelfs/Dropbox/NORMENT//R_Scripts/MoBa_geography/norge_fylker.json"
mapdata_load <- geojsonio::geojson_read(file, what = "sp")
mapdata_load@data$id <- rownames(mapdata_load@data)
mapdata_fortified <- fortify(mapdata_load, region = "id")
mapdata_merged <- merge(mapdata_fortified, mapdata_load@data, by = "id")

moba_load <- readxl::read_xlsx("/Users/dtroelfs/Dropbox/NORMENT//R_Scripts/MoBa_geography/MOBA_inkluderte_fylkesvis.xlsx", skip = 2) %>%
  rename(fylke_no = `...1`,
         oldname = `...2`,
         total = SUM) %>%
  filter(!is.na(oldname)) %>%
  rename_at(vars(starts_with("2")), list( ~paste0("y",.))) %>%
  replace(is.na(.), 0)

#-- Get new fylke names ------------------------

fylke_names <- data.frame(
  oldname = c("Ãstfold","Akershus","Hedmark","Oppland","Buskerud",
              "Vestfold","Telemark","Aust-Agder","Vest-Agder","Hordaland",
              "Bergen","Sogn og Fjordane","Sør-Trøndelag","Nord-Trøndelag",
              "Troms","Finnmark",
              "Oslo","Rogaland","Møre og Romsdal","Nordland"),
  newname = c("Viken","Viken","Innlandet","Innlandet","Viken",
              "Vestfold og Telemark","Vestfold og Telemark","Agder", "Agder",
              "Vestland","Vestland","Vestland","Trøndelag","Trøndelag",
              "Troms og Finnmark","Troms og Finnmark",
              "Oslo","Rogaland","Møre og Romsdal","Nordland")
)

#-- Combine map with MoBa ------------------------

mapdata <- mapdata_merged %>%
  select(id,long,lat,group)

alldata <- mapdata_merged %>%
  rename(oldname = NAME_1) %>%
  mutate(oldname = ifelse(str_detect(oldname, "Trøndelag"), "Trøndelag", oldname)) %>%
  merge(fylke_names, by = "oldname", all.x = TRUE) %>%
  mutate(oldname = ifelse(oldname == "Ãstfold", "Østfold", oldname)) %>%
  merge(moba_load, by = "oldname", all.x = TRUE)

alldata_newnames <- alldata %>%
  group_by(newname, order, id) %>%
  summarise_at(vars(starts_with("y20")), .funs = list(sum))

#-- Create table ------------------------

fylke_table <- alldata %>%
  group_by(oldname) %>%
  summarise(ncases = first(y2009)) %>%
  mutate(ncases = as.integer(ncases)) %>%
  arrange(-ncases)

plotdata <- alldata %>%
  mutate(n = y2009)

#-- Create plot ------------------------

p2 <- ggplot(plotdata, aes(fill = n, color = n)) +
  geom_polygon(data = map_data("world"), aes(long,lat,group = group),
               fill = "grey20", inherit.aes = FALSE) +
  geom_map(aes(map_id = id), map = mapdata %>% rename(x = long, y = lat)) +
  labs(title = "Number of MoBa participants per fylke",
       fill = NULL,
       color = NULL) +
  scale_color_norment(palette = "acton", reverse = TRUE,
                      guide = guide_colorbar(barwidth = 0.5, barheight = 8, 
                                             ticks = FALSE, label.position = "left")) +
  scale_fill_norment(palette = "acton", reverse = TRUE, guide = FALSE) +
  coord_map(projection = "conic", lat0 = 60,
            xlim = c(1.8,30), ylim = c(57,71)) +
  theme_norment(dark = TRUE, bg_col = "transparent") +
  theme(
    panel.grid.major = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 16),
    legend.position = c(0.92,0.01),
    legend.direction = "vertical",
    legend.background = element_blank(),
    legend.title = element_text(size = 10),
    legend.justification = c(0.025,0),
    plot.margin = margin(t = 10, r = 0, b = 0, l = 0),
  )

#-- ALLUVIAL PLOT ------------------------

data(vaccinations)
vaccinations <- vaccinations %>%
  mutate(freq = freq * 1000)
p3 <- ggplot(vaccinations,
             aes(x = survey, stratum = response, alluvium = subject,
                 y = freq, fill = response, label = freq)) +
  labs(title = "Vaccination survey responses at three points in time",
       x = "Survey",
       y = "Responses",
       fill = NULL) +
  geom_flow() +
  geom_stratum() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_norment(discrete = TRUE) +
  theme_norment(dark = TRUE, bg_col = "transparent") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 8))
show_norment_logo(p3)

#-- VAGINA PLOT ------------------------

palette <- c("#e5007d", "#0081c9" )

p4 <- ToothGrowth %>%
  mutate(dose = as_factor(dose)) %>%
  ggplot(aes(x = dose, y = len, fill = supp)) +
  geom_violin(alpha = 0.8, position = position_dodge(1)) +
  geom_boxplot(width = 0.2, alpha = 0.8, position = position_dodge(1)) + 
  labs(title = "Length of odontoblasts after treatment with supplement",
       x = "Dose (mg)",
       y = "Tooth length (mm)",
       fill = "Supplement type") +
  scale_fill_manual(values = palette) +
  theme_norment(dark = TRUE, bg_col = "transparent") +
  theme(plot.title = element_text(size = 8))
print(p4)

#-- Label ------------------------

png(filename = "/Users/dtroelfs/Dropbox/University/PhD/210301 Mid-term evaluation/introduction_slides/files/normentR_example_composite.png",
    bg = "transparent", width = 16, height = 9, units = "in", res = 300)
show_norment_logo(
  (((p1) / (p3 | p4)) | p2), 
  pos = c(0.95, 0.1))
dev.off()

