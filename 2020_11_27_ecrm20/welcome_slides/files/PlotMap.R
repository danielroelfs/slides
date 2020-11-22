### MAKE NORWAY PLOT ########################

#-- Libraries ------------------------

library(tidyverse)

#-- Load data ------------------------

file <- "../../NORMENT/R_Scripts/MoBa_geography/norge_fylker.json"
mapdata_load <- geojsonio::geojson_read(file, what = "sp")
mapdata_load@data$id <- rownames(mapdata_load@data)
mapdata_fortified <- fortify(mapdata_load, region = "id")
mapdata_merged <- merge(mapdata_fortified, mapdata_load@data, by = "id")

mapdata <- mapdata_merged %>%
  select(id,long,lat,group)

#-- Get scanner locations ------------------------

#register_google(google_code)
#scanner_locs <- tibble(city = c("St. Olav, Trondheim","Haukeland Sykehus","Ullevål Sykehus")) %>%
#  mutate_geocode(city)
#save(scanner_locs, file = "scanner_locs.Rdata")

locs <- tribble(
  ~city, ~lon, ~lat,
  "Ullevål Sykehus", 10.7, 59.9,
  "Haukeland Sykehus", 5.36, 60.4,
  "St. Olav, Trondheim", 10.4, 63.4
)

#-- Create plot ------------------------

ggplot(mapdata) +
  geom_polygon(data = map_data("world"), aes(long,lat,group = group),
               fill = rgb(9,9,9, maxColorValue = 255), inherit.aes = FALSE) +
  geom_map(aes(map_id = id), map = mapdata %>% rename(x = long, y = lat),
           fill = rgb(9,9,9,maxColorValue = 255)) +
  geom_point(data = locs, aes(x = lon, y = lat), inherit.aes = FALSE, 
             shape = 18, size = 4, color = "#E896C1") +
  labs(fill = NULL,
       color = NULL) +
  scale_fill_gradient(low = palette[1], high = palette[2], guide = FALSE) +
  coord_map(projection = "conic", lat0 = 60,
            xlim = c(-2,24), ylim = c(58,67)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = -10),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = rgb(34,34,34,maxColorValue = 255)),
        plot.background = element_rect(fill = rgb(34,34,34,maxColorValue = 255)))

ggsave("map.png", bg = "transparent")
