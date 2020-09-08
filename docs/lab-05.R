library(tidyverse)
library(sf)
library(raster)
library(getlandsat)
library(mapview)
library(osmdata)
library(stats)

bb = read_csv("data/uscities.csv") %>%
  filter(city  == "Palo") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(5000) %>%
  st_bbox() %>%
  st_as_sfc

mapview(bb)

bwgs = st_transform(bb, 4326)

osm = osmdata::opq(bwgs) %>%
  osmdata::add_osm_feature("land") %>%
  osmdata::osmdata_sf()

mapview(osm$osm_polygons)

scenes = lsat_scenes()
bbwgs = st_bbox(bwgs)

down = scenes %>%
  filter(min_lat <= bbwgs$ymin, max_lat >= bbwgs$ymax,
         min_lon <= bbwgs$xmin, max_lon >= bbwgs$xmax,
         as.Date(acquisitionDate) == as.Date('2016-09-26'))

write.csv(down, file = 'data/palo-flood.csv', row.names = F)
