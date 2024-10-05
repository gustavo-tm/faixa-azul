library(tidyverse)
library(osmdata)

# https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html
# https://rspatialdata.github.io/osm.html

# https://wiki.openstreetmap.org/wiki/Map_features
available_features()

osm <- getbb('São Paulo') |> 
  opq(bbox = _) |> 
  add_osm_feature(key = 'highway', value = c(
    "motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "service",
    "motorway_link", "trunk_link", "primary_link", "secondary_link", "motorway_junction",
    "speed_camera"))  |> 
  osmdata_sf()


mapview::mapview(osm$osm_points)
