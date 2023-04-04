library(tdcR)
library(tidyverse)
library(sf)
library(config)

config <- config::get()

# Topo template
t_prefix <- "http://tiles-a.data-cdn.linz.govt.nz/services;key="
key <- config$linzkey
tMid <- "/tiles/v4/layer="
tSuffix <- "/EPSG:3857/{z}/{x}/{y}.png"
topo_50_template <- paste0(t_prefix, key, tMid, 50767, tSuffix)

# Sites
sites <- tdcR::get_sites(latlong = FALSE) %>%
  st_as_sf(coords = c("easting", "northing"), crs = 2193) %>%
  st_transform(crs = 4326)

# Catchments
catchments <- st_read("data/context.gpkg", layer = "catchments") %>%
  mutate(catchment = factor(catchment,
                            ordered = TRUE,
                            levels = c("Aorere", "Takaka", "Riwaka", "Motueka", "Marahau", "Moutere", "Waimea", "Nelson", "Buller")
  )) %>%
  mutate(catchment = replace(catchment, catchment == "Other", "Motueka")) %>%
  st_transform(crs = 4326)

catchment_centroids <- catchments %>% st_centroid(catchments)

# Rivers
rivers <- st_read("data/context.gpkg", layer = "rivers") %>%
  st_transform(crs = 4326)
