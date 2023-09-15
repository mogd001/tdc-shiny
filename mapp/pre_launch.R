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


transform_to_wgs_from_nztm_coords  <- function(df) {
  df %>%
    st_as_sf(coords = c("easting", "northing"), crs = 2193) %>%
    st_transform(crs = 4326)
}

# All sites
all_sites <- get_sites(latlong = FALSE) %>%
  transform_to_wgs_from_nztm_coords()

# Flow sites
flow_sites <- get_sites(collection = "ActiveFlowSites", latlong = FALSE) %>%
  mutate(measurement = "flow") %>%
  transform_to_wgs_from_nztm_coords()

# Rainfall sites
rainfall_sites <- get_sites(collection = "Rainfall", latlong = FALSE) %>%
  mutate(measurement = "rainfall")  %>%
  transform_to_wgs_from_nztm_coords()

# Groundwater sites
groundwater_sites <- get_sites(collection = "GWcheckDiff", latlong = FALSE)  %>%
  mutate(measurement = "groundwater") %>%
  transform_to_wgs_from_nztm_coords()

# Water level sites
waterlevel_sites <- get_sites(collection = "ActiveWLonlySites", latlong = FALSE) %>%
  mutate(measurement = "waterlevel") %>%
  transform_to_wgs_from_nztm_coords()

# Tide sites
tide_sites <- get_sites(collection = "ActiveTideSites", latlong = FALSE, synonyms = FALSE) %>%
  mutate(measurement = "tide")  %>%
  transform_to_wgs_from_nztm_coords()

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
