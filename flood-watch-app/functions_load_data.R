library(tidyverse)
library(lubridate)
library(sf)
library(tdcR)

# Catchments
catchments <- st_read("data/context.gpkg", layer = "catchments") %>%
  mutate(catchment = factor(catchment,
                            ordered = TRUE,
                            levels = c("Aorere", "Takaka", "Riwaka", "Motueka", "Marahau", "Moutere", "Waimea", "Nelson", "Buller")
  )) %>%
  mutate(catchment = replace(catchment, catchment == "Other", "Motueka"))

# Sites
sites <- get_sites(synonyms = TRUE) %>%
  mutate(
    longitude_ = longitude,
    latitude_ = latitude
  ) %>%
  st_as_sf(coords = c("longitude_", "latitude_"), crs = 4326) %>%
  st_transform(crs = 2193) %>%
  mutate(
    easting = st_coordinates(.)[, "X"],
    northing = st_coordinates(.)[, "Y"],
    site_name = second_synonym
  ) %>% 
  st_join(catchments, join = st_intersects) %>%
  replace_na(list(catchment = "Motueka")) %>% 
  st_transform(crs = 4326)

catchments <- catchments %>% st_transform(crs = 4326)
catchment_centroids <- catchments %>% st_centroid(catchments)

# Rivers
rivers <- st_read("data/context.gpkg", layer = "rivers") %>%
  st_transform(crs = 4326)

site_names <- sites %>% 
  select(site, site_name) %>% 
  st_drop_geometry()

get_flows <- function(from = "", to = "", site_catchment) {
  get_data_collection(
    collection = "ActiveFlowSites", from = from, to = to
  ) %>%
    rename(flow_m3ps = value) %>%
    group_by(site) %>%
    arrange(datetime) %>%
    ungroup() %>% 
    mutate(
      datetime = with_tz(datetime, tz= "NZ")
    ) %>% 
    left_join(site_catchment, by = "site")  %>% 
    left_join(site_names, by = "site") 
}

get_rainfall <- function(from = "", to = "", site_catchment) {
  get_data_collection(collection = "AllRainfall", method = "Total", from = from, to = to, interval = "1 hour") %>%
    rename(rainfall_total_mm = value) %>%
    group_by(site) %>%
    arrange(datetime) %>%
    ungroup() %>%
    mutate(
      datetime = with_tz(datetime, tz = "NZ"),
      rainfall_total_mm = round(rainfall_total_mm, digits = 2)
    ) %>%
    filter(!is.na(rainfall_total_mm)) %>% 
    left_join(site_catchment, by = "site") %>% 
    left_join(site_names, by = "site")
}