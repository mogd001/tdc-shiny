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
sites <- get_sites() %>%
  mutate(
    longitude_ = longitude,
    latitude_ = latitude
  ) %>%
  st_as_sf(coords = c("longitude_", "latitude_"), crs = 4326) %>%
  st_transform(crs = 2193) %>%
  mutate(
    easting = st_coordinates(.)[, "X"],
    northing = st_coordinates(.)[, "Y"]
  ) %>% 
  st_join(catchments, join = st_intersects) %>%
  replace_na(list(catchment = "Motueka")) %>% 
  mutate(
    site_name = substring(site, 4)
  )
