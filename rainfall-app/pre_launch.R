config <- config::get()

catchments <- catchments <- st_read("data/context.gpkg", layer = "catchments")  %>%
  st_transform(crs = 4326)
catchment_centroids <- catchments %>% st_centroid(catchments)

rivers <- st_read("data/context.gpkg", layer = "rivers") %>%
  st_transform(crs = 4326)

sites <- get_sites(collection = "AllRainfall", synonyms = TRUE) %>%
  mutate(
    longitude_ = longitude,
    latitude_ = latitude
  ) %>%
  st_as_sf(coords = c("longitude_", "latitude_"), crs = 4326) %>%
  st_join(catchments, join = st_intersects) %>%
  replace_na(list(catchment = "Motueka")) %>%
  rename(site_name = second_synonym)

site_choices <- append(c("All Sites"), pull(sites, site_name))

time_select_choices <- c("Relative", "Absolute")

default_start_date <- as.Date(now()) - days(7) # Past 7 days
default_end_date <- as.Date(now())

# Initialise
rainfall <- NULL
summary <- NULL

sites <- sites %>%
  select(site, site_name, latitude, longitude) %>%
  mutate(
    rainfall_total_mm = NA,
    label = glue("{site_name}")
  )

t_prefix <- "http://tiles-a.data-cdn.linz.govt.nz/services;key="
key <- config$linzkey
tMid <- "/tiles/v4/layer="
tSuffix <- "/EPSG:3857/{z}/{x}/{y}.png"
topo_50_template <- paste0(t_prefix, key, tMid, 50767, tSuffix)

basemap <- function(map_data, longitude, latitude, zoom) {
  leaflet(map_data) %>%
    setView(longitude, latitude, zoom) %>%
    addTiles(urlTemplate = topo_50_template, group = "NZ Topo50")
}
