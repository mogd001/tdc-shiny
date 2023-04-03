library(tidyverse)
library(lubridate)
library(sf)
library(tdcR)

get_flows <- function(from = "", to = "", site_catchment) {
  get_data_collection(
    collection = "ActiveFlowSites", from = from, to = to
  ) %>%
    rename(flow_m3ps = value) %>%
    group_by(site) %>%
    arrange(datetime) %>%
    ungroup() %>% 
    mutate(
      datetime = with_tz(datetime, tz= "NZ"),
      site_name = substring(site, 4)
    ) %>% 
    left_join(site_catchment, by = "site")
}

get_rainfall <- function(from = "", to = "", site_catchment) {
  get_data_collection(collection = "AllRainfall", method = "Total", from = from, to = to, interval = "1 hour") %>%
    rename(rainfall_total_mm = value) %>%
    group_by(site) %>%
    arrange(datetime) %>%
    ungroup() %>%
    mutate(
      datetime = with_tz(datetime, tz = "NZ"),
      rainfall_total_mm = round(rainfall_total_mm, digits = 2),
      site_name = substring(site, 4)
    ) %>%
    filter(!is.na(rainfall_total_mm)) %>% 
    left_join(site_catchment, by = "site")
}