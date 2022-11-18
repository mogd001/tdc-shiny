library(tidyverse)
library(lubridate)
library(sf)
library(tdcR)

get_flows <- function(from = "", to = "", site_catchment) {
  get_data_collection(
    collection = "ActiveFlowSites", method = "Extrema",
    time_interval = NA, from = from, to = to, interval = "15 minutes", alignment = "00:00"
  ) %>%
    rename(flow_m3ps = value) %>%
    group_by(site) %>%
    arrange(datetime) %>%
    slice(-1) %>%
    ungroup() %>% 
    mutate(
      site_name = substring(site, 4)
    ) %>% 
    left_join(site_catchment, by = "site")
}

get_rainfall <- function(from = "", to = "", site_catchment) {
  get_data_collection(collection = "Rainfall", method = "Total", 
    time_interval = NA, from = from, to = to, interval = "1 hour", alignment = "00:00") %>%
    rename(rainfall_total_mm = value) %>%
    group_by(site) %>%
    arrange(datetime) %>%
    slice(-1) %>% 
    ungroup() %>%
    mutate(
      interval = hours(1),
      datetime = datetime + interval,# totals with rainfall so add one hour, "rainfall up to hour"
      rainfall_total_mm = round(rainfall_total_mm, digits = 2)
    ) %>%
    filter(!is.na(rainfall_total_mm)) %>% 
    mutate(
      site_name = substring(site, 4)
    ) %>% 
    left_join(site_catchment, by = "site")
}
