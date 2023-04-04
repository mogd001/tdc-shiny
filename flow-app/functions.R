library(tdcR)
library(tidyverse)
library(lubridate)

get_flows <- function(from = "", to = "Now") {
  get_data_collection(
    collection = "ActiveFlowSites", from = from, to = to
  ) %>%
    rename(flow_m3ps = value) %>%
    group_by(site) %>%
    arrange(datetime) %>%
    mutate(
      datetime = with_tz(datetime, tz = "NZ"),
      date = as.Date(datetime, tz = "Etc/GMT-12")) %>%
    slice(-1) %>%
    ungroup() %>%
    mutate(
      site_name = substring(site, 4)
    )
}
