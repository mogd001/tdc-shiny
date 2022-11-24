library(tdcR)
library(tidyverse)

get_flows <- function(from = "", to = "Now") {
  get_data_collection(
    collection = "ActiveFlowSites", method = "Extrema",
    time_interval = NA, from = from, to = to, interval = "1 hour", alignment = "00:00"
  ) %>%
    rename(flow_m3ps = value) %>%
    group_by(site) %>%
    arrange(datetime) %>%
    slice(-1) %>%
    ungroup() %>%
    mutate(
      site_name = substring(site, 4)
    )
}
