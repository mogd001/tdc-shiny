library(tdcR)
library(tidyverse)

get_rainfall <- function(from = "", to = "Now") {
  get_data_collection(
    collection = "Rainfall", method = "Total", from = from, to = to, interval = "1 hour"
  ) %>%
    rename(rainfall_total_mm = value) %>%
    group_by(site) %>%
    arrange(datetime) %>%
    mutate(date = as.Date(datetime, tz = "Etc/GMT-12")) %>%
    slice(-1) %>%
    ungroup() %>%
    mutate(
      site_name = substring(site, 4)
    )
}
