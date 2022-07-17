library(tidyverse)
library(lubridate)
library(hms)
library(XML)
library(xml2)
library(httr)
library(zeallot)
library(rapport)

library(hillr)

# Also see get_data_hilltopserver_hillr.R for fetching site/measurement/site-specific data.

get_sites_hilltop_server <- function() {
  endpoint <- "http://envdata.tasman.govt.nz/data.hts?"
  sites <- hillr::getHilltopSites(endpoint = endpoint) %>% 
  as_tibble() %>% 
  rename_all(tolower) %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))
}


interval_to_offset <- function(interval){
  # Function to return offset to correct for Hilltops right-bound datetimes
  if (!is.character(interval)) {
    return(NA)
  } else {
    if (grepl("hour", interval)) {
      return(hours(1))
    } else if (grepl("day", interval)) {
      return(days(1))  
    } else if (grepl("month", interval)) {
      return(months(1))  
    } else if (grepl("year", interval)) {
      return(years(1))
    } else {
      return(NA)
    }
  }
}


get_data_hilltop_server <- function(collection, method, time_interval = "P1Y/now", from = "", to = "", interval = "1 day", alignment = "00:00") {
  # Function to get data for a Collection from Hilltop Server.
  endpoint <- "http://envdata.tasman.govt.nz/data.hts?"

  statistic <- paste0(
    "&Method=", method,
    "&Interval=", interval,
    "&Alignment=", alignment
  )
  
  interval_offset <- interval_to_offset(interval)
  print(interval_offset)
  
  if (!is_empty(time_interval)) {
    statistic <- paste0(statistic, "&TimeInterval=", time_interval)
  } else if (!is_empty(from) | !is_empty(to)) {
    statistic <- paste0(statistic, "&From=", from, "&To=", to)
  } else {
    print("Incorrect time_interval or from/to specified.")
    break
  }

  statistic <- gsub(" ", "%20", statistic)

  url <-
    paste0(
      endpoint,
      "Service=Hilltop&Request=GetData&Collection=",
      gsub(" ", "%20", collection),
      statistic
    )
  print(url)

  hilltop_data <- read_xml(url)
  sites <- xpathSApply(xmlParse(hilltop_data), "//Measurement", xmlGetAttr, "SiteName")

  hilltop_df <- hilltop_data %>%
    as_list() %>%
    as_tibble() %>%
    slice(-1) %>% # drop first node (Agency not required)
    unnest_longer("Hilltop") %>%
    filter(Hilltop_id == "Data") %>%
    select("Hilltop") %>%
    mutate(site = sites) %>%
    unnest(cols = names(.)) %>%
    unnest_wider("Hilltop") %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    transmute(
      site = site,
      datetime = ymd_hms(T, tz = "NZ") - interval_offset, 
      value = as.numeric(I1)
    ) %>%
    mutate(
      year = year(datetime),
      yday = yday(datetime),
      month = month(datetime, label = TRUE),
      year_month = floor_date(datetime, unit = "month"),
      day_hour = floor_date(datetime, unit = "hour"),
      time = as_hms(datetime)
    )
}

get_collections <- function() {
  # Function to get Collection list from Hilltop Server
  url <- "http://envdata.tasman.govt.nz/data.hts?Service=Hilltop&Request=CollectionList"

  hilltop_data <- read_xml(url)
  collections <- xpathSApply(xmlParse(hilltop_data), "//Collection", xmlGetAttr, "Name")

  hilltop_df <- hilltop_data %>%
    as_list() %>%
    as_tibble() %>%
    mutate(collection = collections) %>%
    unnest_longer("HilltopProject") %>%
    filter(HilltopProject_id == "Item") %>% # rows without an "Item" have no sites in the collection.
    transmute(collection = collection, data = HilltopProject) %>%
    unnest_wider("data") %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    transmute(
      collection = collection,
      site = SiteName,
      measurement = Measurement
    )
}

############################  Ensemble Statistics
#
# t <- "Month" # options: Day Month Year
# method <- paste("MonthlyPDF") # options: HourlyPDF DailyPDF MonthlyPDF (among others, see Hilltop Server documentation)
# lower_percentile <- "15"
# upper_percentile <- "85"
# time_interval <- "P10Y/now" # options: D M Y # or use &From=1/1/2021&To=1/1/2022
# interval <- "1 day" # options: hour month year
# alignment <- "00:00" # options: 00:00 1 month 1 year
#
# # need to functionlise this such to have either time_interval or from/to
# ensemble_statistic <- paste0(
#   "&Statistic=", method,
#   "&LowerPercentile=", lower_percentile,
#   "&UpperPercentile=", upper_percentile,
#   "&TimeInterval=", time_interval,
#   "&Alignment=", alignment
# )
# ensemble_statistic <- gsub(" ", "%20", ensemble_statistic)
#
# url <-
#   paste0(
#     endpoint,
#     "Service=Hilltop&Request=EnsembleStats&Collection=",
#     gsub(" ", "%20", collection),
#     ensemble_statistic
#   )
#
# hilltop_data <- read_xml(url)
# sites <- xpathSApply(xmlParse(hilltop_data), "//Site", xmlValue)
# period <- xpathSApply(xmlParse(hilltop_data), paste0("//", t), xmlGetAttr, "Name")
#
# hilltop_df <- hilltop_data %>%
#   as_list() %>%
#   as_tibble() %>%
#   slice(-1) %>% # drop first node (Agency not required)
#   mutate(site = sites) %>%
#   unnest_longer("HilltopServer") %>%
#   filter(HilltopServer_id == t) %>%
#   select("HilltopServer", site) %>%
#   mutate(period = period) %>%
#   unnest_wider("HilltopServer") %>%
#   unnest(cols = names(.)) %>%
#   unnest(cols = names(.)) %>%
#   rename_all(., .funs = tolower) %>%
#   mutate(
#     site = as.factor(site),
#     period = factor(period, levels = period[1:12])
#   ) %>%
#   mutate_if(is.character, as.numeric) %>%
#   select(site, period, min:upperpercentile)
#
# ###################### Plotting
# ggplot(hilltop_df, aes(x = period, y = mean, group = site)) +
#   geom_col() +
#   geom_line(aes(x = period, y = lowerpercentile, group = site), color = "red") +
#   geom_line(aes(x = period, y = upperpercentile, group = site), color = "red") +
#   facet_wrap(~site) +
#   labs(x = "Month", y = "Mean [15th, 85th percentile] Rainfall (mm)") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
######################
