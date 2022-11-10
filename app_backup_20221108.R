library(dplyr) # for data manipulation
library(lubridate) # for converting dates
library(shiny) # for interactivity
library(leaflet) # for mapping
library(leaflet.opacity)
library(tdcR)

# Load data
site_choices <- get_collections() %>%
  filter(collection == "Rainfall") %>%
  arrange(site) %>%
  filter(site != "HY Wakapuaka Combined") %>%
  pull(site)

site_choices <- append(c("All Sites"), site_choices)

sites <- get_sites() %>%
  filter(site %in% site_choices)

default_start_date <- as.Date(force_tz(now(), tz = "")) - days(7) # Past 7 days
default_end_date <- as.Date(force_tz(now(), tz = ""))

interval_dt <- minutes(15)

get_rainfall_data <- function(from = format(default_start_date, "%Y%m%d"), to = "") {
  get_data_collection(collection = "Rainfall", method = "Total", time_interval = NA, from = from, to = "", interval = "15 minutes", alignment = "00:00") %>%
    rename(rainfall = value) %>%
    group_by(site) %>%
    arrange(datetime) %>%
    slice(-1) %>% # remove first row due to offset, datetime refers to start of interval.
    ungroup() %>%
    mutate(
      interval = interval_dt
    ) %>%
    filter(!is.na(rainfall))
}


update_sites <- function(rainfalll, sd, ed) {
  # Function to update rainfall totals for each site based on user selection
  summary <- rainfall %>%
    filter(date >= sd & date <= ed) %>%
    group_by(site) %>%
    summarise(total = sum(rainfall, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      start_date = sd,
      end_date = ed
    )

  sites <- sites %>%
    select(site, latitude, longitude) %>%
    left_join(summary, by = "site")
}


rainfall <- get_rainfall_data()
sites <- update_sites(rainfall, default_start_date, default_end_date)


# summarise monthly rainfall
# monthly_rainfall <- rainfall %>%
#   group_by(site, year_month) %>%
#   summarise(rainfall_total = round(sum(rainfall, na.rm = TRUE), 2)) %>%
#   mutate(
#     month = month(year_month, label = TRUE),
#     year = year(year_month)
#   ) %>%
#   ungroup() %>%
#   left_join(sites, by = "site") %>%
#   filter(!is.na(year) | !is.na(month))

# min_date <- min(monthly_rainfall$year_month)
# max_date <- max(monthly_rainfall$year_month)

# basemap function used in shiny app
basemap <- function(map_data, longitude, latitude, zoom) {
  # build LINZ URL template for map background
  tPrefix <- "http://tiles-a.data-cdn.linz.govt.nz/services;key="
  key <- "5ede30255f754cee9fb2c53963274636" # my LINZ web service key
  tMid <- "/tiles/v4/layer="
  tSuffix <- "/EPSG:3857/{z}/{x}/{y}.png"
  # NZ Topo250 (layer id = 798)
  topo250_template <- paste0(tPrefix, key, tMid, 798, tSuffix)

  leaflet(map_data) %>%
    setView(longitude, latitude, zoom) %>%
    addTiles() %>%
    addTiles(urlTemplate = topo250_template, group = "NZ Topo250", layerId = "bg")
}

# Define UI for shiny app
ui <- fluidPage(
  # Application title
  titlePanel("Rainfall App"),
  sidebarLayout(
    sidebarPanel(
      img(src = "tdc_logo.png", width = 250),
      br(),
      em("developed by Matt Ogden (July 2022)"),
      h3("Information"),
      p("The purpose of this app is to visualise rainfall data managed by the TDC Hydrology team for various sites around the Tasman region."),
      br(),
      selectInput("site", "Select site", site_choices, selected = site_choices[1]),
      dateRangeInput("daterange", "Date range",
        start = default_start_date, end = default_end_date,
        min = "2022-01-01", max = default_end_date
      )
    ),

    # Chosen category
    mainPanel(
      # set map height relative to window size
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      # rainfall map using leaflet
      leafletOutput("map")
    )
  )
)

# Define server logic for shiny app
server <- function(input, output, session) {
  rainfall <- reactiveVal(rainfall)
  # rainfall data
  rainfall_data <- reactive({
    curr_min_date <- min(rainfall()$day)
    curr_max_date <- max(rainfall()$day)
    input_start_date <- force_tz(input$daterange[1], "NZ")
    input_end_date <- force_tz(input$daterange[2], "NZ")

    if (input_start_date < curr_min_date) {
      updated_rainfall <- get_rainfall_data(from = format(input$daterange[1], "%Y%m%d"), to = "")
      rainfall(updated_rainfall)
    }

    # Filter to date range to calculate total
    x <- rainfall() %>%
      filter(date >= input_start_date & date <= input_end_date)

    sites <- update_sites(x, input_start_date, input_end_date)
  })

  # map data
  map_data <- reactive({
    input_start_date <- force_tz(input$daterange[1], "NZ")
    input_end_date <- force_tz(input$daterange[2], "NZ")

    x <- rainfall() %>%
      filter(date >= input_start_date & date <= input_end_date)
    sites <- update_sites(x, input_start_date, input_end_date)

    # return site data for mapping
    if (input$site == site_choices[1]) { # all sites
      list("sites" = sites, "longitude" = 173.1, "latitude" = -41.4, "zoom" = 9)
    } else {
      x <- sites %>%
        filter(site == input$site) %>%
        head(1)

      list("sites" = sites, "longitude" = x$longitude, "latitude" = x$latitude, "zoom" = 11)
    }
  })

  # set outputs
  output$map <- renderLeaflet(basemap(map_data()$sites, map_data()$longitude, map_data()$latitude, map_data()$zoom))

  # when map_data changes
  observe({
    # add markers to map
    leafletProxy("map", data = map_data()$sites) %>%
      clearShapes() %>%
      addCircles(
        lng = ~longitude, lat = ~latitude,
        weight = 2, color = "white", fillColor = "blue",
        opacity = 0.9, fillOpacity = 0.4,
        radius = ~ sqrt(total) * 80,
        label = ~ paste0(site, ": ", as.character(round(total, 0)), " mm \n[", start_date, ",", end_date, "]"), # ~as.character(rainfall_total),
        labelOptions = labelOptions(
          textsize = "15px",
          opacity = 0.9,
          noHide = FALSE
        )
      )
  })

  observe({
    rainfall_data()
  })
}

shinyApp(ui = ui, server = server)
