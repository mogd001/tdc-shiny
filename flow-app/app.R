library(shiny) # for interactivity
library(leaflet) # for mapping
library(tidyverse)
library(lubridate)
library(tdcR)

site_choices <- get_collections() %>%
  filter(collection == "ActiveFlowSites") %>%
  arrange(site) %>%
  pull(site)

site_choices <- append(c("All Sites"), site_choices)

# site_choices <- c(
#   "All Sites", "HY Motueka at Gorge", "HY Anatoki at Happy Sams",
#   "HY Aorere at Devils Boots", "HY Motueka at Woodmans Bend",
#   "HY Wangapeka at Walter Peak", "HY Tadmor at Mudstone",
#   "HY Motupiko at Christies", "HY Waimea at TDC Nursery"
# )

sites <- get_sites() %>%
  filter(site %in% site_choices)

default_start_date <- ymd("2022-06-01")
default_end_date <- as.Date(force_tz(now(), tz = ""))

get_flow_data <- function(from = format(default_start_date, "%Y%m%d"), to = "") {
  get_data(
    collection = "ActiveFlowSites", method = "Average",
    time_interval = NA, from = from, to = "", interval = "1 hour", alignment = "00:00"
  ) %>%
    rename(flow = value) %>%
    filter(site %in% site_choices) %>%
    group_by(site) %>%
    slice(-1) %>%
    ungroup()
}

flows <- get_flow_data()

summary <- flows %>%
  group_by(site) %>%
  summarise(max_flow = max(flow, na.rm = TRUE),
            mean_flow = mean(flow, na.rm = TRUE))

sites <- sites %>%
  left_join(summary, by = "site")

# Rainfall basemap function used in shiny app
basemap <- function(map_data, longitude, latitude, zoom) {
  # build LINZ URL template for map background
  tPrefix <- "http://tiles-a.data-cdn.linz.govt.nz/services;key="
  key <- "5ede30255f754cee9fb2c53963274636" # my LINZ web service key
  tMid <- "/tiles/v4/layer="
  tSuffix <- "/EPSG:3857/{z}/{x}/{y}.png"
  # NZ Topo250 (layer id = 798)
  Topo250_template <- paste0(tPrefix, key, tMid, 798, tSuffix)

  leaflet(map_data) %>%
    # increase zoom to zoom in
    setView(longitude, latitude, zoom) %>%
    # add LINZ layer
    addTiles(urlTemplate = Topo250_template, group = "NZ Topo250")
}

# Define UI ----
ui <- fluidPage(
  titlePanel("Flow App"),
  sidebarLayout(
    sidebarPanel(
      img(src = "tdc_logo.png", width = 250),
      br(),
      em("developed by Matt Ogden (July 2022)"),
      h3("Information"),
      p("The purpose of this app is to visualise flow data managed by the TDC Hydrology team for various sites around the Tasman region."),
      p("Minimum date is: 2020-01-01."),
      br(),
      selectInput("site", "Select site", site_choices, selected = site_choices[1]),
      dateInput("start_date", "Select start date", value = "2022-06-01", min = "2020-01-01", max = default_end_date),
      dateInput("end_date", "Select end date", value = default_end_date, max = default_end_date)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Dashboard",
          fluidRow(column(10, leafletOutput("map"))),
          fluidRow(column(12, plotOutput("plot")))
        ),
        tabPanel("Table",
          fluidRow(column(12, tableOutput("table")))
        )
      )
    )
  )
)


# Define server logic ----
server <- function(input, output, session) {


  map_data <- reactive({
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

  flows <- reactiveVal(flows)

  flow_data <- reactive({
    # return summary data for plotting, update flow data through the process
    curr_min_date <- min(flows()$day)
    curr_max_date <- max(flows()$day)

    input_start_date <- force_tz(input$start_date, "NZ")
    input_end_date <- force_tz(input$end_date, "NZ")

    if (input_start_date < curr_min_date) {
      updated_flows <- get_flow_data(from = format(input_start_date, "%Y%m%d"), to = "")
      flows(updated_flows)
    }

    # filter displayed flow to between input start date and input end date
    if (input$site == site_choices[1]) { # all sites
      x <- flows() %>%
        filter(date >= input_start_date & date <= input_end_date)
    } else {
      x <- flows() %>%
        filter(site == input$site & date >= input_start_date & date <= input_end_date)
    }

    summary <- x %>%
      group_by(site) %>%
      summarise(mean_flow = mean(flow, na.rm = TRUE),
                max_flow = max(flow, na.rm = TRUE))

    list("flow" = x, "summary" = summary)

  })

  output$map <- renderLeaflet(basemap(map_data()$sites, map_data()$longitude, map_data()$latitude, map_data()$zoom))

  # when map_data changes
  observe({
    # add markers to map
    leafletProxy("map", data = map_data()$sites) %>%
      clearShapes() %>%
      addCircles(
        lng = ~longitude, lat = ~latitude,
        weight = 2, color = "black", fillColor = "blue",
        opacity = 0.9, fillOpacity = 0.7,
        radius = 500,
        label = ~site, # ~as.character(flow_disp)
        labelOptions = labelOptions(
          textsize = "8px",
          opacity = 0.9,
          noHide = FALSE
        )
      )
  })

  output$plot <- renderPlot(
    {
      ggplot(flow_data()$flow) +
        geom_line(aes(datetime, flow, color = site), alpha = 0.5) +
        theme_bw() +
        labs(x = "Datetime (NZST)", y = "Flow (m3/s)", color = "Site", title = "Flow Summary", subtitle = "Flow for all sites or single selected site")
    },
    res = 96
  )

  output$table <- renderTable(flow_data()$summary)
}


# Run the app ----
shinyApp(ui = ui, server = server)
