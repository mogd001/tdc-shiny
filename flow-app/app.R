library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
library(tdcR)
library(plotly)
library(shinybusy)
library(config)

source("functions.R")

config <- config::get()

site_choices <- get_collections() %>%
  filter(collection == "ActiveFlowSites") %>%
  arrange(site) %>%
  mutate(
    site_name = substring(site, 4)
  ) %>%
  pull(site_name)

site_choices <- append(c("All Sites"), site_choices)

default_start_date <- as.Date(force_tz(now(), tz = "")) - days(7) # Past 7 days
default_end_date <- as.Date(force_tz(now(), tz = ""))

sites <- get_sites() %>%
  mutate(
    site_name = substring(site, 4)
  ) %>%
  filter(site_name %in% site_choices)

flows <- get_flows(format(default_start_date, "%Y%m%d"), "Now")

summary <- flows %>%
  group_by(site) %>%
  summarise(
    max_flow = max(flow_m3ps, na.rm = TRUE),
    mean_flow = mean(flow_m3ps, na.rm = TRUE)
  )

sites <- sites %>%
  left_join(summary, by = "site")

basemap <- function(map_data, longitude, latitude, zoom) {
  tPrefix <- "http://tiles-a.data-cdn.linz.govt.nz/services;key="
  key <- config$linzkey
  tMid <- "/tiles/v4/layer="
  tSuffix <- "/EPSG:3857/{z}/{x}/{y}.png"

  Topo50_template <- paste0(tPrefix, key, tMid, 50767, tSuffix)
  leaflet(map_data) %>%
    setView(longitude, latitude, zoom) %>%
    addTiles(urlTemplate = Topo50_template, group = "NZ Topo50")
}

# Define UI ----
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  titlePanel(h1(id = "title-panel", "Flow App"), "Flow App"),
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      span(tags$a(img(src = "tdc_logo.png", width = 250), href = "https://www.tasman.govt.nz/", target = "_blank")),
      br(),
      h3(id = "side-bar-title", "Flow App"),
      em(id = "version", "v0.1"),
      br(),
      em(id = "developed-by", "developed by Matt Ogden (November 2022)"),
      h3("Information"),
      p("The purpose of this app is to visualise flow data managed by the TDC Hydrology team for various sites around the Tasman region."),
      p("Minimum date is: 2020-01-01."),
      br(),
      selectInput("site", "Select site", site_choices, selected = site_choices[1]),
      dateInput("start_date", "Select start date", value = default_start_date, min = "2020-01-01", max = default_end_date),
      dateInput("end_date", "Select end date", value = default_end_date, max = default_end_date),
      width = 2
    ),
    mainPanel(
      id = "main",
      tabsetPanel(
        tabPanel(
          "Dashboard",
          fluidRow(id = "mapoutput", column(12, leafletOutput("map", height = 800))),
          br(),
          fluidRow(column(12, plotlyOutput("plot")))
        ),
        tabPanel(
          "Table",
          fluidRow(column(12, tableOutput("table")))
        )
      ),
      add_busy_spinner(spin = "fading-circle"),
      height = 30
    )
  )
)


# Define server logic ----
server <- function(input, output, session) {
  flows <- reactiveVal(flows)

  map_data <- reactive({
    # return site data for mapping
    if (input$site == site_choices[1]) { # all sites
      list("sites" = sites, "longitude" = 173.1, "latitude" = -41.4, "zoom" = 9)
    } else {
      x <- sites %>%
        filter(site_name == input$site) %>%
        head(1)

      print(x)

      list("sites" = sites, "longitude" = x$longitude, "latitude" = x$latitude, "zoom" = 11)
    }
  })

  flow_data <- reactive({
    # return summary data for plotting, update flow data through the process.
    curr_min_date <- min(flows()$day)
    curr_max_date <- max(flows()$day)

    input_start_date <- force_tz(input$start_date, "NZ")
    input_end_date <- force_tz(input$end_date, "NZ")

    if (input_start_date < curr_min_date) {
      if (input$end_date == default_end_date) {
        updated_flows <- get_flows(from = format(input_start_date, "%Y%m%d"), to = "Now")
        flows(updated_flows)
      } else {
        updated_flows <- get_flows(from = format(input_start_date, "%Y%m%d"), to = format(input_end_date, "%Y%m%d"))
        flows(updated_flows)
      }
    }

    # filter displayed flow to between input start date and input end date
    if (input$site == site_choices[1]) { # all sites
      x <- flows() %>%
        filter(date >= input_start_date & date <= input_end_date)
    } else {
      x <- flows() %>%
        filter(site_name == input$site & date >= input_start_date & date <= input_end_date)
    }

    summary <- x %>%
      group_by(site_name) %>%
      summarise(
        mean_flow = mean(flow_m3ps, na.rm = TRUE),
        max_flow = max(flow_m3ps, na.rm = TRUE)
      )

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
        opacity = 1, fillOpacity = 0.7,
        radius = 1000,
        label = ~site_name,
        labelOptions = labelOptions(
          textsize = "18px",
          opacity = 0.9,
          noHide = FALSE
        )
      )
  })

  output$plot <- renderPlotly({
    p <- ggplot(flow_data()$flow) +
      geom_line(aes(datetime, flow_m3ps, color = site_name), alpha = 0.5) +
      theme_bw() +
      labs(x = "Datetime (NZST)", y = "Flow (m3/s)", color = "Site", title = "Flow Summary")
    ggplotly(p)
  })

  output$table <- renderTable(flow_data()$summary)
}


# Run the app ----
shinyApp(ui = ui, server = server)
