library(shiny)
library(shinytitle)
library(leaflet)
library(tidyverse)
library(lubridate)
library(tdcR)
library(plotly)
library(shinybusy)
library(config)
library(glue)
library(sf)

source("functions.R")
source("pre_launch.R")

# Define UI ----
ui <- fluidPage(
  title = "Rainfall App",
  use_shiny_title(),
  busy_window_title(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  h1(id = "title-panel", "Rainfall App"),
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      span(tags$a(img(src = "tdc_logo.png", width = "100%"), href = "https://www.tasman.govt.nz/", target = "_blank")),
      br(),
      h3(id = "side-bar-title", "Rainfall App"),
      em(id = "version", "v0.1"),
      br(),
      em(id = "developed-by", "developed by Matt Ogden (January 2022)"),
      h3("Information"),
      p("The purpose of this app is to visualise rainfall data managed by the TDC Hydrology team for various sites around the Tasman region."),
      p("Minimum date is: 2020-01-01."),
      br(),
      selectInput("site", "Select site", site_choices, selected = site_choices[1]),
      selectInput("time_select", "Time selection option", time_select_choices, selected = time_select_choices[1]),
      #uiOutput("relative_time_select"),
      #uiOutput("absolute_time_select"),
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
  rainfall <- reactiveVal(rainfall)

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
#
#   rainfall_data <- reactive({
#     # return summary data for plotting, update rainfall data through the process.
#     curr_min_date <- min(rainfall()$date)
#     curr_max_date <- max(rainfall()$date)
#
#     input_start_date <- force_tz(input$start_date, "NZ")
#     input_end_date <- force_tz(input$end_date, "NZ")
#
#     if (input_start_date < curr_min_date) {
#       if (input$end_date == default_end_date) {
#         updated_rainfall <- get_rainfall(from = format(input_start_date, "%Y%m%d"), to = "Now")
#         rainfall(updated_rainfall)
#       } else {
#         updated_rainfall <- get_rainfall(from = format(input_start_date, "%Y%m%d"), to = format(input_end_date, "%Y%m%d"))
#         rainfall(updated_rainfall)
#       }
#     }
#
#     # filter displayed rainfall to between input start date and input end date
#     if (input$site == site_choices[1]) { # all sites
#       r <- rainfall() %>%
#         filter(date >= input_start_date & date <= input_end_date)
#     } else {
#       r <- rainfall() %>%
#         filter(site_name == input$site & date >= input_start_date & date <= input_end_date)
#     }
#
#     summary <- r %>%
#       group_by(site) %>%
#       summarise(
#         total_rainfall = sum(rainfall_total_mm, na.rm = TRUE)
#       )
#
#     sites <- sites %>%
#       select(site, site_name, latitude, longitude) %>%
#       left_join(summary, by = "site") %>%
#       mutate(
#         label = glue("{site_name} - {round(total_rainfall,0)} mm")
#       )
#
#     summary <- sites %>%
#       select(site_name, total_rainfall) %>%
#       arrange(desc(total_rainfall))
#
#     list("rainfall" = r, "summary" = summary)
#   })

  output$map <- renderLeaflet(basemap(map_data()$sites, map_data()$longitude, map_data()$latitude, map_data()$zoom))
#
#   # when map_data changes
#   observe({
#     # add markers to map
#     leafletProxy("map", data = map_data()$sites) %>%
#       addTiles(urlTemplate = topo_50_template, group = "NZ Topo50") %>%
#       addTiles(group = "OSM (default)") %>%
#       addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
#       addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
#       # add catchments
#       addPolygons(
#         group = "Catchments",
#         data = catchments,
#         fillColor = "blue",
#         weight = 2,
#         opacity = 1,
#         color = "blue",
#         fillOpacity = 0.1,
#         labelOptions = labelOptions(noHide = FALSE, direction = "auto")
#       ) %>%
#       addLabelOnlyMarkers(
#         group = "Catchments",
#         data = catchment_centroids,
#         label = ~ as.character(catchment),
#         labelOptions = labelOptions(
#           noHide = TRUE, direction = "top", textOnly = TRUE,
#           style = list(
#             "color" = "blue",
#             "font-family" = "serif",
#             "font-style" = "bold",
#             # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
#             "font-size" = "15px"
#             # "border-color" = "rgba(0,0,0,0.5)"
#           )
#         )
#       ) %>%
#       # add rivers
#       addPolylines(
#         group = "Rivers",
#         data = rivers,
#         weight = 1
#       ) %>%
#       clearShapes() %>%
#       addCircles(
#         lng = ~longitude, lat = ~latitude,
#         weight = 2, color = "black", fillColor = "blue",
#         opacity = 1, fillOpacity = 0.7,
#         radius = ~total_rainfall*20,
#         label = ~label,
#         labelOptions = labelOptions(
#           textsize = "18px",
#           opacity = 0.9,
#           noHide = FALSE
#         )
#       ) %>%
#       # Layers control
#       addLayersControl(
#         baseGroups = c("Toner Lite", "NZ Topo50", "OSM (default)", "Toner"),
#         overlayGroups = c("Catchments", "Rivers", "Sites"),
#         options = layersControlOptions(collapsed = TRUE)
#       )
#   })

  # output$plot <- renderPlotly({
  #   p <- ggplot(rainfall_data()$rainfall) +
  #     geom_col(aes(datetime, rainfall_total_mm, fill = site_name), alpha = 0.5) +
  #     theme_bw() +
  #     labs(x = "Datetime (NZST)", y = "Rainfall (mm)", color = "Site", title = "Rainfall Summary")
  #   ggplotly(p)
  # })

  #output$table <- renderTable(rainfall_data()$summary)
}


# Run the app ----
shinyApp(ui = ui, server = server)
