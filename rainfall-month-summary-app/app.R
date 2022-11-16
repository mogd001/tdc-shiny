library(shiny)
library(shinyWidgets)
library(shinybusy)

library(leaflet)
library(leaflet.opacity)

library(tidyverse)
library(lubridate)
library(glue)
library(tdcR)
library(sf)
library(sp)
library(plotly)
library(ggmap)
library(gstat)
library(automap)
library(raster)
library(car)
library(classInt)
library(RStoolbox)
library(spatstat)
library(dismo)
library(fields)
library(gridExtra)
library(Hmisc)
library(patchwork)
library(tmaptools)
library(comprehenr)

source("functions.R")

register_google(key = "AIzaSyBEj-P3pT3wWs9UazXX0ccrl5XzRO8MvM0")
# has_google_key()

# Set defaults and load initial data
default_month <- month(as.Date(now()) - months(1))
deafult_year <- year(as.Date(now()) - months(1))
default_date <- ym(paste0(deafult_year, "-", default_month))
default_ym_display <- format(default_date, "%Y %B")
max_rainfall <- 1000
nelsontasman <- st_read("data/context.gpkg", layer = "nelsontasman")

grid <- read_csv("data/elevation_pts.csv") %>%
  rename(
    easting = X,
    northing = Y,
    elevation = New_Zealand_Elevation
  )
coordinates(grid) <- ~ easting + northing
crs(grid) <- CRS("+init=epsg:2193")

nelsontasman <- st_read("data/context.gpkg", layer = "nelsontasman")

context <- st_read("data/context.gpkg", layer = "towns", query = "SELECT * FROM \"towns\" WHERE name <> 'Brightwater'")
basemap <- get_map(location = c(lon = 172.83031417, lat = -41.40166015), maptype = "terrain-background", zoom = 8, alpha = 0.3) # Richmond, NZ (alternative for getting location)

tdc_endpoint <- "http://envdata.tasman.govt.nz/data.hts?"
mdc_endpoint <- "http://hydro.marlborough.govt.nz/mdc data.hts?"
wgrc_endpoint <- "http://hilltop.wcrc.govt.nz/Websitedata.hts?"

tdc_sites <- get_site_data(endpoint = tdc_endpoint) %>% mutate(region = "nelsontasman")
mdc_sites <- get_site_data(endpoint = mdc_endpoint) %>% mutate(region = "marlborough")
wgrs_sites <- get_site_data(endpoint = wgrc_endpoint) %>% mutate(region = "westcoast")
sites <- bind_rows(tdc_sites, mdc_sites, wgrs_sites)

from <- format(default_date, "%Y%m%d")
to <- format(default_date + months(1), "%Y%m%d")

rainfall <- get_rainfall_data_all_sites(sites, from, to)
r <- rasterise_rainfall(rainfall, nelsontasman, grid)
map <- generate_rainfall_summary_plot(default_date, rainfall, r, max_rainfall, nelsontasman, context, basemap)
map_ly <- generate_rainfall_summary_plotly(default_date, rainfall, r, max_rainfall, nelsontasman, context, basemap)
y_m <- paste0("Monthly Rainfall Summary ", format(default_date, "%B %Y"))


# ui----
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  titlePanel(h1(id = "title-panel", "TDC Environmental Data"), "TDC Environmental Data"),
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      span(tags$a(img(src = "tdc_logo.png", width = 200), href = "https://www.tasman.govt.nz/", target = "_blank")),
      br(),
      h3(id = "side-bar-title", "Rainfall Monthly Summary"),
      em(id = "version", "v0.1"),
      br(),
      em(id = "developed-by", "developed by Matt Ogden (November 2022)"),
      h3("Information"),
      p("The purpose of this app is to summarise monthly rainfall totals for the Top of the South."),
      br(),
      airDatepickerInput("date_input", "Year month",
        value = default_date,
        maxDate = default_date, dateFormat = "yyyy MMMM", view = "months",  minView = "months",
        autoClose = TRUE
      ),
      actionButton("actionbutton", "Update"),
      p(""),
      downloadButton("download", "Download"),
      width = 2
    ),
    mainPanel(
      id = "main",
      fluidRow(
        textOutput("year_month_title")
      ),
      fluidRow(
        column(
          6, style = "padding: 0px 0px;",
          plotlyOutput("map_interactive", height = 600)
        ),
        column(
          6, style = "padding: 0px 0px;height: 600px",
          plotOutput("map", height = 600)
        )
      ),
      # fluidRow(
      #   column(
      #     6,
      #     plotOutput("plot", width = "auto")
      #   ),
      #   column(
      #     6,
      #     plotOutput("plot2", width = "auto")
      #   )
      # ),
      add_busy_spinner(spin = "fading-circle"),
      height = 30
    )
  )
)


# server ----
server <- function(input, output, session) {
  y_m <- reactiveVal(y_m)
  map_ly <- reactiveVal(map_ly)
  map <- reactiveVal(map)

  output$year_month_title <- renderText({
    y_m()
  })

  output$map_interactive <- renderPlotly(
    {
      map_ly()
    })
  
  output$map <- renderPlot(
    {
      map()
    }, width = 600, res = 75)
  
  # output$plot <- renderPlot({ggplot()})
  # output$plot2 <- renderPlot({ggplot()})
  
  observeEvent(input$actionbutton, {
    isolate({
      d <- ymd(substr(input$date_input[1], 1, 10))
      print(d)
      m <- month(d)
      y <- year(d)
      d <- ym(paste0(y, "-", m))

      from <- format(d, "%Y%m%d")
      to <- format(d + months(1), "%Y%m%d")

      rainfall <- get_rainfall_data_all_sites(sites, from = from, to = to)
      r <- rasterise_rainfall(rainfall, nelsontasman, grid)
      map(generate_rainfall_summary_plot(d, rainfall, r, max_rainfall, nelsontasman, context, basemap))
      map_ly(generate_rainfall_summary_plotly(d, rainfall, r, max_rainfall, nelsontasman, context, basemap))
      y_m(paste0("Monthly Rainfall Summary ", format(d, "%B %Y")))
    })
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(year(ymd(substr(input$date_input[1], 1, 10))), "-", month(ymd(substr(input$date_input[1], 1, 10))), ".png")
    },
    contentType = "image/jpeg",
    content = function(file) {
      ggsave(file, plot = map(), device = "png", width = 6.2, height = 9.085, dpi = 300)
    }
  )
}


# app ----
shinyApp(ui = ui, server = server)
