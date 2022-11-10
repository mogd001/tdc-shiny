library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.opacity)

library(tidyverse)
library(lubridate)
library(glue)
library(tdcR)
library(sf)
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

library(shinybusy)

register_google(key = "AIzaSyBEj-P3pT3wWs9UazXX0ccrl5XzRO8MvM0")
# has_google_key()

# Set defaults
default_month <- month(as.Date(now()) - months(1))
deafult_year <- year(as.Date(now()) - months(1))
default_date <- ym(paste0(deafult_year, "-", default_month))
default_my_display <- format(default_date, "%Y %B")
max_rainfall <- 1000
nelsontasman <- st_read("data/context.gpkg", layer = "nelsontasman")


# Define functions
get_site_data <- function(endpoint = endpoint) {
  get_sites(endpoint = endpoint) %>%
    mutate(
      longitude_ = longitude,
      latitude_ = latitude
    ) %>%
    st_as_sf(coords = c("longitude_", "latitude_"), crs = 4326) %>%
    st_transform(crs = 2193) %>%
    mutate(
      easting = st_coordinates(.)[, "X"],
      northing = st_coordinates(.)[, "Y"]
    )
}

get_rainfall_monthly_data <- function(endpoint = endpoint, collection = "Rainfall", from = "", to = "", month = "") {
  get_data_collection(
    endpoint = endpoint, collection = collection, method = "Total", interval = "1 months",
    from = from, to = to
  ) %>%
    rename(rainfall_total = value) %>%
    group_by(site) %>%
    arrange(site, datetime) %>%
    # slice(-1) %>% # remove first row due to offset, datetime refers to start of interval.
    ungroup() %>%
    mutate(
      interval = months(1),
      rainfall_total = round(rainfall_total, digits = 2)
    ) %>%
    filter(!is.na(rainfall_total)) %>%
    group_by(site, month) %>%
    summarise(
      rainfall_avg_total = mean(rainfall_total, na.rm = TRUE),
      rainfall_med_total = median(rainfall_total, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(month == !!month)
}

get_rainfall_data_all_sites <- function(from = "", to = "") {
  tdc_month_rainfall <- get_rainfall_monthly_data(endpoint = tdc_endpoint, collection = "Rainfall", from = from, to = to, month = format(ymd(from), "%b"))
  mdc_month_rainfall <- get_rainfall_monthly_data(endpoint = mdc_endpoint, collection = "Rainfall2", from = from, to = to, month = format(ymd(from), "%b"))
  wgrs_month_rainfall <- get_rainfall_monthly_data(endpoint = wgrc_endpoint, collection = "WebRainfall", from = from, to = to, format(ymd(from), "%b"))

  month_rainfall <- dplyr::bind_rows(tdc_month_rainfall, mdc_month_rainfall, wgrs_month_rainfall) %>%
    dplyr::select(-c(month, rainfall_med_total)) %>%
    dplyr::rename(rainfall_total = rainfall_avg_total)

  rainfall_sites <- tibble(site = unique(month_rainfall$site))

  rainfall_sites %>%
    left_join(sites, by = "site") %>%
    left_join(month_rainfall, by = "site") %>%
    st_drop_geometry() %>%
    dplyr::select(-geometry) %>%
    drop_na() %>%
    subset(site != "Red Hills")
}

rasterise_rainfall <- function(rainfall, nelsontasman) {
  pts <- rainfall %>%
    transmute(
      site = site,
      rainfall_total = if_else(rainfall_total <= 0, 0.0001, rainfall_total), # account for 0 rainfall months
      x = easting,
      y = northing,
      rainfall_total = rainfall_total
    )

  coordinates(pts) <- ~ x + y
  crs(pts) <- CRS("+init=epsg:2193")
  pt_val <- powerTransform(pts$rainfall_total)$lambda

  rainfall_total.bc <- bcPower(pts$rainfall_total, pt_val)
  rainfall$rainfall_total.bc <- bcPower(pts$rainfall_total, pt_val)

  v.rainfall_total <- variogram(rainfall_total.bc ~ 1, data = pts, cloud = F)

  m.rainfall_total <- vgm(psill = max(v.rainfall_total$gamma) * 0.9, model = "Exp", range = max(v.rainfall_total$dist) / 2, nugget = mean(v.rainfall_total$gamma) / 4)
  # Least square fit
  m.f.rainfall_total <- fit.variogram(v.rainfall_total, m.rainfall_total)

  g <- gstat(NULL, id = "rainfall_total", form = rainfall_total.bc ~ 1, data = pts)

  v.cross <- variogram(g)

  g <- gstat(g, id = "rainfall_total", model = m.f.rainfall_total, fill.all = T)
  g <- fit.lmc(v.cross, g)

  Sys.sleep(2)
  ck <- stats::predict(g, grid)

  k1 <- 1 / pt_val
  ck$ck.pred <- ((ck$rainfall_total.pred * pt_val + 1)^k1)
  ck$ck.var <- ((ck$rainfall_total.var * pt_val + 1)^k1)

  ck.pred <- rasterFromXYZ(as.data.frame(ck)[, c("easting", "northing", "ck.pred")])
  # ck.var <- rasterFromXYZ(as.data.frame(ck)[, c("easting", "northing", "ck.var")])

  r <- raster::mask(x = ck.pred, mask = nelsontasman)
  raster::crs(r) <- "EPSG:2193"

  r
}

generate_rainfall_summary_plot <- function(month_year, rainfall, rainfall_raster, max_rainfall, nelsontasman, context) {
  nelsontasman_wgs84 <- st_transform(nelsontasman, crs = 4326)
  bb <- st_bbox(nelsontasman_wgs84)
  context_wgs84 <- st_transform(context, crs = 4326) %>%
    mutate(
      lon = st_coordinates(.)[, "X"],
      lat = st_coordinates(.)[, "Y"]
    )

  r_wgs84 <- rainfall_raster %>%
    projectRaster(crs = 4326)

  plot_r <- as_tibble(as.data.frame(r_wgs84, xy = TRUE)) %>%
    rename(ck_rainfall = ck.pred) %>%
    mutate(ck_rainfall = ifelse(ck_rainfall > max_rainfall, max_rainfall, ck_rainfall))

  pts <- rainfall %>% mutate(
    rainfall_total_capped = if_else(rainfall_total >= 1000, 1000, rainfall_total)
  )

  ggmap(basemap, darken = c(0.6, "white")) +
    coord_cartesian() +
    geom_raster(plot_r, mapping = aes(x = x, y = y, fill = ck_rainfall)) +
    geom_sf(nelsontasman_wgs84, mapping = aes(), fill = NA, color = "black", inherit.aes = FALSE) +
    geom_sf(context_wgs84, mapping = aes(), fill = NA, shape = 2, size = 2, color = "white", inherit.aes = FALSE) +
    geom_text(context_wgs84, mapping = aes(lon, lat, label = name), size = 2.5, color = "white", hjust = -0.18, fontface = "bold", alpha = 0.5) +
    geom_point(pts, mapping = aes(x = longitude, y = latitude), size = 1.5, alpha = 0.4, color = "black", shape = 3) +
    geom_text(pts, mapping = aes(x = longitude, y = latitude, label = round(rainfall_total, 0)), color = "black", hjust = -0.25, vjust = -0.25, size = 2.5) + # fontface = "bold"
    coord_sf(xlim = c(bb$xmin - 0.3, bb$xmax + 0.2), ylim = c(bb$ymin - 0.2, bb$ymax + 0.2)) +
    labs(fill = "Rainfall Total (mm)", title = glue('Monthly Rainfall Summary {format(month_year, "%B %Y")}')) +
    theme_bw() +
    scale_color_viridis(option = "turbo", limits = c(0, max_rainfall), alpha = 0.7) +
    scale_fill_viridis(option = "turbo", limits = c(0, max_rainfall), na.value = "transparent", alpha = 0.7) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.background = element_blank(),
      legend.position = c(0.15, 0.9)
    ) +
    guides(color = "none", size = none)
}


# Load initial data
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

rainfall <- get_rainfall_data_all_sites(from, to)
r <- rasterise_rainfall(rainfall, nelsontasman)
p <- generate_rainfall_summary_plot(default_date, rainfall, r, max_rainfall, nelsontasman, context)


# UI
ui <- fluidPage(
  tags$head(tags$style(
    HTML('

         #sidebar {
            background-color: #ffffff;
        }
        ')
  )),
  sidebarLayout(
    sidebarPanel(id="sidebar",
      h3("Rainfall Monthly Summary"),
      img(src = "tdc_logo.png", width = 250),
      br(),
      em("developed by Matt Ogden (November 2022)"),
      h3("Information"),
      p("The purpose of this app is to summarise monthly rainfall totals for the Top of the South."),
      br(),
      dateInput("date_input", "Year month",
        value = default_date,
        max = default_date, format = "yyyy MM", startview = "month"
      ),
      actionButton("actionbutton", "Update"),
      p(" "),
      downloadButton("download", "Download"),
      width = 2
    ),
    mainPanel(fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("plot_interactive"), plotOutput("plot"), )
    ),
    add_busy_spinner(spin = "fading-circle"),
    height = 20
    )
  )
)

# Server logic
server <- function(input, output, session) {
  p <- reactiveVal(p)

  output$plot <- renderPlot({
    p()
  }, width = 600 )

  output$plot_interactive <- renderPlotly({
    ggplotly(p())
  })

  observeEvent(input$actionbutton, {
    isolate({
      d <- input$date_input
      m <- month(d)
      y <- year(d)
      d <- ym(paste0(y, "-", m))

      from <- format(d, "%Y%m%d")
      to <- format(d + months(1), "%Y%m%d")

      rainfall <- get_rainfall_data_all_sites(from = from, to = to)
      r <- rasterise_rainfall(rainfall, nelsontasman)

      p(generate_rainfall_summary_plot(d, rainfall, r, max_rainfall, nelsontasman, context))
    })
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(year(input$date_input), "-", month(input$date_input), ".png")
    },
    contentType = "image/jpeg",
    content = function(file) {
      ggsave(file, plot = p(), device = "png", width = 6.2, height = 9.085, dpi = 300)
    }
  )
}

shinyApp(ui = ui, server = server)
