library(shiny)
library(shinyWidgets)
library(shinytitle)
library(shinybusy)
library(leaflet)

source("pre_launch.R")

# Define UI ----
ui <- fluidPage(
  title = "MAPP",
  use_shiny_title(),
  busy_window_title(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  h1(id = "title-panel", "MAPP"),
  fluidRow(
    column(
      12,
      leafletOutput("map", height = "100vh")
    )
  ),
  add_busy_spinner(
    spin = "breeding-rhombus",
    position = "top-left",
    height = 50,
    margins = c("50%", "50%")
  )
)


# Define server logic ----
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = topo_50_template, group = "NZ Topo50") %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      # add catchments
      addPolygons(
        group = "Catchments",
        data = catchments,
        fillColor = "blue",
        weight = 2,
        opacity = 1,
        color = "blue",
        fillOpacity = 0.2,
        labelOptions = labelOptions(noHide = FALSE, direction = "auto")
      ) %>%
      addLabelOnlyMarkers(
        group = "Catchments",
        data = catchment_centroids,
        label = ~ as.character(catchment),
        labelOptions = labelOptions(
          noHide = TRUE, direction = "top", textOnly = TRUE,
          style = list(
            "color" = "blue",
            "font-family" = "serif",
            "font-style" = "bold",
            # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
            "font-size" = "15px"
            # "border-color" = "rgba(0,0,0,0.5)"
          )
        )
      ) %>%
      # add rivers
      addPolylines(
        group = "Rivers",
        data = rivers,
        weight = 1
      ) %>%
      # add sites
      addCircleMarkers(
        group = "Sites",
        data = sites,
        radius = 5,
        color = "red",
        stroke = FALSE,
        fillOpacity = 1,
        label = ~ as.character(site),
        # clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = FALSE),
        labelOptions = labelOptions(noHide = FALSE, direction = "auto", style = list(
          "color" = "red",
          "font-family" = "serif",
          "font-size" = "15px"
        ))
      ) %>%
      # Layers control
      addLayersControl(
        baseGroups = c("NZ Topo50", "OSM (default)", "Toner", "Toner Lite"),
        overlayGroups = c("Catchments", "Rivers", "Sites"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)
