library(shiny)
library(shinyWidgets)
library(shinybusy)
library(leaflet)
library(nominatimlite)

source("pre_launch.R")

# Define UI ----
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  titlePanel(h1(id = "title-panel", "STIV Data Prep"), "Stiv Data Prep"),
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      span(tags$a(img(src = "tdc_logo.png", width = "100%"), href = "https://www.tasman.govt.nz/", target = "_blank")),
      br(),
      h3(id = "side-bar-title", "STIV Data Prep"),
      em(id = "version", "v0.1"),
      br(),
      em(id = "developed-by", "developed by TDC Environmental Data (January 2023)"),
      h3("Information"),
      p("Tool to support Space Time Image Velocimetry (STIV) data processing."),
      width = 3
    ),
    mainPanel(
      id = "main",
      fluidRow(
        column(
          4,
          selectizeInput("site_input", label = "Site", selected = "No site chosen", choices = sites$site, options = list(create = TRUE, placeholder = "Enter your site")),
          verbatimTextOutput("site_selected")
        ),
        column(
          8,
          leafletOutput("map")
        )
      ),
      fluidRow(
        column(
          4,
          fileInput("adcp_input", label = h3("ADCP file input"), accept = ".csv"),
          dataTableOutput("adcp_table")
        ),
        column(
          8,
          fileInput("rtk_input", label = h3("RTK file input"), accept = ".csv"),
          dataTableOutput("rtk_table")
        )
      ),
      add_busy_spinner(
        spin = "breeding-rhombus",
        position = "top-left",
        height = 50,
        margins = c("50%", "50%")
      )
    )
  )
)


# Define server logic ----
server <- function(input, output, session) {
  selected_site <- reactiveVal(NULL)
  sites_subset <- reactiveVal(sites)

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

  observeEvent(input$site_input, {
    if (is.null(selected_site())){
      selected_site("No site chosen")
    } else {
      selected_site(input$site_input)
    }
  })

  observeEvent(input$map_bounds, {
    xmin <- input$map_bounds$west
    xmax <- input$map_bounds$east
    ymin <- input$map_bounds$south
    ymax <- input$map_bounds$north

    bounding_box <- nominatimlite::bbox_to_poly(c(xmin, ymin, xmax, ymax))

    sites_subset(sites %>% st_filter(bounding_box))
  })

  observe({
    updateSelectizeInput(session, "site_input",
      label = "Site",
      selected = selected_site(),
      choices = append(sites_subset()$site, selected_site()),
      options = list(create = TRUE, placeholder = "Enter your site")
    )
  })

  output$site_selected <- renderText({
    selected_site()
  })

  adcp_input <- reactive({
    req(input$adcp_input)
    read.csv(input$adcp_input$datapath,
      stringsAsFactors = FALSE
    )
  })

  rtk_input <- reactive({
    req(input$rtk_input)
    read.csv(input$rtk_input$datapath,
      stringsAsFactors = FALSE
    )
  })

  output$adcp_table <- renderDataTable(
    adcp_input(),
    options = list(
      paging = FALSE,
      searching = FALSE,
      scrollX = FALSE,
      scrollY = "250px",
      dom = "t"
    )
  )


  output$rtk_table <- renderDataTable(
    rtk_input(),
    options = list(
      paging = FALSE,
      searching = FALSE,
      scrollX = TRUE,
      scrollY = "250px",
      dom = "t"
    )
  )
}


# Run the app ----
shinyApp(ui = ui, server = server)
