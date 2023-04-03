library(tidyverse)
library(bslib)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinybusy)
library(thematic)
library(lubridate)

source("functions.R")
source("load_data.R")
source("generate_visuals.R")

default_date <- as.Date(now(), tz = "")

start_date <- default_date - days(3)
end_date <- default_date

site_catchment <- select(tibble(sites), c(site, catchment))

flows <- get_flows(format(start_date, "%Y%m%d"), "Now", site_catchment)
rainfall <- get_rainfall(format(start_date, "%Y%m%d"), "Now", site_catchment)

catchment_names <- levels(sites$catchment)
flow_sites <- unique(flows$site_name)
rainfall_sites <- unique(rainfall$site_name)

flow_thresholds <- read_csv("data/20230402_flow_thresholds.csv")

map <- generate_map(catchments, sites, flow_sites, rainfall_sites)
p_rainsummary <- generate_rainfall_summary_plot(rainfall, sites)
p_flowrain <- NULL


# UI
ui <- dashboardPage(
  skin = "blue",
  title = "TDC Flood Watch",

  # HEADER ------------------------------------------------------------------
  dashboardHeader(
    title = span(tags$a(img(src = "tdc_logo.png", height = 45), href="https://www.tasman.govt.nz/", target="_blank"), ""),
    titleWidth = 350,
    tags$li(
      class = "dropdown",
      a("Rainfall",
        href = "https://www.tasman.govt.nz/my-region/environment/environmental-data/rainfall/report/",
        target = "_blank",
        style = "cursor: pointer;"
      )
    ),
    tags$li(
      class = "dropdown",
      a("Flow",
        href = "https://www.tasman.govt.nz/my-region/environment/environmental-data/river-flow/flow-report/",
        target ="_blank",
        style = "cursor: pointer;"
      )
    ),
    tags$li(
      class = "dropdown",
      a(img(src = "R_logo.png", height = 20),
        href = "https://www.r-project.org/",
        target ="_blank",
        style = "cursor: pointer;"
      )
    )
  ),

  # SIDEBAR -----------------------------------------------------------------
  dashboardSidebar(
    width = 350,
    h3(id = "side-bar-title", "Flood Watch Portal"),
    em(id = "version", "v0.2"),
    br(),
    em(id = "developed-by", "developed by TDC Environmental Data (last updated 3/04 April 2023)"),
    h3(id = "information", "Information"),
    p(id = "purpose", "The purpose of this app is to support TDC Flood Warning functions."),
    airDatepickerInput("start_date", "Start",
                       value = force_tz(default_date, tz = "NZ") - days(3),
                       maxDate = force_tz(end_date, tz = "NZ"), dateFormat = "yyyy MM dd", view = "days",  minView = "days",
                       autoClose = TRUE
    ),
    airDatepickerInput("end_date", "End",
                       value = force_tz(default_date, tz = "NZ"),
                       maxDate = force_tz(default_date, tz = "NZ"), dateFormat = "yyyy MM dd", view = "days",  minView = "days",
                       autoClose = TRUE
    ),
    pickerInput("catchment", "Catchment",
                choices = catchment_names, selected = catchment_names, multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Clear",
                  `select-all-text` = "All",
                  `none-selected-text` = "No catchment selected"
                )
    ),
    pickerInput("rainfall_site", "Rainfall site",
                choices = rainfall_sites, selected = NULL, multiple = TRUE,
                options = list(
                  `actions-box` = FALSE,
                  `deselect-all-text` = "Clear",
                  `select-all-text` = "All",
                  `none-selected-text` = "No rainfall site selected",
                  "max-options" = 1
                )
                
    ),
    pickerInput("flow_site", "Flow site",
                choices = flow_sites, selected = NULL, multiple = TRUE,
                options = list(
                  `actions-box` = FALSE,
                  `deselect-all-text` = "Clear",
                  `select-all-text` = "All",
                  `none-selected-text` = "No flow site selected",
                  "max-options" = 1
                )
    ),
    br(),
    actionButton("update", "Update"),
    br()#,
    #downloadButton("download", "Download")
  ),
  
  # BODY -----------------------------------------------------------------
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    fluidRow(
      column(6, id = 'col1', 
        div(id = "map-title", "Map"),
        leafletOutput("map", height = "800px"),
      ),
      column(6, id = 'col2', 
         tabsetPanel(
           tabPanel("Rainfall Summary", plotOutput("rainfall_summary_plot"), height = "400px"),
           tabPanel("Rainfall Table", dataTableOutput('rainfall_table'),  height = "400px"),
           tabPanel("Flow Table", dataTableOutput('flow_table'),  height = "400px")
         ),
         plotOutput("plot",  height = "400px"),
      )
    ), 
    add_busy_spinner(#spin = "fading-circle",
      spin = "breeding-rhombus",
      position = "top-left",
      height = 30,
      margins = c("130px", "400px")
    )
  )
)


# Server
server <- function(input, output, session) {
  start_date <- reactiveVal(start_date)
  end_date <- reactiveVal(end_date)
  
  flows <- reactiveVal(flows)
  rainfall <- reactiveVal(rainfall)
  
  map <- reactiveVal(map)
  p_rainsummary <- reactiveVal(p_rainsummary)
  p_flowrain <- reactiveVal(p_flowrain)
  
  observeEvent(input$start_date, {
    if (input$start_date - input$end_date > 0) {
      updateAirDateInput(
        session = session,
        inputId = "start_date",
        value = input$end_date
      )
      start_date(input$end_date)
    } else {
      start_date(input$start_date)
    }
  })
  
  observeEvent(input$end_date, {
    if (input$end_date - input$start_date < 0) {
      # set end_date to start_date
      updateAirDateInput(
        session = session,
        inputId = "end_date",
        value = input$start_date
      )
      end_date(input$start_date)
    } else {
      end_date(input$end_date)
    }
  })
  
  observeEvent(input$catchment, {
    # filter sites based on catchment selection
    isolate({
      if (is.null(input$catchment)) {
        sites_updated <- sites
      } else {
        sites_updated <- subset(sites, catchment %in% input$catchment)
      }

      flow_sites <- flow_sites[flow_sites %in% sites_updated$site_name]
      rainfall_sites <- rainfall_sites[rainfall_sites %in% sites_updated$site_name]
      
      updatePickerInput(
        session,
        "flow_site",
        choices = flow_sites
      )
      
      updatePickerInput(
        session,
        "rainfall_site",
        choices = rainfall_sites,
      )
    })
  })
  
  observeEvent(input$update, {
    isolate({
      
      if (end_date() == default_date) {
        flows(get_flows(format(start_date(), "%Y%m%d"), "Now", site_catchment))
        rainfall(get_rainfall(format(start_date(), "%Y%m%d"), "Now", site_catchment))
      } else {
        flows(get_flows(format(start_date(), "%Y%m%d"), format(end_date(), "%Y%m%d"), site_catchment))
        rainfall(get_rainfall(format(start_date(), "%Y%m%d"), format(end_date(), "%Y%m%d"), site_catchment))
      }
      # subset catchments, flow site and rainfall site
      if (!is.null(input$catchment)) {
        catchments <- subset(catchments, catchment %in% input$catchment)
      }
      
      flows(subset(flows(), catchment %in% input$catchment))
      rainfall(subset(rainfall(), catchment %in% input$catchment))

      if (!is.null(input$flow_site)) {
        flow_sites <- input$flow_site
      }
      if (!is.null(input$rainfall_site)) {
        rainfall_sites <- input$rainfall_site
      }
      map(generate_map(catchments, sites, flow_sites, rainfall_sites))
      
      if (!is.null(input$rainfall_site)) {
        rainfall(subset(rainfall(), site_name == input$rainfall_site))
        p_rainsummary(generate_rainfall_summary_plot(rainfall(), sites))
      }
          
      if (!is.null(input$rainfall_site) & !is.null(input$flow_site)) {
        flows(subset(flows(), site_name == input$flow_site))
        p_flowrain(generate_rainfall_flow_for_site_plot(flows(), rainfall(), input$start_date, input$end_date, flow_thresholds))
      } else{
        p_rainsummary(generate_rainfall_summary_plot(rainfall(), sites))
      }
    })
  })
  
  output$map <- renderLeaflet({
    map()
  })
  
  output$rainfall_summary_plot <- renderPlot({
    p_rainsummary()
  }, res = 90)
  
  output$plot <- renderPlot({
    p_flowrain()
  })
  
  output$rainfall_table <- renderDataTable(
    rainfall() %>% select(site_name, datetime, rainfall_total_mm),
    options = list(
      pageLength = 5
    )
  )
  
  output$flow_table <- renderDataTable(
    flows() %>% select(site_name, datetime, flow_m3ps),
    options = list(
      pageLength = 5
    )
  )
}


shinyApp(ui = ui, server = server)