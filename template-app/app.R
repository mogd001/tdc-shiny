library(tidyverse)
library(bslib)
library(shiny)
library(shinydashboard)
library(thematic)


library(lubridate)
default_month <- month(as.Date(now()) - months(1))
deafult_year <- year(as.Date(now()) - months(1))
default_date <- ym(paste0(deafult_year, "-", default_month))

# UI
ui <- dashboardPage(
  skin = "black",
  title = "Environmental Data TDC",

  # HEADER ------------------------------------------------------------------
  dashboardHeader(
    title = span(tags$a(img(src = "tdc_logo.png", height = 45), href="https://www.tasman.govt.nz/", target="_blank"), "Data Portal"),
    titleWidth = 500
  ),

  # SIDEBAR -----------------------------------------------------------------
  dashboardSidebar(
    width = 400,
    h3("Title"),
    em("developed by TDC Evnironmental Data"),
    p("The purpose of this app is to...."),
    br(),
    dateInput("date_input", "Year month",
              value = default_date,
              max = default_date, format = "yyyy MM", startview = "month"
    ),
    br(),
    actionButton("actionbutton", "Update"),
    br(),
    downloadButton("download", "Download"),
    br(),
    actionButton("actionbutton2", "Update2")
  ),
  
  # BODY -----------------------------------------------------------------
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    fluidRow(
      column(6,
             plotOutput("plot")
      ),
      column(6,
             plotOutput("plot2")
      )
    ), 
    br(),
    fluidRow(
      column(6,
             plotOutput("plot3")
      ),
      column(6,
             plotOutput("plot4")
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    ggplot()
  })
  
  output$plot2 <- renderPlot({
    ggplot()
  })
  
  output$plot3 <- renderPlot({
    ggplot()
  })
  
  output$plot4 <- renderPlot({
    ggplot()
  })

}

shinyApp(ui = ui, server = server)