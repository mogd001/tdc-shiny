library(shiny)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  titlePanel(h1(id = "title-panel", "TDC Environmental Data"), "TDC Environmental Data"),
  mainPanel(
    id = "main",
    fluidRow(
      span(id = "tdc-logo", tags$a(img(src = "tdc_logo.png", width = "50%"), href = "https://www.tasman.govt.nz/", target = "_blank"))
    ),
    fluidRow(
      p("Click a link below to go to a specific tool"),
      h2(id = "flood-watch-app", tags$a(href = "https://tdc-environmental.shinyapps.io/flood-watch-app/", "Flood Watch Portal", target = "_blank")),
      h2(id = "rainfall-month-summary-app", tags$a(href = "https://tdc-environmental.shinyapps.io/rainfall-month-summary-app/", "Rainfall Summary", target = "_blank")),
      h2(id = "flow-app", tags$a(href = "https://tdc-environmental.shinyapps.io/flow-app/", "Flow", target = "_blank")),
      h2(id = "hirds-app", tags$a(href = "https://tdc-environmental.shinyapps.io/hirds-app/", "Hirds", target = "_blank"))
    ), 
    fluidRow(
      p("PowerBI example integration"),
      htmlOutput("frame")
    )
  )
)

server <- function(input, output) {
  
  output$frame <- renderUI({
    tags$iframe(src="https://app.powerbi.com/view?r=eyJrIjoiZWJiYjMyOWQtZjNkNi00YTI3LTk0ZDQtMTY1MWQ0NDNmMDQxIiwidCI6IjFhNGMwYzk4LThkMmMtNDc4ZC1iM2QwLThiYjhmZmNkNDU1NCJ9", height = 600, width = "100%")
  })

}

shinyApp(ui = ui, server = server)
