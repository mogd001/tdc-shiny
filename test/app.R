library(shiny)
library(shinyWidgets)

first <- 10
second <- 100

ui <- fluidPage(
  actionButton("first", label = "First"),  
  actionButton("second", label = "Second"),
  verbatimTextOutput("result"),
  
  radioGroupButtons(
    inputId = "Id062",
    direction = "vertical",
    label = "Label",
    choices = c("A", 
                "B", "C")
  )
  
)

server <- function(input, output, session) {
  results <- reactiveVal(0)
  
  observeEvent(input$first, {
    print(paste("Add",first))
    results(results() + first)
  })  
  
  observeEvent(input$second, {
    print(paste("Add",second))
    results(results() + second)
  })
  output$result <- renderText(paste0("Result: ",as.character(results())))
}

shinyApp(ui = ui, server = server)
