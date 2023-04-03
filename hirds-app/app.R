library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
library(tdcR)
library(plotly)
library(shinybusy)
library(config)
require(scales)
library(glue)

source("functions.R")

hirds_all <- readRDS("20221124_tdc_site_hird_data.rds")

site_choices <- tibble(site = names(hirds_all)) %>%
  mutate(
    site_name = substring(site, 4)
  ) %>%
  pull(site)

default_start_date <- as.Date(now(), tz = "NZ") - days(7) # Past 7 days
default_end_date <- as.Date(now(), tz = "NZ")

p <- ggplot()

# Other data components ----
nz_record <- tribble(
  ~duration, ~value,
  1/6, 34,
  1, 134,
  12, 566,
  24, 758,
  48, 1086,
  120, 1368
)

duraction_text_to_val <- tribble(
  ~duration_text, ~duration, ~duration_interval, ~duration_label,
  "dur_10min", 1 / 6, "10 minutes", "10 min",
  "dur_20min", 2 / 6, "20 minutes", "20 min",
  "dur_30min", 3 / 6, "30 minutes", "30 min",
  "dur_1hour", 1, "1 hour", "1 hour",
  "dur_2hour", 2, "2 hours", "2 hours",
  "dur_6hour", 6, "6 hours", "6 hours",
  "dur_12hour", 12, "12 hours", "12 hours",
  "dur_1day", 24, "24 hours", "1 day",
  "dur_2day", 48, "48 hours", "2 days",
  "dur_3day", 72, "72 hours", "3 days",
  "dur_4day", 96, "96 hours", "4 days",
  "dur_5day", 120, "120 hours", "5 days"
)

intervals <- duraction_text_to_val$duration_interval

# Define UI ----
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  h1(id = "title-panel", "Hirds App"),
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      span(tags$a(img(src = "tdc_logo.png", width = "100%"), href = "https://www.tasman.govt.nz/", target = "_blank")),
      br(),
      h3(id = "side-bar-title", "Hirds App"),
      em(id = "version", "v0.12"),
      br(),
      em(id = "developed-by", "developed by TDC Environmental Data (last updated 3/04 April 2023)"),
      h3("Information"),
      p("The purpose of this app is to summarise rainfall observations against NIWA Hirds."),
      br(),
      selectInput("site", "Select site", site_choices, selected = site_choices[1]),
      dateInput("start_date", "Select start date", value = default_start_date, max = default_end_date),
      dateInput("end_date", "Select end date", value = default_end_date, max = default_end_date),
      actionButton("updatebutton", "Update"),
      width = 3
    ),
    mainPanel(
      id = "main",
      fluidRow(id = "plot", column(12, plotlyOutput("plot"))),
      add_busy_spinner(spin = "fading-circle"),
      height = 30
    )
  )
)


# Define server logic ----
server <- function(input, output, session) {
  p <- reactiveVal(p)

  observeEvent(input$updatebutton, {
    isolate({
      site <- input$site
      from <- format(input$start_date, "%Y%m%d")
      to <- format(input$end_date, "%Y%m%d")

      hirds <- hirds_all[[site]]

      observed <- do.call(rbind, lapply(intervals, get_max_rainfall, site = site, from = from, to = to)) %>%
        left_join(duraction_text_to_val, by = "duration_interval") %>%
        mutate(
          ari_for_duration = unlist(map2(duration_label, value, determine_ari_duration, hirds)),
          ari_for_duration = if_else(ari_for_duration == -999, "<1.58",
            if_else(ari_for_duration == 999, ">100+", as.character(round(ari_for_duration, 1)))
          )
        )

      p1 <- ggplot() +
        geom_line(hirds, mapping = aes(duration, val, color = ari), linetype = "dashed") +
        geom_line(observed, mapping = aes(duration, value), color = "magenta", size = 1) +
        geom_line(nz_record, mapping = aes(duration, value, linetype = "NZ Record"), color = "red", size = 1) +
        geom_point(observed, mapping = aes(duration, value, linewidth = ari_for_duration), color = "magenta", size = 2.5) +
        scale_x_continuous(trans = "log10", breaks = duraction_text_to_val$duration, labels = duraction_text_to_val$duration_label) +
        scale_y_log_eng() +
        scale_color_viridis_d() +
        labs(
          x = "Duration", y = "Rainfall (mm)", color = "ARI (years)", linetype = NULL,
          title = glue(
            {
              site
            },
            " ",
            {
              from
            },
            "-",
            {
              to
            },
            " Rainfall"
          )
        ) +
        annotation_logticks(sides = "l") +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.grid.minor.x = element_blank()
        )

      # update plot
      p(p1)
    })
  })

  output$plot <- renderPlotly({
    ggplotly(p())
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)
