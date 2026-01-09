library(DBI)
library(dbplyr)
library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Lab 9 shiny app"),
  sidebarLayout(
    sidebarPanel(
      selectInput("theme", "Select a theme", choices = NULL),
      sliderInput("no_parts", "No. of parts", min = 1, max = 10, value = 5, step = 1)
    ),
    mainPanel(
      tableOutput("filtered_sets")
    )
  )
)
server <- function(input, output, session) {
  con <- dbConnect(RSQLite::SQLite(), "../data/lego.sqlite")

  sets <- tbl(con, "sets")
  themes <- tbl(con, "themes")

  choices <- sets |>
    distinct(theme_id) |> inner_join(themes, join_by(theme_id == id)) |> distinct(name) |> collect() |> pull(1) 
  optim_no_parts <- sets |> summarize(min_parts = min(num_parts), max_parts = max(num_parts)) |> collect()
  max_parts <- optim_no_parts$max_parts
  min_parts <- optim_no_parts$min_parts

  observeEvent(TRUE, {
    updateSelectInput(session,"theme", choices = choices)
    updateSliderInput(session,"no_parts", min = min_parts, max = max_parts, value = max_parts)
  },once = TRUE)
  filt_sets <- reactive({
    sets |> inner_join(themes, join_by(theme_id == id)) |> 
      filter(name.y == input$theme & num_parts == input$no_parts) |> collect()
  })

  output$filtered_sets <- renderTable({
    filt_sets()
  })
  session$onSessionEnded(\() dbDisconnect(con))
}

shinyApp(ui, server)
