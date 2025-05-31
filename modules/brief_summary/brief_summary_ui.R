brief_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Data Summary"),
    sidebarLayout(
      sidebarPanel(
        p("Select Columns to Remove From Summary"),
        selectizeInput(
          inputId = ns("IDCols"),
          label = "Select ID Columns",
          choices = NULL, # Will be updated by server
          selected = NULL,
          multiple = TRUE
        )
      ),
      mainPanel(
        verbatimTextOutput(ns("dfsummary"))
      )
    )
  )
}
