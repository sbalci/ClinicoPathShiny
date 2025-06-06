age_pyramid_ui <- function(id) {

  ns <- NS(id)
  tagList(
    titlePanel("Age Pyramid Analysis"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("age_var"), "Age Variable", choices = NULL),
        selectInput(ns("gender_var"), "Gender Variable", choices = NULL),
        selectInput(ns("female_level"), "Select Female Level", choices = NULL), # Updated by server
        numericInput(ns("bin_width"), "Bin Width for Age Groups", value = 5, min = 1, step = 1),
        textInput(ns("plot_title"), "Plot Title", value = "Age Pyramid")
      ),
      mainPanel(
        plotOutput(ns("age_pyramid_plot")),
        hr(),
        h4("Summary Table"),
        tableOutput(ns("age_pyramid_table"))
      )
    )
  )
}
