agreement_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Interrater Reliability Analysis"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("vars"), "Variables (select 2 or more rater columns)",
                    choices = NULL,
                    multiple = TRUE),
        hr(),
        h4("Cohen's Kappa / Fleiss' Kappa Options"),
        selectInput(ns("wght"), "Weighting (for 2 raters Cohen's Kappa)",
                    choices = c("unweighted", "equal", "squared"),
                    selected = "unweighted"),
        checkboxInput(ns("exct"), "Exact Kappa (for >=3 raters Fleiss' Kappa - provides CI)",
                      value = FALSE),
        hr(),
        h4("Heatmap Options"),
        checkboxInput(ns("heatmap"), "Show Agreement Heatmap", value = TRUE),
        checkboxInput(ns("heatmap_details"), "Show Kappa on Heatmap (if heatmap shown)", value = FALSE),
        hr(),
        h4("Krippendorff's Alpha Options"),
        checkboxInput(ns("kripp"), "Calculate Krippendorff's Alpha", value = FALSE),
        selectInput(ns("kripp_method"), "Alpha: Level of Measurement",
                    choices = c("nominal", "ordinal", "interval", "ratio"),
                    selected = "nominal"),
        checkboxInput(ns("bootstrap_ci"), "Bootstrap CI for Alpha (can be slow)", value = FALSE) # Renamed from 'bootstrap' to avoid conflict
      ),
      mainPanel(
        h4("Contingency Table (Raw Counts)"),
        verbatimTextOutput(ns("contingency_table_output")),
        hr(),
        h4("Grouped Counts Table"),
        tableOutput(ns("grouped_counts_table_output")),
        hr(),
        h4("Interrater Reliability Statistics (Kappa)"),
        tableOutput(ns("irr_kappa_table_output")),
        hr(),
        conditionalPanel(
            condition = paste0("input['", ns("kripp"), "']"), # Condition based on Kripp checkbox
            h4("Krippendorff's Alpha Statistics"),
            tableOutput(ns("kripp_alpha_table_output"))
        ),
        hr(),
        conditionalPanel(
            condition = paste0("input['", ns("heatmap"), "']"), # Condition based on Heatmap checkbox
            h4("Agreement Heatmap"),
            plotOutput(ns("agreement_heatmap_plot"))
        )
      )
    )
  )
}
