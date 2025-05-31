brief_summary_server <- function(id, H_loaded_data, H_column_names_reactive) {
  moduleServer(id, function(input, output, session) {

    observe({
      updateSelectizeInput(session, "IDCols", choices = H_column_names_reactive(), selected = NULL)
    })

    output$dfsummary <- renderPrint({
      req(H_loaded_data())
      data_to_summarize <- H_loaded_data()

      if (!is.null(input$IDCols) && length(input$IDCols) > 0) {
        # Ensure selected columns exist in the data before trying to remove
        cols_to_remove <- intersect(input$IDCols, names(data_to_summarize))
        if (length(cols_to_remove) > 0) {
          data_to_summarize <- data_to_summarize %>% dplyr::select(-dplyr::one_of(cols_to_remove))
        }
      }

      if (ncol(data_to_summarize) == 0) {
        return("No columns selected or all columns removed.")
      }

      summary(data_to_summarize)
    })
  })
}
