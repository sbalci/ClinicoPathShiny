agreement_server <- function(id, H_loaded_data, H_column_names_reactive_arg) {
  moduleServer(id, function(input, output, session) {

    observe({
      column_choices <- H_column_names_reactive_arg()
      updateSelectInput(session, "vars", choices = column_choices)
    })

    agreement_instance_reactive <- reactive({
      req(H_loaded_data(), input$vars, length(input$vars) >= 2)

      current_data <- H_loaded_data()
      if(is.null(current_data) || nrow(current_data) == 0) return(NULL)

      opts <- list(
        vars = input$vars,
        wght = input$wght,
        exct = input$exct,
        heatmap = input$heatmap,
        heatmapDetails = input$heatmap_details, # Ensure R6 class uses this name
        kripp = input$kripp,
        krippMethod = input$kripp_method, # Ensure R6 class uses this name
        bootstrap = input$bootstrap_ci # Ensure R6 class uses this name
      )

      instance <- AgreementLogic$new(data = current_data, options = opts)
      run_success <- instance$run_analysis() # Assuming R6 class has a renamed top-level run method

      if(run_success) {
        return(instance)
      } else {
        # Optionally, handle run_success == FALSE (e.g. show a notification)
        return(NULL)
      }
    })

    output$contingency_table_output <- renderPrint({
      instance <- agreement_instance_reactive()
      req(instance, instance$raw_contingency_table)
      print(instance$raw_contingency_table)
    })

    output$grouped_counts_table_output <- renderTable({
      instance <- agreement_instance_reactive()
      req(instance, instance$grouped_counts_df)
      instance$grouped_counts_df
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$irr_kappa_table_output <- renderTable({
      instance <- agreement_instance_reactive()
      req(instance, instance$irr_summary_df)
      instance$irr_summary_df
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$kripp_alpha_table_output <- renderTable({
      instance <- agreement_instance_reactive()
      # This table should only render if Kripp alpha was calculated
      req(instance, instance$options$kripp, instance$kripp_summary_df)
      instance$kripp_summary_df
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$agreement_heatmap_plot <- renderPlot({
      instance <- agreement_instance_reactive()
      # This plot should only render if heatmap was requested and plot object exists
      req(instance, instance$options$heatmap, instance$heatmap_plot_object)
      print(instance$heatmap_plot_object)
    })
  })
}
