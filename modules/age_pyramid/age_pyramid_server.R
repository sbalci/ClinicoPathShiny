age_pyramid_server <- function(id, H_loaded_data, H_column_names_reactive_arg) {
  moduleServer(id, function(input, output, session) {

    observe({
      column_choices <- H_column_names_reactive_arg()
      updateSelectInput(session, "age_var", choices = column_choices, selected = column_choices[1])
      updateSelectInput(session, "gender_var", choices = column_choices, selected = column_choices[2])
    })

    # Update choices for 'female_level' based on selected gender_var
    observe({
      req(H_loaded_data(), input$gender_var)
      data <- H_loaded_data()
      gender_col <- input$gender_var
      if (gender_col %in% names(data)) {
        unique_levels <- unique(na.omit(data[[gender_col]]))
        updateSelectizeInput(session, "female_level", choices = unique_levels, selected = unique_levels[1])
      } else {
        updateSelectizeInput(session, "female_level", choices = character(0))
      }
    })

    # Reactive for AgePyramidLogic instance
    age_pyramid_instance <- reactive({
      req(H_loaded_data(), input$age_var, input$gender_var, input$female_level) # Ensure essential inputs are available

      current_data <- H_loaded_data()
      # Ensure data is not empty
      if(is.null(current_data) || nrow(current_data) == 0) return(NULL)

      # Map Shiny inputs to options list for R6 class
      opts <- list(
        age_var = input$age_var,
        gender_var = input$gender_var,
        female_level = input$female_level,
        bin_width = input$bin_width,
        plot_title = input$plot_title
      )

      # Create and run the R6 class instance
      instance <- AgePyramidLogic$new(data = current_data, options = opts)
      success <- instance$run() # run() now returns TRUE/FALSE
      if(success) {
        return(instance)
      } else {
        return(NULL) # Return NULL if run failed
      }
    })

    # Render the plot
    output$age_pyramid_plot <- renderPlot({
      instance <- age_pyramid_instance()
      if (!is.null(instance) && !is.null(instance$plot_object)) {
        print(instance$plot_object)
      } else {
        # Return a blank plot with a message if something went wrong
        ggplot2::ggplot() +
          ggplot2::labs(title = "Age pyramid plot could not be generated.", subtitle="Please check data and options.") +
          ggplot2::theme_void()
      }
    })

    # Render the table
    output$age_pyramid_table <- renderTable({
      instance <- age_pyramid_instance()
      if (!is.null(instance) && !is.null(instance$plot_data_for_table)) {
        instance$plot_data_for_table
      } else {
        data.frame(Message = "Table data could not be generated.")
      }
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

  })
}
