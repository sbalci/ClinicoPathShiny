# Select Data to Read With Button ----
button_val <- reactiveVal("init")

observeEvent(input$test_Button, {
  new_val <- "test"
  button_val(new_val)
})

observeEvent(input$excel_Button, {
  new_val <- "excel"
  button_val(new_val)
})

observeEvent(input$spss_Button, {
  new_val <- "sav"
  button_val(new_val)
})

# Assuming a CSV button sets button_val to "csv"
# If server_csv_ui.R or similar handles this, this might not be needed here.
# For now, adding it for completeness of button_val logic.
observeEvent(input$csv_Button, {
  button_val("csv")
})


output$value55 <- renderText({
  button_val()
})


read_data <- reactiveVal() # This is the old reactiveVal

observe({
  current_button_val <- button_val()
  data_to_load <- NULL

  # Need to ensure uploaded_csv_data, uploaded_excel_data, etc. are available
  # These are defined in server_read_data.R, which is sourced in app.R's server function
  # testdata() is from server_reactiveValues.R (ultimately from server0_data.R)

  if (current_button_val == "test" || current_button_val == "init") {
    # Assuming testdata() is reactive and available here, ultimately providing exampleData
    # exampleData is loaded in app.R's global scope via server0_data.R initially.
    # For H_main_data, we should use the actual exampleData if "test" is chosen.
    # server0_data.R (sourced at app startup) creates 'mydata' and 'exampleData'
    # server_reactiveValues.R creates testdata() reactive which is 'mydata'
    # So testdata() here should be exampleData or mydata.
    # The subtask: H_main_data(exampleData) for test data.
    # If testdata() directly returns exampleData, this is fine.
    # Let's assume testdata() is the correct reactive for the example dataset.
    data_to_load <- tryCatch(testdata(), error = function(e) NULL)
  } else if (current_button_val == "csv") {
    data_to_load <- tryCatch(uploaded_csv_data(), error = function(e) NULL)
  } else if (current_button_val == "excel") {
    data_to_load <- tryCatch(uploaded_excel_data(), error = function(e) NULL)
  } else if (current_button_val == "sav") {
    data_to_load <- tryCatch(uploaded_spss_data(), error = function(e) NULL)
  }

  if (!is.null(data_to_load)) {
    read_data(data_to_load)    # Update old reactiveVal for compatibility if needed
    H_main_data(data_to_load)  # Update new global reactiveVal
  } else if (current_button_val != "init") {
    # Avoid clearing H_main_data on init if nothing is loaded yet,
    # but if a button was pressed and data_to_load is still NULL (e.g. from tryCatch error),
    # then clear H_main_data.
    H_main_data(NULL) # Clear if a specific load attempt failed or was not covered
  }
})


# observe({
#   
#   button <- button_val()
#   
#   mydata <- switch (button,
#     "test" = testdata(),
#     "init" = testdata(),
#     "excel" = uploaded_excel_data(),
#     "sav" = uploaded_spss_data()
#   )
#   
#   read_data(mydata)
# 
# })




# Names Columns for inputs ----

# All Names ----

observe({
  mydata <- read_data()
  names_data <- names(mydata)
  updateSelectizeInput(session,
                       'IDCols',
                       choices = names_data,
                       server = TRUE)
})


# Func: Get Categorical Variables ----
observe({
  mydata <- read_data()
  catVar1 <-
    names(which(# is.factor(mydata)
      sapply(mydata, class) == "character"))
  updateSelectInput(session,
                    'survfactor',
                    choices = catVar1)
})


observe({
  mydata <- read_data()
  # numericVars <- names(get_numeric_variables(mydata))
  numericVars <- names(mydata)
  updateSelectInput(session,
                    "x_variable",
                    choices = numericVars,
                    selected = numericVars[1]
  )
  
  updateSelectInput(session,
                    "y_variable",
                    choices = numericVars,
                    selected = numericVars[2]
  )
  
  cat_vars <-
    names(get_category_variables(mydata))
  
  updateSelectInput(session,
                    "color_variable",
                    choices = cat_vars,
                    selected = cat_vars[1])
  
  updateSelectInput(session,
                    "size_variable",
                    choices = cat_vars,
                    selected = cat_vars[1])
})


observe({
  
  mydata <- read_data()
  # numericVars <- names(get_numeric_variables(mydata))
  numericVars_cor <- names(mydata)
  
  updateSelectizeInput(session,
                       "corr_mat_Cols",
                       choices = numericVars_cor,
                       server = TRUE
  )
})


