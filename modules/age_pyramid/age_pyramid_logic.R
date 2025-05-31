# Adapted from user-provided agepyramidClass
AgePyramidLogic <- R6::R6Class(
  "AgePyramidLogic",
  public = list(
    data = NULL,
    options = list(), # Shiny inputs will be mapped here

    # Store results directly
    plot_data_for_table = NULL,
    plot_object = NULL,

    initialize = function(data, options) {
      self$data <- data
      self$options <- options
    },

    run = function() {
      # Check if required options are provided from Shiny inputs
      if (is.null(self$options$age_var) || self$options$age_var == "" ||
          is.null(self$options$gender_var) || self$options$gender_var == "" ||
          is.null(self$options$female_level) || self$options$female_level == "") {
        # message_user("Required inputs (age, gender, female level) are not all selected.", continue_working=FALSE) # Cannot message user from here
        print("Required inputs (age, gender, female level) for Age Pyramid are not all selected.")
        return(FALSE) # Indicate failure or incomplete setup
      }

      if (nrow(self$data) == 0) {
        print("Data for Age Pyramid contains no (complete) rows")
        return(FALSE)
      }

      # Read and prepare data ----
      mydata_logic <- self$data # Use a different name to avoid confusion with self$data

      age_col_name <- self$options$age_var
      gender_col_name <- self$options$gender_var

      # Select and clean the required columns
      # Replacing jmvcore::select and jmvcore::naOmit
      required_cols <- c(age_col_name, gender_col_name)
      if (!all(required_cols %in% names(mydata_logic))) {
        print("Age or Gender column not found in data for Age Pyramid.")
        return(FALSE)
      }

      mydata_logic <- mydata_logic[, required_cols, drop = FALSE]
      mydata_logic <- tidyr::drop_na(mydata_logic, dplyr::all_of(required_cols))


      if (nrow(mydata_logic) == 0) {
        print("Data becomes empty after removing NAs from Age/Gender columns for Age Pyramid.")
        return(FALSE)
      }

      # Convert age to numeric and gender to factor
      # Replacing jmvcore::toNumeric
      mydata_logic[[age_col_name]] <- as.numeric(mydata_logic[[age_col_name]])
      mydata_logic[[gender_col_name]] <- as.factor(mydata_logic[[gender_col_name]])

      # Check for conversion issues
      if (all(is.na(mydata_logic[[age_col_name]]))) {
          print(paste("Age column", age_col_name, "could not be converted to numeric or all values are NA."))
          return(FALSE)
      }


      # Create a standardized gender variable based on the selected 'female' level
      mydata_logic <- mydata_logic %>%
        dplyr::mutate(
          Gender2 = dplyr::case_when(
            .data[[gender_col_name]] == self$options$female_level ~ "Female",
            TRUE ~ "Male" # Assuming binary gender for pyramid
          )
        )

      # Determine bin width (default to 5 years if not provided) ----
      bin_width <- if (!is.null(self$options$bin_width) && is.numeric(self$options$bin_width) && self$options$bin_width > 0) self$options$bin_width else 5

      max_age <- max(mydata_logic[[age_col_name]], na.rm = TRUE)
      if (is.infinite(max_age) || is.na(max_age)) {
          print("Max age is NA or infinite after processing for Age Pyramid.")
          return(FALSE)
      }

      breaks_seq <- seq(from = 0, to = max_age, by = bin_width)
      if (max_age > tail(breaks_seq, n = 1)) {
        breaks_seq <- c(breaks_seq, max_age) # Ensure max_age is included
      }
      # Ensure unique breaks if max_age is a multiple of bin_width or very close
      breaks_seq <- unique(breaks_seq)
      if(length(breaks_seq) < 2) breaks_seq <- c(0, max_age)


      mydata_logic[["Pop"]] <- cut(mydata_logic[[age_col_name]],
                                 include.lowest = TRUE,
                                 right = TRUE, # Standard for age groups
                                 breaks = breaks_seq,
                                 ordered_result = TRUE)

      if (all(is.na(mydata_logic[["Pop"]]))) {
          print("Population bins ('Pop') could not be created. Check age data and bin width for Age Pyramid.")
          return(FALSE)
      }

      # Prepare data for plotting and table output ----
      plotData_for_pyramid <- mydata_logic %>%
        dplyr::select(Gender = Gender2, Pop) %>%
        dplyr::filter(!is.na(Pop)) %>% # Filter out NA Pop categories that might arise from cut() if data is sparse
        dplyr::group_by(Gender, Pop) %>%
        dplyr::count() %>%
        dplyr::ungroup()

      if (nrow(plotData_for_pyramid) == 0) {
        print("No data available for plotting after processing for Age Pyramid.")
        return(FALSE)
      }

      # Save data for plot rendering (mimics image$setState)
      # self$results$plot_data_for_pyramid <- plotData_for_pyramid # Storing directly

      # Pivot data for table output ----
      plotData_for_table_temp <- plotData_for_pyramid %>%
        tidyr::pivot_wider(names_from = Gender,
                           values_from = n,
                           values_fill = list(n = 0)) %>%
        dplyr::arrange(dplyr::desc(Pop)) %>%
        # dplyr::filter(!is.na(Pop)) %>% # Already filtered
        dplyr::mutate(Pop = as.character(Pop))

      # Store for table output
      self$plot_data_for_table <- as.data.frame(plotData_for_table_temp)

      # Generate plot object directly
      self$generate_plot_object(plotData_for_pyramid) # Call to generate and store plot

      return(TRUE) # Indicate success
    },

    generate_plot_object = function(plot_data_arg) { # Renamed from .plot to avoid conflict if R6 has private .plot
      # Retrieve the prepared plot data
      # plotData_for_pyramid <- self$results$plot_data_for_pyramid # From stored data
      plotData_for_pyramid <- plot_data_arg

      if (is.null(plotData_for_pyramid) || nrow(plotData_for_pyramid) == 0) {
        print("Plot data is null or empty for Age Pyramid.")
        self$plot_object <- ggplot2::ggplot() + ggplot2::ggtitle("No data to display") # Return empty plot
        return()
      }

      # Ensure that the age bins (Pop) reflect the latest bin width:
      # Convert 'Pop' to character then back to factor with the order of appearance.
      # plotData_for_pyramid$Pop <- factor(as.character(plotData_for_pyramid$Pop), levels = unique(as.character(plotData_for_pyramid$Pop)))
      # Using levels from the original cut ensures correct ordering
      plotData_for_pyramid$Pop <- factor(plotData_for_pyramid$Pop, levels = levels(plotData_for_pyramid$Pop))


      # Set plot title (using user option if provided)
      plot_title <- if (!is.null(self$options$plot_title) && self$options$plot_title != "") self$options$plot_title else "Age Pyramid"

      # Create a visually appealing age pyramid plot ----
      # Ensure 'n' is numeric
      plotData_for_pyramid$n <- as.numeric(plotData_for_pyramid$n)

      max_n <- max(plotData_for_pyramid$n, na.rm = TRUE)
      if(is.na(max_n) || is.infinite(max_n) || max_n == 0) max_n <- 1 # handle edge cases for limits

      plot <- ggplot2::ggplot(data = plotData_for_pyramid,
                              mapping = ggplot2::aes(
                                  x = Pop,
                                  y = ifelse(Gender == "Female", -n, n),
                                  fill = Gender
                              )) +
          ggplot2::geom_col(width = 0.7, color = "black", show.legend = TRUE) +
          ggplot2::coord_flip() +
          ggplot2::scale_y_continuous(labels = abs,
                                      limits = c(-max_n, max_n)
          ) +
          ggplot2::labs(x = "Age Group",
                        y = "Population Count",
                        title = plot_title,
                        fill = "Gender") +
          ggplot2::theme_minimal(base_size = 14) + # Increased base_size for better readability
          ggplot2::theme(
              plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size=ggplot2::rel(1.2)),
              axis.text = ggplot2::element_text(size = ggplot2::rel(0.9)),
              axis.title = ggplot2::element_text(size = ggplot2::rel(1.0)),
              legend.position = "bottom",
              legend.text = ggplot2::element_text(size=ggplot2::rel(0.9))
          )

      self$plot_object <- plot
    }
  )
)
