AgreementLogic <- R6::R6Class(
  "AgreementLogic",
  public = list(
    data = NULL,
    options = list(), # Shiny inputs will be mapped here

    # Public fields to store results
    raw_contingency_table = NULL,
    grouped_counts_df = NULL,
    irr_summary_df = NULL,
    kripp_summary_df = NULL,
    heatmap_plot_object = NULL,

    initialize = function(data, options) {
      self$data <- data
      self$options <- options

      # Ensure default values for options if not provided,
      # as Shiny inputs might not be available immediately.
      defaults <- list(
        vars = character(0),
        wght = "unweighted",
        exct = FALSE,
        heatmap = TRUE,
        heatmapDetails = FALSE,
        kripp = FALSE,
        krippMethod = "nominal",
        bootstrap = FALSE # Renamed from bootstrap_ci in UI to bootstrap in R6
      )
      self$options <- defaults # Start with defaults
      # Overwrite with user-provided options if they exist
      for (name in names(options)) {
        if (!is.null(options[[name]])) {
          self$options[[name]] <- options[[name]]
        }
      }
    },

    run_analysis = function() {
      if (length(self$options$vars) < 2) {
        print("Please select at least two rater variables.")
        return(FALSE)
      }

      selected_data <- self$data[, self$options$vars, drop = FALSE]
      selected_data <- tidyr::drop_na(selected_data) # Equivalent to na.omit for selected columns

      if (nrow(selected_data) == 0) {
        print("No complete rows after removing NAs from selected variables.")
        return(FALSE)
      }

      # Convert all selected columns to factor for agreement calculations
      for (var_name in self$options$vars) {
        selected_data[[var_name]] <- as.factor(selected_data[[var_name]])
      }

      # Contingency Table (raw counts)
      # For 2 raters, table() is fine. For >2, it's more complex if we want a single table.
      # The original code seemed to imply using irr::agree for counts.
      # irr::agree can produce counts for >2 raters.
      if (length(self$options$vars) == 2) {
         self$raw_contingency_table <- base::table(selected_data[[1]], selected_data[[2]])
      } else {
        # For more than 2 raters, a simple 2D contingency table isn't directly applicable for all pairs.
        # We'll show counts per rater per item or use irr::agree which is more for stats.
        # The original jmv code for `self$results$result3` used pivot_wider which is suitable for grouped_counts.
        # For raw contingency, perhaps show a summary of levels or first two raters.
        # For now, let's use irr::agree to get a sense of agreement counts.
        # Or, more simply, just print a message that for >2 raters, use grouped counts.
        # The original R code used `as.data.frame(table(d))` where d was selected_data, which works.
        self$raw_contingency_table <- as.data.frame(base::table(selected_data))

      }


      # Grouped Counts Table (similar to original result3)
      # This part seems to be what the original `self$results$result3` was doing.
      # It reshapes the data to long format, counts occurrences of each rating combination.
      # This is useful for understanding patterns of agreement/disagreement.
      if (ncol(selected_data) >= 2) {
          # Using reshape2::melt as an example if that was intended by original logic for "grouped counts"
          # Or more simply, using dplyr:
          tryCatch({
            self$grouped_counts_df <- selected_data %>%
              dplyr::group_by_all() %>%
              dplyr::summarise(Count = dplyr::n(), .groups = 'drop') %>%
              as.data.frame()
          }, error = function(e) {
            print(paste("Error creating grouped counts table:", e$message))
            self$grouped_counts_df <- data.frame(Error = e$message)
          })
      }


      # Kappa Statistics
      # Using irr package
      if (!requireNamespace("irr", quietly = TRUE)) {
        print("Package 'irr' is required for Kappa statistics but not installed.")
        self$irr_summary_df <- data.frame(Statistic="Kappa", Value=NA, Message="irr package not installed")
      } else {
        kappa_val <- NA
        kappa_ci <- c(NA, NA)
        n_raters <- length(self$options$vars)

        if (n_raters == 2) {
          kappa_result <- try(irr::kappa2(selected_data[, 1:2], weight = self$options$wght), silent = TRUE)
          if (!inherits(kappa_result, "try-error")) {
            kappa_val <- kappa_result$value
            # No CI directly from kappa2 for weighted, usually.
          } else {
            print(paste("Error calculating Cohen's Kappa:", as.character(kappa_result)))
          }
          stat_name <- paste0("Cohen's Kappa (", self$options$wght, ")")
        } else if (n_raters > 2) {
          # For Fleiss' Kappa, data needs to be a matrix of ratings (subjects x categories) or (subjects x raters)
          # The current `selected_data` is (subjects x raters).
          # irr::kappam.fleiss requires a matrix where rows are subjects and columns are raters.
          # Exact kappa (conf.level)
          kappa_result <- try(irr::kappam.fleiss(selected_data, exact = self$options$exct, conf.level=0.95), silent = TRUE)
          if (!inherits(kappa_result, "try-error")) {
            kappa_val <- kappa_result$value
            if(self$options$exct && !is.null(kappa_result$confid)) { # Check if confid is there
                 kappa_ci <- kappa_result$confid[1,] # Taking the first row if multiple CIs
            }
          } else {
            print(paste("Error calculating Fleiss' Kappa:", as.character(kappa_result)))
          }
          stat_name <- "Fleiss' Kappa"
        } else {
          stat_name <- "Kappa (N/A)"
        }

        self$irr_summary_df <- data.frame(
          Statistic = stat_name,
          Value = round(kappa_val, 3),
          Lower_CI = round(kappa_ci[1], 3),
          Upper_CI = round(kappa_ci[2], 3),
          N_Raters = n_raters,
          N_Subjects = nrow(selected_data)
        )
      }


      # Krippendorff's Alpha
      if (self$options$kripp) {
        if (!requireNamespace("irr", quietly = TRUE)) {
          print("Package 'irr' is required for Krippendorff's Alpha but not installed.")
          self$kripp_summary_df <- data.frame(Statistic="Kripp. Alpha", Value=NA, Message="irr package not installed")
        } else {
          # irr::kripp.alpha requires data in a specific format: matrix (raters x subjects)
          # or (subjects x raters) depending on interpretation. The documentation says (subjects x raters).
          kripp_data <- as.matrix(selected_data)
          kripp_result <- try(irr::kripp.alpha(kripp_data, method = self$options$krippMethod), silent = TRUE)

          alpha_val <- NA
          if (!inherits(kripp_result, "try-error")) {
            alpha_val <- kripp_result$value
          } else {
            print(paste("Error calculating Krippendorff's Alpha:", as.character(kripp_result)))
          }

          # Bootstrap CI (simplified example, actual bootstrapping is more involved)
          # The irr package's kripp.alpha doesn't directly provide bootstrap CIs.
          # This would typically require the 'boot' package and custom resampling.
          # For this adaptation, we'll just note if bootstrap was requested.
          ci_method_note <- ""
          if(self$options$bootstrap) {
            ci_method_note <- "(Bootstrap CI requested - not implemented in this basic version)"
            # Placeholder: Actual bootstrapping would go here if implemented
            # For now, just acknowledge the option.
          }

          self$kripp_summary_df <- data.frame(
            Statistic = paste0("Krippendorff's Alpha (", self$options$krippMethod, ")"),
            Value = round(alpha_val, 3),
            Note = ci_method_note
          )
        }
      }

      # Heatmap
      if (self$options$heatmap && length(self$options$vars) == 2) {
         private$.prepareHeatmapData(selected_data) # This will set self$heatmap_plot_object
      } else if (self$options$heatmap && length(self$options$vars) != 2) {
         print("Heatmap is only available for 2 raters.")
         self$heatmap_plot_object <- ggplot2::ggplot() + ggplot2::ggtitle("Heatmap only for 2 raters") + ggplot2::theme_void()
      }

      return(TRUE)
    }
  ),

  private = list(
    .prepareHeatmapData = function(data_for_heatmap) {
        # Assuming data_for_heatmap has 2 columns (the selected raters)
        if (ncol(data_for_heatmap) != 2) {
            self$heatmap_plot_object <- ggplot2::ggplot() + ggplot2::ggtitle("Heatmap requires exactly 2 raters.") + ggplot2::theme_void()
            return()
        }

        # Create a frequency table (contingency table)
        freq_table <- as.data.frame(base::table(data_for_heatmap[[1]], data_for_heatmap[[2]]))
        names(freq_table) <- c("Rater1", "Rater2", "Freq")

        # Generate the heatmap plot
        private$.heatmapPlot(freq_table) # This will set self$heatmap_plot_object
    },

    .heatmapPlot = function(heatmap_data_df) {
        # Get Kappa value if details are requested (only for 2 raters, unweighted)
        kappa_label <- ""
        if (self$options$heatmapDetails && length(self$options$vars) == 2) {
            # Recalculate Kappa for the two variables used in heatmap
            # This assumes self$data contains the original dataset and self$options$vars has the two rater names
            # For simplicity, let's use the already calculated Kappa if available and appropriate
            # Or, re-calculate based on the two vars in heatmap_data_df's original form.
            # For now, let's assume irr_summary_df is populated and relevant.
            if (!is.null(self$irr_summary_df) && grepl("Cohen's Kappa", self$irr_summary_df$Statistic[1])) {
                 kappa_val_for_plot <- self$irr_summary_df$Value[1]
                 if(!is.na(kappa_val_for_plot)) kappa_label <- paste("Kappa =", round(kappa_val_for_plot, 2))
            }
        }

        plot_obj <- ggplot2::ggplot(heatmap_data_df, ggplot2::aes(x = Rater1, y = Rater2, fill = Freq)) +
            ggplot2::geom_tile(color = "white") + # Add white lines between tiles
            ggplot2::scale_fill_gradient(low = "lightblue", high = "darkblue") +
            ggplot2::geom_text(ggplot2::aes(label = Freq), color = "black", size = 4) + # Add frequency numbers
            ggplot2::labs(title = "Agreement Heatmap",
                          subtitle = kappa_label,
                          x = names(self$data)[self$options$vars[1]],
                          y = names(self$data)[self$options$vars[2]],
                          fill = "Frequency") +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                           plot.title = ggplot2::element_text(hjust = 0.5, face="bold"),
                           plot.subtitle = ggplot2::element_text(hjust = 0.5))

        self$heatmap_plot_object <- plot_obj
    }
  )
)
