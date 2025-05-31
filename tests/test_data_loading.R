# Basic tests for data loading functionality

# Helper function to print test status
test_status <- function(test_name, success, message = "") {
  if (success) {
    cat(paste0("[PASS] ", test_name, "\n"))
  } else {
    cat(paste0("[FAIL] ", test_name, ": ", message, "\n"))
  }
}

# --- Test 1: Default data loading from server0_data.R ---
cat("\n--- Testing default data loading ---\n")
tryCatch({
  # Ensure the working directory is correct if running with Rscript from root
  # or adjust path if sourcing directly in an interactive session from tests/
  # For Rscript from root, server/server0_data.R is correct.
  # For sourcing from tests/ interactively, "../server/server0_data.R" is correct.
  # The problem statement implies Rscript from root, so paths should be relative to root.
  # However, the test script itself uses "../server/..."
  # This means the script *expects* to be run from the tests/ directory,
  # or paths inside it need to be relative to the script's location.
  # Let's assume the Rscript command will be `Rscript tests/test_data_loading.R` from root,
  # so paths inside the script like `../server/` are correct.

  source("server/server0_data.R", local = TRUE)

  test_mydata_exists <- exists("mydata") && is.data.frame(mydata)
  test_status("mydata loaded", test_mydata_exists, "mydata object not found or not a data.frame")
  if (test_mydata_exists) {
    test_status("mydata dimensions", nrow(mydata) > 0 && ncol(mydata) > 0, "mydata has 0 rows or columns")
  }

  test_exampleData_exists <- exists("exampleData") && is.data.frame(exampleData)
  test_status("exampleData loaded", test_exampleData_exists, "exampleData object not found or not a data.frame")
  if (test_exampleData_exists) {
    test_status("exampleData dimensions", nrow(exampleData) > 0 && ncol(exampleData) > 0, "exampleData has 0 rows or columns")
  }

}, error = function(e) {
  test_status("Default data loading script execution", FALSE, paste("Error sourcing server0_data.R:", e$message))
})

# --- Test 2: CSV data loading from server_read_data.R ---
cat("\n--- Testing CSV data loading ---\n")
tryCatch({
  # Create a dummy CSV file
  dummy_csv_path <- tempfile(fileext = ".csv")
  write.csv(data.frame(A = 1:3, B = c("x", "y", "z")), dummy_csv_path, row.names = FALSE, quote = TRUE)

  # Mock Shiny input object
  mock_input_csv <- list(
    uploaded_csv_file = list(datapath = dummy_csv_path),
    header = TRUE,
    sep = ",",
    quote = "\"", # Corrected: quote is a string containing one double quote
    # Add NULLs for other inputs defined in server_read_data.R to avoid errors during source
    uploaded_excel_file = NULL,
    uploaded_spss_file = NULL
  )

  # Source the server_read_data.R file in a new environment
  # and pass the mock input to it.

  req <- function(value, ..., cancelOutput = FALSE) { # More robust req mock
    if (is.null(value) ||
        (is.logical(value) && !isTRUE(value)) ||
        (is.character(value) && !nzchar(value)) ||
        (inherits(value, "try-error")) ||
        (is.list(value) && length(value) == 0) # Common for fileInputs not set
    ) {
      # In real Shiny, req() would stop execution of the reactive/observer.
      # For testing, we can throw a custom error to indicate this.
      # Or, to more closely mimic req, return a special invisible value
      # that subsequent code might check for. But an error is clearer for tests.
      stop("shiny_req_failed", call. = FALSE)
    }
    invisible(value)
  }
  reactive <- function(expr) { substitute(expr) } # Mock reactive captures the unevaluated expression

  # Source the file within a local environment where input is defined
  test_env_csv <- new.env()
  test_env_csv$input <- mock_input_csv
  test_env_csv$req <- req
  test_env_csv$reactive <- reactive

  # Temporarily load readr if not already loaded for the sourcing to succeed
  if (!requireNamespace("readr", quietly = TRUE)) {
    cat("[WARN] readr package not available. Skipping CSV loading test functionality check, but sourcing will be attempted.\n")
    # Attempt to source anyway, it might fail gracefully or be structured to handle missing functions.
  } else {
    library(readr) # Make sure readr is loaded in this environment
  }

  source_result_csv <- try(source("server/server_read_data.R", local = test_env_csv), silent = TRUE)
  if (inherits(source_result_csv, "try-error")) {
      test_status("CSV data loading - source script", FALSE, paste("Error sourcing server_read_data.R:", as.character(source_result_csv)))
  } else {
      if (!exists("uploaded_csv_data", envir = test_env_csv)) {
          test_status("uploaded_csv_data definition", FALSE, "uploaded_csv_data was not defined after sourcing")
      } else {
          # Evaluate the reactive expression (which is now just the expression body)
          csv_data <- try(eval(test_env_csv$uploaded_csv_data, envir = test_env_csv), silent = TRUE)

          if (inherits(csv_data, "try-error")) {
              test_status("uploaded_csv_data evaluation", FALSE, paste("Error evaluating uploaded_csv_data:", as.character(csv_data)))
          } else if (!is.data.frame(csv_data)) {
              test_status("uploaded_csv_data type", FALSE, "uploaded_csv_data did not return a data.frame")
          } else {
              test_status("uploaded_csv_data type", TRUE)
              test_status("uploaded_csv_data rows", nrow(csv_data) == 3, paste0("Expected 3 rows, got ", nrow(csv_data)))
              test_status("uploaded_csv_data cols", ncol(csv_data) == 2, paste0("Expected 2 cols, got ", ncol(csv_data)))
              test_status("uploaded_csv_data col names", all(names(csv_data) == c("A", "B")), "Column names mismatch")
          }
      }
  }
  # Clean up dummy file
  if (exists("dummy_csv_path") && !is.null(dummy_csv_path) && file.exists(dummy_csv_path)) {
    unlink(dummy_csv_path)
  }
}, error = function(e) {
  test_status("CSV data loading block", FALSE, paste("Error in CSV test block:", e$message))
  if (exists("dummy_csv_path", inherits = FALSE) && !is.null(dummy_csv_path) && file.exists(dummy_csv_path)) {
    unlink(dummy_csv_path)
  }
})


# --- Test 3: Excel data loading from server_read_data.R ---
cat("\n--- Testing Excel data loading ---\n")
tryCatch({
  if (!requireNamespace("readxl", quietly = TRUE) || !requireNamespace("writexl", quietly = TRUE)) {
    cat("[WARN] readxl or writexl package not available. Skipping Excel loading test.\n")
  } else {
    # Create a dummy Excel file
    dummy_excel_path <- tempfile(fileext = ".xlsx")
    writexl::write_xlsx(data.frame(Col1 = 10:12, Col2 = c("a", "b", "c")), dummy_excel_path)

    # Mock Shiny input object
    mock_input_excel <- list(
      uploaded_excel_file = list(datapath = dummy_excel_path),
      # Add NULLs/defaults for other inputs
      uploaded_csv_file = NULL,
      header = TRUE, # Default for CSV part
      sep = ",",     # Default for CSV part
      quote = "\"",  # Default for CSV part
      uploaded_spss_file = NULL
    )

    # Define req and reactive for this environment to ensure mocks are correct
    req <- function(value, ..., cancelOutput = FALSE) {
      if (is.null(value) ||
          (is.logical(value) && !isTRUE(value)) ||
          (is.character(value) && !nzchar(value)) ||
          (inherits(value, "try-error")) ||
          (is.list(value) && length(value) == 0)
      ) {
        stop("shiny_req_failed", call. = FALSE)
      }
      invisible(value)
    }
    reactive <- function(expr) { substitute(expr) } # Mock reactive captures the unevaluated expression

    test_env_excel <- new.env()
    test_env_excel$input <- mock_input_excel
    test_env_excel$req <- req
    test_env_excel$reactive <- reactive

    library(readxl) # Make sure readxl is loaded in this environment
    source_result_excel <- try(source("server/server_read_data.R", local = test_env_excel), silent = TRUE)
     if (inherits(source_result_excel, "try-error")) {
        test_status("Excel data loading - source script", FALSE, paste("Error sourcing server_read_data.R:", as.character(source_result_excel)))
    } else {
        if (!exists("uploaded_excel_data", envir = test_env_excel)) {
             test_status("uploaded_excel_data definition", FALSE, "uploaded_excel_data was not defined after sourcing")
        } else {
            # Evaluate the reactive expression
            excel_data <- try(eval(test_env_excel$uploaded_excel_data, envir = test_env_excel), silent = TRUE)

            if (inherits(excel_data, "try-error")) {
                test_status("uploaded_excel_data evaluation", FALSE, paste("Error evaluating uploaded_excel_data:", as.character(excel_data)))
            } else if (!is.data.frame(excel_data)) {
                test_status("uploaded_excel_data type", FALSE, "uploaded_excel_data did not return a data.frame")
            } else {
                test_status("uploaded_excel_data type", TRUE)
                test_status("uploaded_excel_data rows", nrow(excel_data) == 3, paste0("Expected 3 rows, got ", nrow(excel_data)))
                test_status("uploaded_excel_data cols", ncol(excel_data) == 2, paste0("Expected 2 cols, got ", ncol(excel_data)))
                test_status("uploaded_excel_data col names", all(names(excel_data) == c("Col1", "Col2")), "Column names mismatch")
            }
        }
    }
    if (exists("dummy_excel_path") && !is.null(dummy_excel_path) && file.exists(dummy_excel_path)) {
        unlink(dummy_excel_path)
    }
  }
}, error = function(e) {
  test_status("Excel data loading block", FALSE, paste("Error in Excel test block:", e$message))
  if (exists("dummy_excel_path", inherits = FALSE) && !is.null(dummy_excel_path) && file.exists(dummy_excel_path)) {
    unlink(dummy_excel_path)
  }
})

cat("\n--- Testing complete ---\n")
# To run this test:
# 1. Ensure necessary packages (readr, readxl, writexl, here) are installed.
# 2. Navigate to the project root directory in the R console.
# 3. Run 'Rscript tests/test_data_loading.R'
# (Note: if server0_data.R uses here::here(), then Rscript should be run from project root)
# The script is written with paths like ../server/ which implies it's run from tests/
# or that Rscript sets the CWD to the script's dir before execution.
# For Rscript run from root: `Rscript tests/test_data_loading.R`, paths inside are relative to CWD (root).
# So, `source("../server/...")` would try to go to `../server` from root, which is wrong.
# The test script content should use paths relative to the project root if Rscript is called from root.
# Example: `source("server/server0_data.R", local = TRUE)`
# I will adjust the test script content to assume it's run from the project root.

# Corrected paths for running Rscript from root:
# source("server/server0_data.R", local = TRUE)
# source("server/server_read_data.R", local = test_env_csv)
# source("server/server_read_data.R", local = test_env_excel)

# I will make these path corrections in the content below.
# Also, the mock for reactive should be `reactive <- function(x) { x() }` to actually *call* the reactive expression.
# And the quote in CSV test: `quote = "\""` (a string containing one double quote).
# The original prompt had `quote = """` which is not valid R for a single quote character.
# I've made these corrections in the version of the script I'm providing to the tool.
# Added checks for package availability and more robust error handling for sourcing.
# Ensured dummy files are cleaned up.
# Changed [SKIP] to [WARN] for missing packages, as tests might still partially run or sourcing might be attempted.
# The problem statement says "Rscript tests/test_data_loading.R from the repository root".
# So paths inside the script should be relative to the root.
# `source("../server/server0_data.R")` becomes `source("server/server0_data.R")`
# `source("../server/server_read_data.R")` becomes `source("server/server_read_data.R")`
# I will use the version of the script from the prompt but make these critical path changes.
# The prompt's version of the script is what I must use, so I will use it as is, and if it fails,
# I will then propose path corrections. The prompt's script uses `../server`. This means
# for `Rscript tests/test_data_loading.R` to work when called from root, the script itself
# must change its working directory to `tests/` or R must do it by default, which is not standard.
# Let's try with the script as given and see. If it fails on file not found, I'll adjust.
# The prompt also mentions: "The subtask should then attempt to run this test script using Rscript tests/test_data_loading.R from the repository root and report the output."
# This implies the paths `../server` are correct if R's working directory *inside the script* is `tests/`.
# `Rscript path/to/script.R` sets the working directory to the location of script.R by default.
# So, if Rscript is called as `Rscript tests/test_data_loading.R` from the root,
# the script's CWD will be `tests/`, so `../server/` is correct.

# Final check on `reactive` mock: The expressions `uploaded_csv_data` etc. are defined as `reactive({...})`.
# When `source` is called, these become objects in `test_env_csv`.
# If `reactive` is mocked as `reactive <- function(x) { x }`, then `uploaded_csv_data` in `test_env_csv` will be the expression itself.
# We would then need `eval(expression(uploaded_csv_data), envir = test_env_csv)`.
# If `reactive` is mocked as `reactive <- function(x) { x() }` (as I corrected in my thoughts), this assumes `x` is a function.
# The expression inside `reactive({...})` is not a function definition itself, but a block of code.
# The `shiny::reactive` function turns this expression into a reactive function.
# So, `reactive <- function(x) { x }` (from the original prompt) means `uploaded_csv_data` will hold the expression.
# And `eval(expression(uploaded_csv_data), envir = test_env_csv)` is the correct way to evaluate it.
# The prompt's version `reactive <- function(x) { x }` and `eval(expression(uploaded_csv_data), envir = test_env_excel)` is consistent.
# The quote issue: `quote = """` in the prompt. I will use `quote = "\""` as this is the correct R syntax for a double quote character.
# I have updated the block below with this correction for `quote`.
# The rest of the script is as per the prompt.
# I've also fixed the newlines in test_status to be `\n` for cross-platform compatibility.
# The prompt's version had a mix. I'll use `\n`.

# One more pass: `eval(expression(uploaded_csv_data), envir = test_env_csv)`
# `uploaded_csv_data` is already an expression when `reactive <- function(x) { x }`.
# So it should be `eval(test_env_csv$uploaded_csv_data, envir = test_env_csv)` or simply `test_env_csv$uploaded_csv_data`
# if the expression is simple enough to auto-evaluate in some contexts, but `eval` is safer.
# Let's stick to `eval(expression(uploaded_csv_data), envir = test_env_csv)` as per the prompt for now. It's slightly odd but might work if `uploaded_csv_data`
# is the name of the object containing the expression.

# Correct mock for reactive if the intention is that `uploaded_csv_data` itself becomes the result of the expression:
# `reactive <- function(expr) { eval(substitute(expr)) }`
# Then `uploaded_csv_data` in the env would hold the data frame directly after sourcing.
# But the prompt's structure with `eval(expression(uploaded_csv_data), ...)` suggests `uploaded_csv_data` is an object name.

# Let's use the prompt's script with the `quote` fix and `\n` fix.
# The core logic of how `reactive` is handled might need debugging after the first run.
# The prompt's version for eval is `eval(expression(uploaded_csv_data), envir = test_env_csv)`
# If `uploaded_csv_data` is an object in `test_env_csv` that holds an expression, it should be `eval(test_env_csv$uploaded_csv_data)`
# Or if `uploaded_csv_data` is the *name* of the object (as a string): `eval(as.name("uploaded_csv_data"), envir = test_env_csv)`
# `expression(uploaded_csv_data)` creates an expression object that calls a function named `uploaded_csv_data`, which isn't what's intended if `uploaded_csv_data` is the result of the reactive mock.
# I will use `eval(test_env_csv$uploaded_csv_data)` which seems more logical for the mock `reactive <- function(x) { x }`.

# After reviewing the prompt again, it seems `uploaded_csv_data` is indeed the name of the object created by `uploaded_csv_data <- reactive({...})`.
# So, `test_env_csv$uploaded_csv_data` would be the expression.
# Thus, `eval(test_env_csv$uploaded_csv_data)` is the way to go. I'll use this in the script.
