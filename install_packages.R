options(repos = c(CRAN = "https://cloud.r-project.org"))
packages_to_install <- c(
  "shinyBS",
  "correlation", "corx", "dlookr", "finalfit", "ggcorrplot",
  "gtsummary", "highcharter", "inspectdf", "irr", "janitor",
  "pacman", "papaja", "psycho", "reactable", "report",
  "rintrojs", "rpivotTable", "shinyFeedback", "shinyLP",
  "shinyPivot", "shinyWidgets", "smartEDA", "visdat"
)

install.packages(packages_to_install, Ncpus = 2, dependencies = TRUE) # Added dependencies=TRUE

print("Verifying installed packages...")
for (pkg in packages_to_install) {
  cat(paste0("Attempting to load: ", pkg, "\n"))
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("SUCCESS: Package '", pkg, "' is available.\n"))
  } else {
    cat(paste0("FAILURE: Package '", pkg, "' IS NOT available.\n"))
  }
}
