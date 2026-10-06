required_packages <- c(
  "shiny", "shinydashboard", "dplyr", "tidyr", "ggplot2", "plotly",
  "DT", "scales", "psych", "cluster"
)

install.packages(setdiff(required_packages, rownames(installed.packages())))

