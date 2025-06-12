# In this file you can do things like load common libraries and source files you want to easily access

# Typically include only the libraries you use everywhere
required_packages <- c(
  "dplyr",
  "here",
  "ggplot2",
  "kableExtra",
  "readr",
  "tidyr"
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all packages
for (package in required_packages) {
  library(package, character.only = TRUE)
}

# Load the theme file
source(here::here("examples", "theme.R"))