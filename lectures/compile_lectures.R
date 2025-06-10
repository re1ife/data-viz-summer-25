#!/usr/bin/env Rscript

# Configuration
OUTPUT_DIR <- "slides"  # Change this to your desired output directory

# List of required packages
required_packages <- c(
  "quarto",    # For rendering Quarto documents
  "fs",        # For file system operations
  "glue",      # For string interpolation
  "cli",       # For command-line interface
  "here"       # For path handling
)

# Function to install packages if they're not already installed
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(sprintf("Installing %s...", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

# Install all required packages
message("Checking and installing required packages...")
for (pkg in required_packages) {
  install_if_missing(pkg)
}

# Load required packages
library(quarto)
library(fs)
library(glue)
library(cli)
library(here)

# Function to compile a single lecture
compile_lecture <- function(lecture_file, output_dir) {
  # Get the day number from the path or filename depending on if it's a postmortem
  is_postmortem <- grepl("postmortem", lecture_file)
  if (is_postmortem) {
    day_num <- path_file(lecture_file)
    day_num <- sub("\\.qmd$", "", day_num)  # Remove .qmd extension
  } else {
    day_num <- path_file(path_dir(lecture_file))
  }
  
  # Create output directory if it doesn't exist
  dir_create(here(output_dir), recurse = TRUE)
  
  # Store original working directory
  old_wd <- getwd()
  
  cli_alert_info("Compiling {lecture_file}")
  tryCatch({
    # Change to the directory containing the lecture file
    setwd(path_dir(lecture_file))
    
    # Render the file using quarto CLI
    system(sprintf("quarto render %s", path_file(lecture_file)))
    
    # Move the rendered file to the output directory
    rendered_file <- paste0(day_num, ".html")
    if (file_exists(rendered_file)) {
      file_move(
        rendered_file,
        path(here(output_dir), paste0(day_num, ".html"))
      )
    }
    
    cli_alert_success("Successfully compiled {lecture_file}")
  }, error = function(e) {
    cli_alert_danger("Failed to compile {lecture_file}: {e$message}")
  }, finally = {
    # Restore the original working directory
    setwd(old_wd)
  })
}

# List of lecture files to compile
lecture_files <- c(
  here("lectures", "day_1", "day_1_lecture.qmd"),
  here("lectures", "day_2", "day_2_lecture.qmd"),
  here("lectures", "day_3", "day_3_lecture.qmd"),
  here("postmortems", "day_1_postmortem.qmd")
)

# Create main output directory if it doesn't exist
dir_create(here(OUTPUT_DIR), recurse = TRUE)

# Compile each lecture
cli_h1("Compiling Lectures")
for (file in lecture_files) {
  compile_lecture(file, OUTPUT_DIR)
}

cli_h1("Compilation Complete")
cli_alert_info("All lectures have been compiled to the {OUTPUT_DIR} directory")
