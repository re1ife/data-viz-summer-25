#!/usr/bin/env Rscript

required_packages <- c(
  "quarto",
  "fs",    
  "glue",  
  "cli",   
  "here"   
)

# Function to install packages if they're not already installed
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(sprintf("Installing %s...", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

# Configuration
RENDERED_DIR <- file.path(here::here(), "docs")  # Main output directory in project root
LECTURES_DIR <- file.path(RENDERED_DIR, "slides")  # Lectures are the slides
EXAMPLES_DIR <- file.path(RENDERED_DIR, "examples")  # Examples output directory
POSTMORTEMS_DIR <- file.path(RENDERED_DIR, "postmortems")  # Postmortems output directory

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

# Function to ensure directory exists
ensure_dir <- function(dir) {
  if (!dir_exists(dir)) {
    dir_create(dir, recurse = TRUE)
  }
}

# Function to clean up temporary files
cleanup_temp_files <- function(file_path) {
  # Get the base name without extension
  base_name <- basename(file_path)
  base_name <- sub("\\.qmd$", "", base_name)
  
  # Remove _files directory if it exists
  files_dir <- file.path(dirname(file_path), paste0(base_name, "_files"))
  if (dir_exists(files_dir)) {
    dir_delete(files_dir)
  }
  
  # Remove any temporary HTML files
  temp_html <- file.path(dirname(file_path), paste0(base_name, ".html"))
  if (file_exists(temp_html)) {
    file_delete(temp_html)
  }
}

# Function to compile a single document
compile_document <- function(file_path, output_dir) {
  # Get the base name without extension
  base_name <- basename(file_path)
  base_name <- sub("\\.qmd$", "", base_name)
  
  # Ensure output directory exists
  ensure_dir(output_dir)
  
  # Store original working directory
  old_wd <- getwd()
  
  cli_alert_info("Compiling {file_path}")
  tryCatch({
    # Change to the directory containing the file
    setwd(dirname(file_path))
    
    # First clean up any existing temporary files
    cleanup_temp_files(file_path)
    
    # Render the file using quarto CLI
    system(sprintf("quarto render %s --to html", basename(file_path)))
    
    # Move the rendered file to the output directory
    rendered_file <- file.path(dirname(file_path), paste0(base_name, ".html"))
    if (file_exists(rendered_file)) {
      file_move(rendered_file, file.path(output_dir, paste0(base_name, ".html")))
    }
    
    # Clean up temporary files after rendering
    cleanup_temp_files(file_path)
    
    cli_alert_success("Successfully compiled {file_path}")
  }, error = function(e) {
    cli_alert_danger("Failed to compile {file_path}: {e$message}")
  }, finally = {
    # Restore the original working directory
    setwd(old_wd)
  })
}

# Function to compile all files in a directory
compile_directory <- function(dir_path, output_dir) {
  # Ensure output directory exists
  ensure_dir(output_dir)
  
  # Get all .qmd files in the directory
  qmd_files <- dir_ls(dir_path, glob = "*.qmd", recurse = TRUE)
  
  # Compile each file
  for (file in qmd_files) {
    compile_document(file, output_dir)
  }
}

# Main compilation process
cli_h1("Starting Compilation Process")

# Create all necessary directories
ensure_dir(RENDERED_DIR)
ensure_dir(LECTURES_DIR)
ensure_dir(EXAMPLES_DIR)
ensure_dir(POSTMORTEMS_DIR)

# Compile lectures (slides) directly to slides directory
cli_h2("Compiling Lectures")
compile_directory(here("lectures"), LECTURES_DIR)

# Compile examples
cli_h2("Compiling Examples")
compile_directory(here("examples"), EXAMPLES_DIR)

# Compile postmortems
cli_h2("Compiling Postmortems")
compile_directory(here("postmortems"), POSTMORTEMS_DIR)

cli_h1("Compilation Complete")
cli_alert_info("All documents have been compiled to their respective directories:")
cli_bullets(c(
  "Lectures (slides): {LECTURES_DIR}",
  "Examples: {EXAMPLES_DIR}",
  "Postmortems: {POSTMORTEMS_DIR}"
)) 