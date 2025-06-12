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

# Function to get the appropriate project directory for a file
get_project_dir <- function(file_path) {
  if (grepl("lectures", file_path)) {
    return(here("lectures"))
  } else if (grepl("examples", file_path)) {
    return(here("examples"))
  } else if (grepl("postmortems", file_path)) {
    return(here("postmortems"))
  }
  return(dirname(file_path))
}

# Function to get the output file name based on the input file
get_output_filename <- function(file_path) {
  base_name <- basename(file_path)
  base_name <- sub("\\.qmd$", "", base_name)  # Remove .qmd extension
  
  # For lectures, use the day number
  if (grepl("lectures", file_path)) {
    day_num <- path_file(path_dir(file_path))
    return(paste0(day_num, ".html"))
  }
  
  # For examples and postmortems, just use the base name
  return(paste0(base_name, ".html"))
}

# Function to compile a single document
compile_document <- function(file_path, output_dir) {
  cli_alert_info("Processing file: {file_path}")
  cli_alert_info("Output directory: {output_dir}")
  
  # Ensure output directory exists
  ensure_dir(output_dir)
  
  # Store original working directory
  old_wd <- getwd()
  
  cli_alert_info("Compiling {file_path}")
  tryCatch({
    # Change to the appropriate project directory to use its _quarto.yml
    project_dir <- get_project_dir(file_path)
    cli_alert_info("Project directory: {project_dir}")
    setwd(project_dir)
    
    # First clean up any existing temporary files
    cleanup_temp_files(file_path)
    
    # Get the base name for the output file
    base_name <- basename(file_path)
    base_name <- sub("\\.qmd$", "", base_name)
    
    # For lectures, use the day number
    if (grepl("lectures", file_path)) {
      output_name <- paste0(path_file(path_dir(file_path)), ".html")
      # Look for the rendered file in the source directory
      nested_file <- file.path(dirname(file_path), paste0(path_file(path_dir(file_path)), "_lecture.html"))
    } else {
      # For examples and postmortems, use the base name
      output_name <- paste0(base_name, ".html")
      # Look for the rendered file in the same directory as the source
      nested_file <- file.path(dirname(file_path), paste0(base_name, ".html"))
    }
    
    target_file <- file.path(output_dir, output_name)
    
    cli_alert_info("Looking for rendered file at: {nested_file}")
    
    # Render the file using quarto CLI with the appropriate _quarto.yml
    render_cmd <- sprintf("quarto render %s", file_path)
    cli_alert_info("Running command: {render_cmd}")
    system(render_cmd)
    
    if (file_exists(nested_file)) {
      cli_alert_success("Found rendered file at: {nested_file}")
      
      # Move the file to the flat structure
      file_move(nested_file, target_file)
      cli_alert_success("Moved file to: {target_file}")
      
      # Also handle the _files directory
      if (grepl("lectures", file_path)) {
        nested_files_dir <- file.path(output_dir, path_file(path_dir(file_path)), paste0(path_file(path_dir(file_path)), "_lecture_files"))
      } else {
        nested_files_dir <- file.path(dirname(file_path), paste0(base_name, "_files"))
      }
      
      target_files_dir <- file.path(output_dir, paste0(sub("\\.html$", "", output_name), "_files"))
      
      if (dir_exists(nested_files_dir)) {
        cli_alert_success("Found _files directory at: {nested_files_dir}")
        # Move the _files directory to the flat structure
        file_move(nested_files_dir, target_files_dir)
        cli_alert_success("Moved _files directory to: {target_files_dir}")
      }
      
      # Clean up the empty day directory for lectures
      if (grepl("lectures", file_path)) {
        day_dir <- file.path(output_dir, path_file(path_dir(file_path)))
        if (dir_exists(day_dir)) {
          dir_delete(day_dir)
          cli_alert_success("Cleaned up empty directory: {day_dir}")
        }
      }
    } else {
      cli_alert_danger("Rendered file not found at: {nested_file}")
    }
    
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
  cli_alert_info("Compiling directory: {dir_path}")
  cli_alert_info("Output directory: {output_dir}")
  
  # Ensure output directory exists
  ensure_dir(output_dir)
  
  # Get all .qmd files in the directory, excluding project-example
  qmd_files <- dir_ls(dir_path, glob = "*.qmd", recurse = TRUE)
  qmd_files <- qmd_files[!grepl("project-example", qmd_files)]
  
  # Exclude the examples/project-example directory
  qmd_files <- qmd_files[!grepl("examples/project-example", qmd_files)]
  
  cli_alert_info("Found {length(qmd_files)} files to compile")
  
  # Compile each file
  for (file in qmd_files) {
    compile_document(file, output_dir)
  }
}

# Function to clean up output directories while preserving index files
cleanup_output_dirs <- function() {
  cli_alert_info("Cleaning up output directories...")
  
  # List of directories to clean
  dirs_to_clean <- c(LECTURES_DIR, EXAMPLES_DIR, POSTMORTEMS_DIR)
  
  for (dir in dirs_to_clean) {
    if (dir_exists(dir)) {
      # List all files and directories
      items <- dir_ls(dir, all = TRUE)
      
      # Remove each item
      for (item in items) {
        if (dir_exists(item)) {
          dir_delete(item)
        } else {
          file_delete(item)
        }
      }
      cli_alert_success("Cleaned directory: {dir}")
    }
  }
}

# Main compilation process
cli_h1("Starting Compilation Process")

# Create all necessary directories
ensure_dir(RENDERED_DIR)
ensure_dir(LECTURES_DIR)
ensure_dir(EXAMPLES_DIR)
ensure_dir(POSTMORTEMS_DIR)

# Clean up existing files in output directories
cleanup_output_dirs()

# Compile lectures (slides) directly to slides directory
cli_h2("Compiling Lectures")
compile_directory(here("lectures"), LECTURES_DIR)

# Compile examples
cli_h2("Compiling Examples")
compile_directory(here("examples"), EXAMPLES_DIR)

# Compile postmortems
cli_h2("Compiling Postmortems")
compile_directory(here("postmortems"), POSTMORTEMS_DIR)

# Render the index file
cli_h2("Rendering Index")
index_file <- here("docs", "index.qmd")
if (file_exists(index_file)) {
  cli_alert_info("Rendering index file: {index_file}")
  setwd(here("docs"))  # Change to docs directory to use its _quarto.yml
  system("quarto render index.qmd")
  setwd(here())  # Return to project root
  cli_alert_success("Index file rendered successfully")
} else {
  cli_alert_danger("Index file not found at: {index_file}")
}

cli_h1("Compilation Complete")
cli_alert_info("All documents have been compiled to their respective directories:")
cli_bullets(c(
  "Lectures (slides): {LECTURES_DIR}",
  "Examples: {EXAMPLES_DIR}",
  "Postmortems: {POSTMORTEMS_DIR}"
)) 