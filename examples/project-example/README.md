# Project Example

This is an example project demonstrating a typical data analysis workflow using R and Quarto. The project shows how to:

1. Prepare and clean data
2. Create visualizations
3. Organize code in a reproducible way

## Project Structure

```
project-example/
├── _quarto.yml          # Common Quarto configuration
├── 1_data_prep.qmd      # Data preparation notebook
├── 2_visualization.qmd  # Visualization notebook
├── data/                # Directory for processed data
└── README.md           # This file
```

## Getting Started

1. Open the project in RStudio by double-clicking the `.Rproj` file
2. Run the notebooks in order:
   - First run `1_data_prep.qmd` to prepare the data
   - Then run `2_visualization.qmd` to create visualizations

## Dependencies

This project requires the following R packages:
- dplyr
- ggplot2
- tibble

You can install them using:

```r
install.packages(c("dplyr", "ggplot2", "tibble"))
```

## Data

The project uses the built-in `mtcars` dataset from R, which contains fuel consumption and other characteristics of 32 automobiles from 1973-74. 