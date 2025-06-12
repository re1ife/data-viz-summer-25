library(shiny)
library(dplyr)
library(ggplot2)
library(ggridges)
library(kableExtra)
library(forcats)
library(tidyr)

source(here::here("examples", "theme.R"))

# Load your processed data
data <- readr::read_csv(here::here("data", "processed", "simulated_data.csv"))

# UI
ui <- fluidPage(
  titlePanel("Healthcare Access and Outcomes Dashboard"),

  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select a State:",
                  choices = unique(data$state),
                  selected = "AK"),
      selectInput("plot_type", "Select Analysis:",
                  choices = c("Demographics", "Health Conditions", "Access to Care"),
                  selected = "Demographics")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Summary Statistics",
          h3("State-Level Summary"),
          tableOutput("summary_table")
        ),
        tabPanel("Visualizations",
          conditionalPanel(
            condition = "input.plot_type == 'Demographics'",
            h3("Income Distribution"),
            plotOutput("income_plot", height = "400px"),
            h3("Postnatal Care by Income Level"),
            plotOutput("postnatal_income_plot", height = "400px"),
            h3("Education Level Distribution"),
            plotOutput("edu_plot", height = "400px")
          ),
          conditionalPanel(
            condition = "input.plot_type == 'Health Conditions'",
            h3("Prevalence of Health Conditions"),
            plotOutput("health_conditions_plot", height = "600px")
          ),
          conditionalPanel(
            condition = "input.plot_type == 'Access to Care'",
            h3("Distance to Provider Distribution"),
            plotOutput("distance_plot", height = "400px"),
            h3("Postnatal Care Access"),
            plotOutput("postnatal_plot", height = "400px")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {

  # Filter data based on selected state
  state_data <- reactive({
    data |> filter(state == input$state)
  })

  # Generate summary table
  output$summary_table <- renderTable({
    state_data() |>
      summarize(
        n = n(),
        avg_age = mean(age, na.rm = TRUE),
        pct_postnatal_care = mean(received_comprehensive_postnatal_care, na.rm = TRUE) * 100,
        avg_distance = mean(distance_to_provider, na.rm = TRUE),
        pct_hypertension = mean(hypertension, na.rm = TRUE) * 100
      ) |>
      mutate(
        avg_age = round(avg_age, 1),
        pct_postnatal_care = round(pct_postnatal_care, 1),
        avg_distance = round(avg_distance, 1),
        pct_hypertension = round(pct_hypertension, 1)
      ) |>
      rename(
        "Sample Size" = n,
        "Average Age" = avg_age,
        "% Receiving Postnatal Care" = pct_postnatal_care,
        "Average Distance to Provider (miles)" = avg_distance,
        "% with Hypertension" = pct_hypertension
      )
  })

  # Income Distribution Plot
  output$income_plot <- renderPlot({
    # Create ordered factor levels for income
    income_levels <- c("$0–$24,999", "$25,000–$49,999", "$50,000–$74,999", 
                      "$75,000–$99,999", "$100,000–$124,999", "$125,000–$149,999",
                      "$150,000–$174,999", "$175,000+")
    
    ggplot(state_data(), aes(x = factor(self_report_income, levels = income_levels))) +
      geom_bar(fill = colors$HopkinsBlue) +
      coord_flip() +
      labs(
        title = "Income Distribution",
        x = "Income Range",
        y = "Count"
      ) +
      theme_jhu_bar()
  })

  # Postnatal Care by Income Plot
  output$postnatal_income_plot <- renderPlot({
    # Create ordered factor levels for income
    income_levels <- c("$0–$24,999", "$25,000–$49,999", "$50,000–$74,999", 
                      "$75,000–$99,999", "$100,000–$124,999", "$125,000–$149,999",
                      "$150,000–$174,999", "$175,000+")
    
    state_data() |>
      group_by(self_report_income) |>
      summarize(
        pct_care = mean(received_comprehensive_postnatal_care, na.rm = TRUE) * 100,
        .groups = "drop"
      ) |>
      ggplot(aes(x = factor(self_report_income, levels = income_levels), 
                 y = pct_care)) +
      geom_col(fill = colors$HopkinsBlue) +
      coord_flip() +
      labs(
        title = "Postnatal Care Access by Income Level",
        x = "Income Range",
        y = "Percentage Receiving Care (%)"
      ) +
      theme_jhu_bar()
  })

  # Add Education Distribution Plot
  output$edu_plot <- renderPlot({
    # Create ordered factor levels for education
    edu_levels <- c("less_than_hs", "hs", "some_college", "college", "post_grad")
    
    ggplot(state_data(), aes(x = factor(edu, levels = edu_levels))) +
      geom_bar(fill = colors$HopkinsBlue) +
      coord_flip() +
      scale_x_discrete(labels = c(
        "less_than_hs" = "Less than High School",
        "hs" = "High School",
        "some_college" = "Some College",
        "college" = "College",
        "post_grad" = "Post Graduate"
      )) +
      labs(
        title = "Education Level Distribution",
        x = "Education Level",
        y = "Count"
      ) +
      theme_jhu_bar()
  })

  # Health Conditions Plot
  output$health_conditions_plot <- renderPlot({
    tryCatch({
      # Calculate prevalence for each condition
      health_data <- state_data() |>
        summarize(
          Obesity = mean(obesity, na.rm = TRUE) * 100,
          Diabetes = mean(diabetes, na.rm = TRUE) * 100,
          `Heart Disease` = mean(heart_disease, na.rm = TRUE) * 100,
          Hypertension = mean(hypertension, na.rm = TRUE) * 100,
          Preeclampsia = mean(preeclampsia, na.rm = TRUE) * 100
        ) |>
        pivot_longer(
          cols = everything(),
          names_to = "condition",
          values_to = "prevalence"
        )
      
      # Create the plot
      ggplot(health_data, aes(x = condition, y = prevalence)) +
        geom_col(fill = colors$HopkinsBlue) +
        coord_flip() +
        labs(
          title = "Prevalence of Health Conditions",
          x = NULL,
          y = "Prevalence (%)"
        ) +
        theme_jhu_bar()
    }, error = function(e) {
      # Return a simple error plot if something goes wrong
      ggplot() + 
        annotate("text", x = 0, y = 0, 
                label = paste("Error loading health conditions:", e$message)) +
        theme_void()
    })
  })

  # Distance to Provider Plot
  output$distance_plot <- renderPlot({
    ggplot(state_data(), aes(x = distance_to_provider)) +
      geom_histogram(fill = colors$HopkinsBlue, bins = 30) +
      labs(
        title = "Distribution of Distance to Healthcare Provider",
        x = "Distance (miles)",
        y = "Count"
      ) +
      theme_jhu()
  })

  # Postnatal Care Plot
  output$postnatal_plot <- renderPlot({
    state_data() |>
      group_by(insurance) |>
      summarize(
        pct_care = mean(received_comprehensive_postnatal_care, na.rm = TRUE) * 100
      ) |>
      ggplot(aes(x = fct_reorder(insurance, pct_care), y = pct_care)) +
      geom_col(fill = colors$HopkinsBlue) +
      coord_flip() +
      labs(
        title = "Postnatal Care Access by Insurance Type",
        x = NULL,
        y = "Percentage Receiving Care (%)"
      ) +
      theme_jhu_bar()
  })
}

# Run App
shinyApp(ui = ui, server = server)
