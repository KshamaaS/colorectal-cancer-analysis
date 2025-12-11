# Load required libraries
library(shiny)
library(plotly)
library(tidyverse)

# Load and prepare data
data <- read.csv("crc_dataset.csv")

data <- data %>% rename(
  Pre_existing_Conditions = Pre.existing.Conditions,
  Carbohydrates = Carbohydrates..g.,
  Proteins = Proteins..g.,
  Fats = Fats..g.,
  Vitamin_A = Vitamin.A..IU.,
  Vitamin_C = Vitamin.C..mg.,
  Iron = Iron..mg.
)

# Prepare data with numeric conversions
prepared_data <- data %>%
  mutate(
    Risk_Numeric = CRC_Risk,
    Lifestyle_Numeric = case_when(
      Lifestyle == "Active" ~ 1,
      Lifestyle == "Moderate Exercise" ~ 2,
      Lifestyle == "Sedentary" ~ 3,
      Lifestyle == "Smoker" ~ 4
    ),
    Gender_Numeric = ifelse(Gender == "Male", 1, 0),
    Family_History_Numeric = ifelse(Family_History_CRC == "Yes", 1, 0)
  )

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Parallel Coordinates - CRC Risk Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Select Variables:"),
      
      checkboxGroupInput(
        "selected_vars",
        NULL,
        choices = c(
          "Age" = "Age",
          "Gender" = "Gender",
          "BMI" = "BMI",
          "Lifestyle" = "Lifestyle",
          "Family History" = "Family_Hx",
          "Carbohydrates" = "Carbs",
          "Proteins" = "Protein",
          "Fats" = "Fats",
          "Vitamin A" = "Vit_A",
          "Vitamin C" = "Vit_C",
          "Iron" = "Iron"
        ),
        selected = c("Age", "BMI", "Lifestyle", "Vit_C", "Iron")
      ),
      
      hr(),
      
      sliderInput(
        "opacity",
        "Line Opacity:",
        min = 0.02,
        max = 0.5,
        value = 0.08,
        step = 0.02
      ),
      
      hr(),
      
      helpText(
        "Drag on axes to filter ranges.",
        br(),
        "Displaying all", nrow(prepared_data), "participants."
      )
    ),
    
    mainPanel(
      width = 9,
      plotlyOutput("parallel_plot", height = "700px")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  output$parallel_plot <- renderPlotly({
    req(length(input$selected_vars) > 0)
    
    pdata <- prepared_data
    dims <- list()
    
    # Add dimensions based on selections
    if("Age" %in% input$selected_vars) {
      dims <- append(dims, list(list(
        label = "Age",
        values = pdata$Age,
        range = c(25, 80)
      )))
    }
    
    if("Gender" %in% input$selected_vars) {
      dims <- append(dims, list(list(
        label = "Gender",
        values = pdata$Gender_Numeric,
        range = c(0, 1),
        tickvals = c(0, 1),
        ticktext = c("F", "M")
      )))
    }
    
    if("BMI" %in% input$selected_vars) {
      dims <- append(dims, list(list(
        label = "BMI",
        values = pdata$BMI,
        range = c(18, 35)
      )))
    }
    
    if("Lifestyle" %in% input$selected_vars) {
      dims <- append(dims, list(list(
        label = "Lifestyle",
        values = pdata$Lifestyle_Numeric,
        range = c(1, 4),
        tickvals = c(1, 2, 3, 4),
        ticktext = c("Active", "Mod", "Sed", "Smoke")
      )))
    }
    
    if("Family_Hx" %in% input$selected_vars) {
      dims <- append(dims, list(list(
        label = "Family Hx",
        values = pdata$Family_History_Numeric,
        range = c(0, 1),
        tickvals = c(0, 1),
        ticktext = c("No", "Yes")
      )))
    }
    
    if("Carbs" %in% input$selected_vars) {
      dims <- append(dims, list(list(
        label = "Carbs (g)",
        values = pdata$Carbohydrates,
        range = c(150, 400)
      )))
    }
    
    if("Protein" %in% input$selected_vars) {
      dims <- append(dims, list(list(
        label = "Protein (g)",
        values = pdata$Proteins,
        range = c(50, 120)
      )))
    }
    
    if("Fats" %in% input$selected_vars) {
      dims <- append(dims, list(list(
        label = "Fats (g)",
        values = pdata$Fats,
        range = c(40, 120)
      )))
    }
    
    if("Vit_A" %in% input$selected_vars) {
      dims <- append(dims, list(list(
        label = "Vit A (IU)",
        values = pdata$Vitamin_A,
        range = c(3000, 8000)
      )))
    }
    
    if("Vit_C" %in% input$selected_vars) {
      dims <- append(dims, list(list(
        label = "Vit C (mg)",
        values = pdata$Vitamin_C,
        range = c(30, 120)
      )))
    }
    
    if("Iron" %in% input$selected_vars) {
      dims <- append(dims, list(list(
        label = "Iron (mg)",
        values = pdata$Iron,
        range = c(5, 20)
      )))
    }
    
    # Create plot
    plot_ly(
      type = "parcoords",
      line = list(
        color = pdata$Risk_Numeric,
        colorscale = list(c(0, "#27AE60"), c(1, "#E74C3C")),
        showscale = TRUE,
        opacity = input$opacity,
        colorbar = list(
          title = "CRC Risk",
          tickvals = c(0, 1),
          ticktext = c("No Risk", "At Risk"),
          len = 0.6,
          thickness = 15
        )
      ),
      dimensions = dims
    ) %>%
      layout(
        title = list(
          text = "Multi-Variable Risk Analysis<br><br><br>",
          font = list(size = 16)
        ),
        margin = list(t = 100, b = 60, l = 60, r = 120)
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)