library(rsconnect)# app.R
library(shiny)
library(readr)
library(dplyr)
library(plotly)

# ---------- Healthy Ranges Reference ----------
# References: USDA Dietary Guidelines and NIH Recommended Dietary Allowances (RDA)
#
# Nutrient              Healthy Range       Notes
# -------------------------------------------------------
# Carbohydrates         225–325 g/day       Based on 2000 kcal diet
# Proteins              50–100 g/day        Varies by activity
# Fats                  44–77 g/day         20–35% of calories
# Vitamin A             2,000–3,000 IU/day  UL: 10,000 IU
# Vitamin C             75–200 mg/day       Higher for immunity
# Iron                  8–18 mg/day         Women need more
# BMI                   18.5–24.9           Healthy adult range
# Age                   18–90               UI bound: Not a nutrient
# -------------------------------------------------------

# ---------- 1. Load data and prepare ----------
crc <- read_csv("crc_dataset.csv", show_col_types = FALSE)

crc <- crc |>
  mutate(
    CRC_Risk               = as.factor(CRC_Risk),
    Gender                 = as.factor(Gender),
    Lifestyle              = as.factor(Lifestyle),
    Ethnicity              = as.factor(Ethnicity),
    Family_History_CRC     = as.factor(Family_History_CRC),
    `Pre-existing Conditions` = as.factor(`Pre-existing Conditions`)
  )

# Fit a logistic regression
crc_model <- glm(
  CRC_Risk ~ Age + BMI +
    `Carbohydrates (g)` + `Proteins (g)` + `Fats (g)` +
    `Vitamin A (IU)` + `Vitamin C (mg)` + `Iron (mg)` +
    Gender + Lifestyle + Ethnicity + Family_History_CRC +
    `Pre-existing Conditions`,
  data   = crc,
  family = binomial
)

# ---------- 2. UI ----------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-size: 14px; }
      .form-group { margin-bottom: 8px; }
      .control-label { font-size: 12px; font-weight: 600; margin-bottom: 3px; }
      .form-control { height: 32px; font-size: 13px; }
      select.form-control { height: 32px; padding-top: 4px; padding-bottom: 4px; }
      .sidebar-panel-custom {
        padding: 15px;
        max-height: 95vh;
        overflow-y: auto;
      }
      .main-panel-custom {
        padding: 15px;
      }
      h4 { margin-top: 5px; margin-bottom: 15px; font-size: 18px; }
      .btn-predict { margin-top: 10px; }
    "))
  ),

  titlePanel("CRC Risk Prediction", windowTitle = "CRC Risk Prediction"),

  fluidRow(
    # Left Panel - Input Form
    column(4,
           class = "sidebar-panel-custom",

           h4("Enter Participant Details"),

           # Row 1: Age and BMI
           fluidRow(
             column(6,
                    div(class = "form-group",
                        tags$label(class = "control-label", "Age"),
                        tags$input(type = "number", class = "form-control", id = "Age",
                                   min = "1", max = "120")
                    )
             ),
             column(6,
                    div(class = "form-group",
                        tags$label(class = "control-label", "BMI"),
                        tags$input(type = "number", class = "form-control", id = "BMI",
                                   min = "10", max = "60", step = "0.1")
                    )
             )
           ),

           # Row 2: Carbs and Proteins
           fluidRow(
             column(6,
                    div(class = "form-group",
                        tags$label(class = "control-label", "Carbohydrates (g)"),
                        tags$input(type = "number", class = "form-control", id = "Carb",
                                   min = "0")
                    )
             ),
             column(6,
                    div(class = "form-group",
                        tags$label(class = "control-label", "Proteins (g)"),
                        tags$input(type = "number", class = "form-control", id = "Prot",
                                   min = "0")
                    )
             )
           ),

           # Row 3: Fats and Vitamin A
           fluidRow(
             column(6,
                    div(class = "form-group",
                        tags$label(class = "control-label", "Fats (g)"),
                        tags$input(type = "number", class = "form-control", id = "Fat",
                                   min = "0")
                    )
             ),
             column(6,
                    div(class = "form-group",
                        tags$label(class = "control-label", "Vitamin A (IU)"),
                        tags$input(type = "number", class = "form-control", id = "VitA",
                                   min = "0")
                    )
             )
           ),

           # Row 4: Vitamin C and Iron
           fluidRow(
             column(6,
                    div(class = "form-group",
                        tags$label(class = "control-label", "Vitamin C (mg)"),
                        tags$input(type = "number", class = "form-control", id = "VitC",
                                   min = "0")
                    )
             ),
             column(6,
                    div(class = "form-group",
                        tags$label(class = "control-label", "Iron (mg)"),
                        tags$input(type = "number", class = "form-control", id = "Iron",
                                   min = "0", step = "0.1")
                    )
             )
           ),

           # Row 5: Gender and Lifestyle
           fluidRow(
             column(6,
                    selectInput("Gender", "Gender",
                                choices = c("Select..." = "", levels(crc$Gender)))
             ),
             column(6,
                    selectInput("Lifestyle", "Lifestyle",
                                choices = c("Select..." = "", levels(crc$Lifestyle)))
             )
           ),

           # Row 6: Ethnicity and Family History
           fluidRow(
             column(6,
                    selectInput("Ethnicity", "Ethnicity",
                                choices = c("Select..." = "", levels(crc$Ethnicity)))
             ),
             column(6,
                    selectInput("FH", "Family History",
                                choices = c("Select..." = "", levels(crc$Family_History_CRC)))
             )
           ),

           # Row 7: Pre-existing Conditions
           selectInput("PreCond", "Pre-existing Conditions",
                       choices = c("Select..." = "", levels(crc$`Pre-existing Conditions`))),

           actionButton("predict_btn", "Predict Risk",
                        class = "btn btn-primary btn-block btn-predict")
    ),

    # Right Panel - Results and Visualization
    column(8,
           class = "main-panel-custom",

           div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
               h4(style = "margin-top: 0; margin-bottom: 10px;", "Prediction Result"),
               verbatimTextOutput("prediction_text")
           ),

           h4(style = "margin-top: 10px; margin-bottom: 10px;", "Risk Visualizations"),

           # 3x2 Grid of visualizations with padding
           fluidRow(style = "margin-bottom: 15px;",
                    column(4, plotlyOutput("risk_gauge", height = "240px")),
                    column(4, plotlyOutput("distance_healthy", height = "240px")),
                    column(4,
                           div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; height: 240px;",
                               h5(style = "margin-top: 0; margin-bottom: 8px; font-size: 13px; font-weight: bold;",
                                  "Risk Prediction Simulator"),
                               selectInput("whatif_feature", "Select Feature:",
                                           choices = c("BMI", "Carbohydrates (g)", "Proteins (g)", "Fats (g)",
                                                       "Vitamin A (IU)", "Vitamin C (mg)", "Iron (mg)"),
                                           selected = "BMI"),
                               sliderInput("whatif_value", "Adjusted Value:",
                                           min = 0, max = 100, value = 50, step = 1),
                               verbatimTextOutput("whatif_result", placeholder = TRUE),
                               tags$style(HTML("#whatif_result { font-size: 11px; padding: 8px; max-height: 60px; overflow-y: auto; }"))
                           )
                    )
           ),
           fluidRow(
             column(4, plotlyOutput("risk_contribution", height = "240px")),
             column(4, plotlyOutput("risk_trajectory", height = "240px")),
             column(4, plotlyOutput("low_risk_comparison", height = "240px"))
           )
    )
  )
)

# ---------- 3. Server ----------
server <- function(input, output, session) {

  # Reactive values to store prediction results
  prediction_results <- reactiveValues(
    prob = NULL,
    class_pred = NULL,
    new_row = NULL,
    history = list()  # Store prediction history for trajectory
  )

  # Update slider range based on selected feature
  observeEvent(input$whatif_feature, {
    ranges <- list(
      "BMI" = c(15, 45, 25),
      "Carbohydrates (g)" = c(100, 500, 275),
      "Proteins (g)" = c(30, 200, 80),
      "Fats (g)" = c(20, 150, 60),
      "Vitamin A (IU)" = c(1000, 15000, 5000),
      "Vitamin C (mg)" = c(20, 200, 90),
      "Iron (mg)" = c(3, 30, 12)
    )

    range_vals <- ranges[[input$whatif_feature]]
    updateSliderInput(session, "whatif_value",
                      min = range_vals[1],
                      max = range_vals[2],
                      value = range_vals[3])
  })

  # What-if calculation
  output$whatif_result <- renderText({
    req(prediction_results$new_row, input$whatif_value)

    # Create modified row
    modified_row <- prediction_results$new_row

    # Update the selected feature
    modified_row[[input$whatif_feature]] <- as.numeric(input$whatif_value)

    # Get new prediction
    new_prob <- predict(crc_model, newdata = modified_row, type = "response")
    current_prob <- prediction_results$prob

    diff <- new_prob - current_prob
    diff_pct <- (diff / current_prob) * 100

    impact_text <- if(abs(diff) < 0.01) {
      "Minimal impact"
    } else if(diff < 0) {
      "Lower risk (Favorable)"
    } else {
      "Higher risk (Unfavorable)"
    }

    paste0(
      "Current Risk: ", round(current_prob * 100, 1), "%\n",
      "Predicted Risk: ", round(new_prob * 100, 1), "%\n",
      "Change: ", ifelse(diff > 0, "+", ""), round(diff * 100, 1), "% ",
      "(", ifelse(diff > 0, "+", ""), round(diff_pct, 1), "%)\n",
      "Impact: ", impact_text
    )
  })

  observeEvent(input$predict_btn, {
    # Validate inputs - get values from HTML inputs using session$input
    age_val <- input$Age
    bmi_val <- input$BMI
    carb_val <- input$Carb
    prot_val <- input$Prot
    fat_val <- input$Fat
    vita_val <- input$VitA
    vitc_val <- input$VitC
    iron_val <- input$Iron

    # Validation
    if (is.null(age_val) || is.na(age_val) || age_val == "" ||
        is.null(bmi_val) || is.na(bmi_val) || bmi_val == "" ||
        is.null(carb_val) || is.na(carb_val) || carb_val == "" ||
        is.null(prot_val) || is.na(prot_val) || prot_val == "" ||
        is.null(fat_val) || is.na(fat_val) || fat_val == "" ||
        is.null(vita_val) || is.na(vita_val) || vita_val == "" ||
        is.null(vitc_val) || is.na(vitc_val) || vitc_val == "" ||
        is.null(iron_val) || is.na(iron_val) || iron_val == "" ||
        input$Gender == "" || input$Lifestyle == "" ||
        input$Ethnicity == "" || input$FH == "" || input$PreCond == "") {

      showModal(modalDialog(
        title = "Missing Information",
        "Please fill in all fields before making a prediction.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    # Build a one-row data frame from the inputs
    new_row <- data.frame(
      Age                 = as.numeric(age_val),
      BMI                 = as.numeric(bmi_val),
      `Carbohydrates (g)` = as.numeric(carb_val),
      `Proteins (g)`      = as.numeric(prot_val),
      `Fats (g)`          = as.numeric(fat_val),
      `Vitamin A (IU)`    = as.numeric(vita_val),
      `Vitamin C (mg)`    = as.numeric(vitc_val),
      `Iron (mg)`         = as.numeric(iron_val),
      Gender              = factor(input$Gender, levels = levels(crc$Gender)),
      Lifestyle           = factor(input$Lifestyle, levels = levels(crc$Lifestyle)),
      Ethnicity           = factor(input$Ethnicity, levels = levels(crc$Ethnicity)),
      Family_History_CRC  = factor(input$FH, levels = levels(crc$Family_History_CRC)),
      `Pre-existing Conditions` = factor(input$PreCond,
                                         levels = levels(crc$`Pre-existing Conditions`)),
      check.names = FALSE
    )

    # Predict probability
    prob <- predict(crc_model, newdata = new_row, type = "response")
    class_pred <- ifelse(prob >= 0.5, "High risk", "Low risk")

    # Store in reactive values
    prediction_results$prob <- prob
    prediction_results$class_pred <- class_pred
    prediction_results$new_row <- new_row

    # Add to history (for trajectory)
    prediction_results$history[[length(prediction_results$history) + 1]] <- list(
      timestamp = Sys.time(),
      risk = prob,
      bmi = as.numeric(bmi_val),
      age = as.numeric(age_val)
    )

    output$prediction_text <- renderText({
      paste0(
        "Predicted CRC risk probability: ", round(prob, 4), " (", round(prob * 100, 2), "%)\n",
        "Predicted class (cutoff 0.5): ", class_pred
      )
    })
  })

  # Risk Gauge Visualization
  output$risk_gauge <- renderPlotly({
    req(prediction_results$prob)

    prob <- prediction_results$prob

    # Create gauge chart
    fig <- plot_ly(
      type = "indicator",
      mode = "gauge+number+delta",
      value = prob * 100,
      title = list(text = "Risk Score (%)", font = list(size = 16)),
      delta = list(reference = 50),
      gauge = list(
        axis = list(range = list(0, 100), tickwidth = 1, tickfont = list(size = 10)),
        bar = list(color = ifelse(prob < 0.3, "green",
                                  ifelse(prob < 0.7, "orange", "red"))),
        steps = list(
          list(range = c(0, 30), color = "lightgreen"),
          list(range = c(30, 70), color = "lightyellow"),
          list(range = c(70, 100), color = "lightcoral")
        ),
        threshold = list(
          line = list(color = "red", width = 3),
          thickness = 0.75,
          value = 50
        )
      )
    )

    fig <- fig |> layout(margin = list(l=10, r=10, t=40, b=10))

    fig
  })

  # Distance from Healthy Range
  output$distance_healthy <- renderPlotly({
    req(prediction_results$new_row)

    # Define healthy ranges (Updated based on USDA/NIH guidelines)
    health_data <- data.frame(
      Feature = c("Age", "BMI", "Carbs", "Protein", "Fat", "Vit A", "Vit C", "Iron"),
      User_Value = c(
        as.numeric(input$Age),
        as.numeric(input$BMI),
        as.numeric(input$Carb),
        as.numeric(input$Prot),
        as.numeric(input$Fat),
        as.numeric(input$VitA),
        as.numeric(input$VitC),
        as.numeric(input$Iron)
      ),
      Min_Healthy = c(18, 18.5, 225, 50, 44, 2000, 75, 8),
      Max_Healthy = c(90, 24.9, 325, 100, 77, 3000, 200, 18)
    )

    # Calculate distance from healthy range
    health_data <- health_data |>
      mutate(
        Distance_Pct = case_when(
          User_Value < Min_Healthy ~ ((User_Value - Min_Healthy) / Min_Healthy) * 100,
          User_Value > Max_Healthy ~ ((User_Value - Max_Healthy) / Max_Healthy) * 100,
          TRUE ~ 0
        ),
        Status_Color = case_when(
          Distance_Pct == 0 ~ "lightgreen",
          abs(Distance_Pct) < 20 ~ "lightyellow",
          TRUE ~ "coral"
        )
      )

    # Create bar chart
    fig <- plot_ly(health_data,
                   x = ~Feature,
                   y = ~Distance_Pct,
                   type = 'bar',
                   marker = list(color = ~Status_Color),
                   text = ~paste0(ifelse(Distance_Pct > 0, "+", ""), round(Distance_Pct, 1), "%"),
                   textposition = 'outside',
                   textfont = list(size = 8),
                   showlegend = FALSE) |>
      layout(
        title = list(text = "Distance from Healthy", font = list(size = 13)),
        xaxis = list(title = "", tickangle = -45, tickfont = list(size = 8)),
        yaxis = list(
          title = "% Off",
          titlefont = list(size = 10),
          tickfont = list(size = 8),
          zeroline = TRUE,
          zerolinecolor = 'green',
          zerolinewidth = 2
        ),
        margin = list(l=40, r=20, t=40, b=60)
      )

    fig
  })

  # Distance from Healthy Range - Shows how far you need to go to reach healthy targets
  output$lifestyle_comparison <- renderPlotly({
    req(prediction_results$new_row)

    # Define healthy ranges
    health_data <- data.frame(
      Feature = c("Age", "BMI", "Carbs", "Protein", "Fat", "Vit A", "Vit C", "Iron"),
      User_Value = c(
        as.numeric(input$Age),
        as.numeric(input$BMI),
        as.numeric(input$Carb),
        as.numeric(input$Prot),
        as.numeric(input$Fat),
        as.numeric(input$VitA),
        as.numeric(input$VitC),
        as.numeric(input$Iron)
      ),
      Min_Healthy = c(18, 18.5, 225, 50, 44, 3000, 75, 8),
      Max_Healthy = c(100, 24.9, 325, 175, 78, 10000, 120, 18)
    )

    # Calculate distance from healthy range
    health_data <- health_data |>
      mutate(
        # Distance: 0 if in range, negative if below, positive if above
        Distance = case_when(
          User_Value < Min_Healthy ~ User_Value - Min_Healthy,  # Negative (below)
          User_Value > Max_Healthy ~ User_Value - Max_Healthy,  # Positive (above)
          TRUE ~ 0  # In range
        ),
        # Calculate as percentage of range width for normalization
        Range_Width = Max_Healthy - Min_Healthy,
        Distance_Pct = case_when(
          User_Value < Min_Healthy ~ ((User_Value - Min_Healthy) / Min_Healthy) * 100,
          User_Value > Max_Healthy ~ ((User_Value - Max_Healthy) / Max_Healthy) * 100,
          TRUE ~ 0
        ),
        Status = case_when(
          Distance == 0 ~ "In Healthy Range",
          Distance < 0 ~ "Below Range",
          Distance > 0 ~ "Above Range"
        ),
        Status_Color = case_when(
          Distance == 0 ~ "lightgreen",
          abs(Distance_Pct) < 20 ~ "lightyellow",
          TRUE ~ "coral"
        )
      ) |>
      arrange(Distance_Pct)

    health_data$Feature <- factor(health_data$Feature, levels = health_data$Feature)

    # Create horizontal bar chart
    fig <- plot_ly(health_data,
                   y = ~Feature,
                   x = ~Distance_Pct,
                   type = 'bar',
                   orientation = 'h',
                   marker = list(
                     color = ~Status_Color,
                     line = list(color = 'black', width = 0.5)
                   ),
                   text = ~paste0(
                     ifelse(Distance_Pct > 0, "+", ""),
                     round(Distance_Pct, 1), "%"
                   ),
                   textposition = 'outside',
                   textfont = list(size = 8),
                   hovertext = ~paste0(
                     Feature, "<br>",
                     "Your Value: ", round(User_Value, 1), "<br>",
                     "Healthy Range: ", Min_Healthy, "-", Max_Healthy, "<br>",
                     "Distance: ", ifelse(Distance_Pct > 0, "+", ""),
                     round(Distance_Pct, 1), "% ", Status
                   ),
                   hoverinfo = 'text',
                   showlegend = FALSE) |>
      layout(
        title = list(text = "Distance from Healthy Range", font = list(size = 14)),
        xaxis = list(
          title = "% Distance",
          titlefont = list(size = 11),
          tickfont = list(size = 9),
          zeroline = TRUE,
          zerolinecolor = 'green',
          zerolinewidth = 3
        ),
        yaxis = list(title = "", tickfont = list(size = 9)),
        margin = list(l=80, r=10, t=40, b=40),
        annotations = list(
          list(
            text = "Green line = Healthy | Negative = Too Low | Positive = Too High",
            x = 0.5, y = -0.15, xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 9)
          )
        )
      )

    fig
  })

  # Feature Importance (coefficient-based)
  output$feature_importance <- renderPlotly({
    req(prediction_results$prob)

    # Get model coefficients
    coefs <- coef(crc_model)

    # Extract numeric coefficients (excluding intercept and factor levels)
    numeric_coefs <- coefs[c("Age", "BMI", "Carbohydrates (g)",
                             "Proteins (g)", "Fats (g)",
                             "Vitamin A (IU)", "Vitamin C (mg)", "Iron (mg)")]

    # Create data frame
    coef_df <- data.frame(
      Feature = names(numeric_coefs),
      Coefficient = as.numeric(numeric_coefs),
      Impact = ifelse(as.numeric(numeric_coefs) > 0, "Increases Risk", "Decreases Risk")
    ) |>
      arrange(abs(Coefficient))

    # Shorten feature names for compact display
    coef_df$Feature <- gsub(" \\(g\\)", "", coef_df$Feature)
    coef_df$Feature <- gsub(" \\(IU\\)", "", coef_df$Feature)
    coef_df$Feature <- gsub(" \\(mg\\)", "", coef_df$Feature)
    coef_df$Feature <- gsub("Carbohydrates", "Carbs", coef_df$Feature)
    coef_df$Feature <- gsub("Proteins", "Protein", coef_df$Feature)
    coef_df$Feature <- gsub("Vitamin ", "Vit ", coef_df$Feature)

    coef_df$Feature <- factor(coef_df$Feature, levels = coef_df$Feature)

    # Create horizontal bar chart
    fig <- plot_ly(coef_df, y = ~Feature, x = ~Coefficient,
                   type = 'bar', orientation = 'h',
                   color = ~Impact,
                   colors = c("Increases Risk" = "coral", "Decreases Risk" = "lightgreen")) |>
      layout(
        title = list(text = "Feature Impact on Risk", font = list(size = 14)),
        xaxis = list(title = "Coefficient", titlefont = list(size = 11), tickfont = list(size = 9)),
        yaxis = list(title = "", tickfont = list(size = 9)),
        showlegend = TRUE,
        legend = list(font = list(size = 9)),
        margin = list(l=80, r=10, t=40, b=40)
      )

    fig
  })

  # Risk Contribution Breakdown (Pie Chart)
  output$risk_contribution <- renderPlotly({
    req(prediction_results$new_row, prediction_results$prob)

    # Get model coefficients
    coefs <- coef(crc_model)

    # Calculate contributions for ALL features
    contributions <- data.frame(
      Factor = character(),
      Contribution = numeric(),
      stringsAsFactors = FALSE
    )

    # Numeric contributions - calculate absolute contribution for each feature
    numeric_features <- c("Age", "BMI", "Carbohydrates (g)", "Proteins (g)",
                          "Fats (g)", "Vitamin A (IU)", "Vitamin C (mg)", "Iron (mg)")
    feature_names <- c("Age", "BMI", "Carbs", "Protein", "Fat", "Vit A", "Vit C", "Iron")

    user_vals <- c(as.numeric(input$Age), as.numeric(input$BMI), as.numeric(input$Carb),
                   as.numeric(input$Prot), as.numeric(input$Fat), as.numeric(input$VitA),
                   as.numeric(input$VitC), as.numeric(input$Iron))

    dataset_means <- c(mean(crc$Age, na.rm = TRUE), mean(crc$BMI, na.rm = TRUE),
                       mean(crc$`Carbohydrates (g)`, na.rm = TRUE),
                       mean(crc$`Proteins (g)`, na.rm = TRUE),
                       mean(crc$`Fats (g)`, na.rm = TRUE),
                       mean(crc$`Vitamin A (IU)`, na.rm = TRUE),
                       mean(crc$`Vitamin C (mg)`, na.rm = TRUE),
                       mean(crc$`Iron (mg)`, na.rm = TRUE))

    # Calculate contribution for each numeric feature
    for(i in 1:length(numeric_features)) {
      if(!is.na(user_vals[i]) && !is.na(dataset_means[i]) && numeric_features[i] %in% names(coefs)) {
        # Contribution = deviation from mean * coefficient
        deviation <- user_vals[i] - dataset_means[i]
        coef_val <- as.numeric(coefs[numeric_features[i]])
        contrib <- abs(deviation * coef_val)

        if(!is.na(contrib) && !is.nan(contrib)) {
          contributions <- rbind(contributions,
                                 data.frame(Factor = feature_names[i],
                                            Contribution = contrib,
                                            stringsAsFactors = FALSE))
        }
      }
    }

    # Add categorical contributions with actual impact
    # Gender
    if(!is.na(input$Gender) && input$Gender != "") {
      gender_name <- paste0("Gender", input$Gender)
      if(gender_name %in% names(coefs)) {
        contributions <- rbind(contributions,
                               data.frame(Factor = "Gender",
                                          Contribution = abs(as.numeric(coefs[gender_name])),
                                          stringsAsFactors = FALSE))
      }
    }

    # Lifestyle
    if(!is.na(input$Lifestyle) && input$Lifestyle != "") {
      lifestyle_name <- paste0("Lifestyle", input$Lifestyle)
      if(lifestyle_name %in% names(coefs)) {
        contributions <- rbind(contributions,
                               data.frame(Factor = "Lifestyle",
                                          Contribution = abs(as.numeric(coefs[lifestyle_name])),
                                          stringsAsFactors = FALSE))
      }
    }

    # Family History
    if(!is.na(input$FH) && input$FH != "") {
      fh_name <- paste0("Family_History_CRC", input$FH)
      if(fh_name %in% names(coefs)) {
        contributions <- rbind(contributions,
                               data.frame(Factor = "Family History",
                                          Contribution = abs(as.numeric(coefs[fh_name])),
                                          stringsAsFactors = FALSE))
      }
    }

    # Ethnicity
    if(!is.na(input$Ethnicity) && input$Ethnicity != "") {
      ethnicity_name <- paste0("Ethnicity", input$Ethnicity)
      if(ethnicity_name %in% names(coefs)) {
        contributions <- rbind(contributions,
                               data.frame(Factor = "Ethnicity",
                                          Contribution = abs(as.numeric(coefs[ethnicity_name])),
                                          stringsAsFactors = FALSE))
      }
    }

    # Pre-existing Conditions
    if(!is.na(input$PreCond) && input$PreCond != "") {
      precond_name <- paste0("Pre-existing Conditions", input$PreCond)
      if(precond_name %in% names(coefs)) {
        contributions <- rbind(contributions,
                               data.frame(Factor = "Condition",
                                          Contribution = abs(as.numeric(coefs[precond_name])),
                                          stringsAsFactors = FALSE))
      }
    }

    # Check if we have any contributions
    if(nrow(contributions) == 0 || sum(contributions$Contribution, na.rm = TRUE) == 0) {
      fig <- plot_ly() |>
        layout(
          title = list(text = "Risk Contribution %", font = list(size = 13)),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(
            list(
              text = "No significant<br>contributions found",
              x = 0.5, y = 0.5,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 11)
            )
          ),
          margin = list(l=10, r=10, t=35, b=10)
        )
      return(fig)
    }

    # Filter and normalize to show ALL contributors
    contributions <- contributions |>
      filter(!is.na(Contribution) & !is.nan(Contribution) & Contribution > 0) |>
      arrange(desc(Contribution))

    # Calculate total and percentages
    total <- sum(contributions$Contribution, na.rm = TRUE)
    contributions$Percentage <- (contributions$Contribution / total) * 100

    # Create donut chart
    fig <- plot_ly(contributions,
                   labels = ~Factor,
                   values = ~Percentage,
                   type = 'pie',
                   hole = 0.4,
                   textinfo = 'label+percent',
                   textposition = 'outside',
                   textfont = list(size = 8),
                   marker = list(colors = c('#FF6B6B', '#FFA07A', '#FFD93D', '#6BCB77',
                                            '#4D96FF', '#A28089', '#C77DFF', '#E9C46A'))) |>
      layout(
        title = list(text = "Risk Contribution %", font = list(size = 13)),
        showlegend = FALSE,
        margin = list(l=20, r=20, t=40, b=20)
      )

    fig
  })

  # Comparison to Low-Risk Profile
  output$low_risk_comparison <- renderPlotly({
    req(prediction_results$new_row)

    # Define typical low-risk profile (ACTUAL MEDIAN VALUES from dataset)
    # Based on analysis of 845 low-risk participants (84.5% of dataset)
    low_risk_profile <- data.frame(
      Feature = c("Age", "BMI", "Carbs", "Protein", "Fat", "Vit A", "Vit C", "Iron"),
      Your_Value = c(
        as.numeric(input$Age),
        as.numeric(input$BMI),
        as.numeric(input$Carb),
        as.numeric(input$Prot),
        as.numeric(input$Fat),
        as.numeric(input$VitA),
        as.numeric(input$VitC),
        as.numeric(input$Iron)
      ),
      Low_Risk_Target = c(51, 26.2, 277, 86, 70, 5916, 77, 12.9)
    )

    # Normalize for comparison
    low_risk_profile <- low_risk_profile |>
      mutate(
        Your_Normalized = scale(Your_Value)[,1],
        Target_Normalized = scale(Low_Risk_Target)[,1]
      )

    # Create grouped bar chart
    fig <- plot_ly(low_risk_profile, x = ~Feature) |>
      add_trace(y = ~Your_Normalized, name = 'You', type = 'bar',
                marker = list(color = 'coral')) |>
      add_trace(y = ~Target_Normalized, name = 'Low-Risk Target', type = 'bar',
                marker = list(color = 'lightgreen')) |>
      layout(
        title = list(text = "You vs Low-Risk Profile", font = list(size = 13)),
        xaxis = list(title = "", tickangle = -45, tickfont = list(size = 8)),
        yaxis = list(title = "Normalized", titlefont = list(size = 10), tickfont = list(size = 8)),
        barmode = 'group',
        showlegend = TRUE,
        legend = list(font = list(size = 8), orientation = 'h', y = -0.25),
        margin = list(l=40, r=10, t=35, b=50)
      )

    fig
  })

  # Risk Trajectory Over Time
  output$risk_trajectory <- renderPlotly({
    history <- prediction_results$history

    if(length(history) == 0) {
      # Show empty state with instructions
      fig <- plot_ly() |>
        layout(
          title = list(text = "Risk Trajectory", font = list(size = 13)),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(
            list(
              text = "Track Your Progress<br><br>Make multiple predictions<br>to see your risk trend.<br><br>Each prediction will add<br>a new point to this chart.",
              x = 0.5, y = 0.5,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 10, color = "#666")
            )
          ),
          margin = list(l=40, r=10, t=35, b=40)
        )
      return(fig)
    }

    # Convert history to data frame
    trajectory_data <- data.frame(
      Session = 1:length(history),
      Risk = sapply(history, function(x) x$risk * 100),
      Timestamp = sapply(history, function(x) format(x$timestamp, "%H:%M:%S"))
    )

    # Calculate trend
    if(length(history) > 1) {
      trend_text <- ifelse(
        trajectory_data$Risk[length(history)] < trajectory_data$Risk[1],
        "Improving Trend",
        ifelse(
          trajectory_data$Risk[length(history)] > trajectory_data$Risk[1],
          "Increasing Trend",
          "Stable Trend"
        )
      )
      trend_color <- ifelse(
        trajectory_data$Risk[length(history)] < trajectory_data$Risk[1],
        "green",
        ifelse(
          trajectory_data$Risk[length(history)] > trajectory_data$Risk[1],
          "red",
          "orange"
        )
      )
    } else {
      trend_text <- "First prediction"
      trend_color <- "gray"
    }

    # Determine y-axis range dynamically based on data
    y_min <- max(0, min(trajectory_data$Risk) - 10)
    y_max <- min(100, max(trajectory_data$Risk) + 10)

    # Create line chart without threshold line
    fig <- plot_ly(trajectory_data, x = ~Session, y = ~Risk,
                   type = 'scatter', mode = 'lines+markers',
                   line = list(color = 'coral', width = 3),
                   marker = list(size = 10, color = 'coral',
                                 line = list(color = 'darkred', width = 2)),
                   text = ~paste0("Prediction #", Session,
                                  "<br>Risk: ", round(Risk, 1), "%",
                                  "<br>Time: ", Timestamp),
                   hoverinfo = 'text',
                   name = 'Your Risk') |>
      layout(
        title = list(text = paste0("Risk Trajectory: ", trend_text),
                     font = list(size = 13, color = trend_color)),
        xaxis = list(
          title = "Prediction #",
          titlefont = list(size = 10),
          tickfont = list(size = 8),
          dtick = 1
        ),
        yaxis = list(
          title = "Risk %",
          titlefont = list(size = 10),
          tickfont = list(size = 8),
          range = c(y_min, y_max)
        ),
        showlegend = FALSE,
        margin = list(l=40, r=10, t=35, b=40)
      )

    fig
  })
}

shinyApp(ui = ui, server = server)
