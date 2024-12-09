# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(palmerpenguins)

# Load and clean dataset
data("penguins")

# Custom function to calculate mode
calculate_mode <- function(x) {
  x <- na.omit(x)
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Clean the dataset
penguins <- penguins %>%
  group_by(species) %>%
  mutate(
    bill_length_mm = ifelse(is.na(bill_length_mm), median(bill_length_mm, na.rm = TRUE), bill_length_mm),
    bill_depth_mm = ifelse(is.na(bill_depth_mm), median(bill_depth_mm, na.rm = TRUE), bill_depth_mm),
    flipper_length_mm = ifelse(is.na(flipper_length_mm), median(flipper_length_mm, na.rm = TRUE), flipper_length_mm),
    body_mass_g = ifelse(is.na(body_mass_g), median(body_mass_g, na.rm = TRUE), body_mass_g),
    sex = ifelse(is.na(sex), as.character(calculate_mode(sex)), as.character(sex))
  ) %>%
  ungroup()

# Normalize the numerical data for PCA
penguins_scaled <- scale(penguins %>%
                           select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g))

# Perform PCA on the scaled dataset
pca_result <- prcomp(penguins_scaled, center = TRUE, scale. = TRUE)

# Shiny App UI
ui <- fluidPage(
  titlePanel("Interactive EDA, PCA, and Modeling for Palmer Penguins"),
  sidebarLayout(
    sidebarPanel(
      # Navigation between analysis types
      selectInput("analysisType", "Choose Analysis Type:", 
                  choices = c("EDA", "PCA", "Summary Stats & Modeling")),
      
      # Filters for species, island, and sex
      conditionalPanel(
        condition = "input.analysisType != 'EDA'",
        checkboxGroupInput("speciesFilter", "Select Species:", choices = unique(penguins$species), selected = unique(penguins$species)),
        checkboxGroupInput("islandFilter", "Select Island:", choices = unique(penguins$island), selected = unique(penguins$island)),
        checkboxGroupInput("sexFilter", "Select Sex:", choices = unique(penguins$sex), selected = unique(penguins$sex))
      ),
      
      # Inputs for EDA
      conditionalPanel(
        condition = "input.analysisType == 'EDA'",
        selectInput("plotType", "Choose Plot Type:", 
                    choices = c("Scatter Plot", "Box Plot", "Histogram")),
        uiOutput("xVarInput"),
        uiOutput("yVarInput"),
        selectInput("colorVar", "Group By:", choices = names(penguins)),
        textInput("plotTitle", "Custom Plot Title:", "EDA Plot")
      ),
      
      # Inputs for PCA
      conditionalPanel(
        condition = "input.analysisType == 'PCA'",
        selectInput("xComp", "X-Axis Component:", 
                    choices = paste0("PC", 1:4), selected = "PC1"),
        selectInput("yComp", "Y-Axis Component:", 
                    choices = paste0("PC", 1:4), selected = "PC2")
      ),
      
      # Inputs for modeling
      conditionalPanel(
        condition = "input.analysisType == 'Summary Stats & Modeling'",
        selectInput("responseVar", "Response Variable:", choices = names(penguins)),
        selectInput("predictorVars", "Predictor Variables:", choices = names(penguins), multiple = TRUE),
        checkboxGroupInput("transformVars", "Transform Variables (if needed):", choices = c("log", "sqrt", "inverse")),
        actionButton("buildModel", "Build Model"),
        actionButton("saveModel", "Save Model"),
        selectInput("diagnosticPlot", "Select Diagnostic Plot:", 
                    choices = c("Residual Plot", "Q-Q Plot", "Scale-Location Plot"))
      )
    ),
    mainPanel(
      # Output panel dynamically switches between views
      conditionalPanel(
        condition = "input.analysisType == 'EDA'",
        plotOutput("edaPlot")
      ),
      conditionalPanel(
        condition = "input.analysisType == 'PCA'",
        plotOutput("pcaPlot"),
        plotOutput("screePlot")
      ),
      conditionalPanel(
        condition = "input.analysisType == 'Summary Stats & Modeling'",
        verbatimTextOutput("summaryStats"),
        verbatimTextOutput("modelSummary"),
        plotOutput("modelDiagnostics"),
        tableOutput("modelComparison")
      ),
      
      # Display data and missing value summary
      textOutput("missingValues"),
      dataTableOutput("dataView")
    )
  )
)

# Shiny App Server
server <- function(input, output, session) {
  # Reactive filtered dataset
  filteredData <- reactive({
    req(input$speciesFilter, input$islandFilter, input$sexFilter)
    penguins %>%
      filter(
        species %in% input$speciesFilter,
        island %in% input$islandFilter,
        sex %in% input$sexFilter
      )
  })
  
  # EDA Outputs
  output$xVarInput <- renderUI({
    selectInput("xVar", "X Variable:", choices = names(penguins))
  })
  
  output$yVarInput <- renderUI({
    if (input$plotType %in% c("Scatter Plot", "Box Plot")) {
      selectInput("yVar", "Y Variable:", choices = names(penguins))
    }
  })
  
  edaPlot <- reactive({
    gg <- ggplot(penguins, aes_string(x = input$xVar, color = input$colorVar))
    if (input$plotType == "Scatter Plot") {
      gg <- gg + aes_string(y = input$yVar) + geom_point(size = 3, alpha = 0.7)
    } else if (input$plotType == "Box Plot") {
      gg <- gg + aes_string(y = input$yVar, fill = input$colorVar) + geom_boxplot()
    } else if (input$plotType == "Histogram") {
      gg <- gg + geom_histogram(binwidth = 200, fill = "steelblue", color = "black")
    }
    gg + labs(title = input$plotTitle) + theme_minimal()
  })
  
  output$edaPlot <- renderPlot({ edaPlot() })
  
  # PCA Scree Plot
  output$screePlot <- renderPlot({
    explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
    scree_plot <- data.frame(
      Principal_Component = paste0("PC", 1:length(explained_variance)),
      Variance_Explained = explained_variance
    )
    ggplot(scree_plot, aes(x = Principal_Component, y = Variance_Explained)) +
      geom_col(fill = "steelblue") +
      geom_line(aes(group = 1), color = "red", size = 1) +
      geom_point(color = "red", size = 2) +
      labs(
        title = "Scree Plot: Variance Explained by Principal Components",
        x = "Principal Components",
        y = "Variance Explained"
      ) +
      theme_minimal()
  })
  
  # PCA Biplot
  output$pcaPlot <- renderPlot({
    ggbiplot(pca_result,
             choices = c(as.numeric(sub("PC", "", input$xComp)),
                         as.numeric(sub("PC", "", input$yComp))),
             obs.scale = 1,
             var.scale = 1,
             groups = penguins$species,
             ellipse = TRUE,
             circle = TRUE) +
      labs(title = "PCA Biplot: Selected Principal Components") +
      theme_minimal() +
      scale_color_brewer(palette = "Set2")
  })
  
  # Dynamic Summary Statistics
  output$summaryStats <- renderPrint({
    summary(filteredData())
  })
  
  # Reactive storage for models
  models <- reactiveValues(savedModels = list())
  
  # Build Model with Transformation
  observeEvent(input$buildModel, {
    req(input$responseVar, input$predictorVars)
    formula <- paste(input$responseVar, "~", paste(input$predictorVars, collapse = "+"))
    if ("log" %in% input$transformVars) formula <- paste("log(", input$responseVar, ") ~", paste(input$predictorVars, collapse = "+"))
    if ("sqrt" %in% input$transformVars) formula <- paste("sqrt(", input$responseVar, ") ~", paste(input$predictorVars, collapse = "+"))
    if ("inverse" %in% input$transformVars) formula <- paste("I(1/", input$responseVar, ") ~", paste(input$predictorVars, collapse = "+"))
    models$currentModel <- lm(as.formula(formula), data = filteredData())
  })
  
  # Save Model
  observeEvent(input$saveModel, {
    req(models$currentModel)
    models$savedModels[[paste0("Model_", length(models$savedModels) + 1)]] <- models$currentModel
  })
  
  # Model Summary
  output$modelSummary <- renderPrint({
    req(models$currentModel)
    summary(models$currentModel)
  })
  
  # Diagnostic Plots
  output$modelDiagnostics <- renderPlot({
    req(models$currentModel, input$diagnosticPlot)
    par(mfrow = c(1, 1))
    if (input$diagnosticPlot == "Residual Plot") {
      plot(models$currentModel, which = 1)
    } else if (input$diagnosticPlot == "Q-Q Plot") {
      plot(models$currentModel, which = 2)
    } else if (input$diagnosticPlot == "Scale-Location Plot") {
      plot(models$currentModel, which = 3)
    }
  })
  
  # Model Comparison
  output$modelComparison <- renderTable({
    req(models$savedModels)
    data.frame(
      Model = names(models$savedModels),
      Adj_R2 = sapply(models$savedModels, function(m) summary(m)$adj.r.squared),
      AIC = sapply(models$savedModels, AIC),
      BIC = sapply(models$savedModels, BIC)
    )
  })
  
  # Display data and missing value summary
  output$dataView <- renderDataTable({ penguins })
  
  output$missingValues <- renderText({
    paste("Total Missing Values:", sum(is.na(penguins)))
  })
}

# Run the Shiny App
shinyApp(ui, server)
