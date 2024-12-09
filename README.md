# Final Project: Palmer Penguins Statistical Analysis and Shiny App

App: https://fof23p-adrian-muro.shinyapps.io/Final_Project_App/

## Project Overview
This project analyzes the `palmerpenguins` dataset using R and an interactive Shiny app. The objectives include:
- Exploratory Data Analysis (EDA)
- Principal Component Analysis (PCA)
- Predictive Model Building and Evaluation
- Dimensionality Reduction and Clustering

The app is designed to be a user-friendly interface for exploring, visualizing, and modeling data with customizable options.

## Deliverables
1. **Shiny App**: A fully deployed interactive application for analysis.
2. **GitHub Repository**: Contains all R scripts, datasets, and detailed instructions for usage.
3. **Documentation**: Comprehensive README file with screenshots and usage details.

## Features of the Shiny App
### **1. Data Preprocessing**
- Imputation of missing values:
  - Numerical values: Replaced with the median grouped by species.
  - Categorical values: Replaced with the mode grouped by species.
- Summary of missing values for transparency.
- Filter dataset by species, island, and sex.

### **2. Exploratory Data Analysis (EDA)**
- Visualize data using scatter plots, box plots, and histograms.
- Customize variables for X and Y axes, grouping factors, and plot titles.
- Display filtered data and generate dynamic summaries.

### **3. Principal Component Analysis (PCA)**
- Visualize variance explained by each principal component using scree plots.
- Explore relationships with PCA biplots (customizable axes).
- Reduce dataset dimensions to analyze dominant patterns.

### **4. Predictive Modeling**
- Build and save multiple linear regression models.
- Apply variable transformations (`log`, `sqrt`, `inverse`) to satisfy assumptions.
- Evaluate models with diagnostic tools:
  - Residual Plots
  - Q-Q Plots
  - Scale-Location Plots
- Compare models using Adjusted R², AIC, and BIC.

### Folder Structure
- **Data**: Contains the cleaned and scaled datasets (`cleaned_penguins.csv`, `scaled_penguins.csv`).
- **Initial Visualizations**: Includes visualizations created during the exploratory data analysis phase.
- **Enhanced Visualizations**: Contains improved and final visualizations.
- **PCA Visualizations**: Includes PCA-specific plots like scree plots and biplots.
- **Final_Project_App**: Shiny app files and scripts for deploying the project.

## Installation Instructions
### Prerequisites
Ensure you have R and RStudio installed.

### Steps
1. **Clone the repository**:
   ```bash
   git clone https://github.com/username/PalmerPenguinsApp.git

2. **Installl the required R packages:**
   install.packages(c("shiny", "ggplot2", "dplyr", "palmerpenguins"))
devtools::install_github("vqv/ggbiplot")  # For PCA biplots

3. **Run the app locally: Open the app script in RStudio and run:**
shiny::runApp("path/to/app.R")
