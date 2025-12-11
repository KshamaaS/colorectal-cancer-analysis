# Colorectal Cancer Risk Analysis

**Authors:** Rishika Mamidibathula (rm4318) and Kshamaa Suresh (ks4423)

**Dataset** https://www.kaggle.com/datasets/ziya07/colorectal-cancer-dietary-and-lifestyle-dataset/data

## Project Overview

This project explores colorectal cancer risk factors through interactive data visualization techniques. Using real patient data from over 1,000 participants (with maintained confidentiality), we analyze the complex relationships between demographic variables, lifestyle patterns, nutritional intake, and cancer risk. Our work demonstrates how interactive visualizations can transform complex multivariate health data into accessible and actionable insights.

## Live Demo

**📊 View the Project:** [https://kshamaas.github.io/colorectal-cancer-analysis/](https://kshamaas.github.io/colorectal-cancer-analysis/)

**Interactive Applications:**
- [Parallel Coordinates Visualization](https://qpwgv0-kshamaa-suresh.shinyapps.io/parallel_coordinates_plot/)
- [Risk Prediction Calculator](https://qpwgv0-kshamaa-suresh.shinyapps.io/risk_prediction/)

## Key Features

### Interactive Parallel Coordinates Plot
An interactive visualization allowing dynamic exploration of relationships between multiple risk factors simultaneously. Users can select variable combinations, adjust opacity, and filter specific value ranges to discover patterns across demographic, lifestyle, and nutritional factors.

### Risk Prediction Calculator
A personalized risk assessment tool powered by logistic regression that predicts colorectal cancer risk based on individual health profiles. Features include:
- Real-time risk calculation with visual gauge
- What-if scenario simulator for exploring lifestyle modifications
- Risk contribution breakdown showing which factors drive predictions
- Comparison to healthy ranges and low-risk profiles
- Risk trajectory tracking across multiple predictions

### Static Visualizations
Comprehensive exploratory data analysis including distributions, correlations, and categorical variable relationships that provide foundational insights into the dataset.

## Dataset

The analysis uses a dataset of 1,000+ real patient records with the following variables:
- **Demographic:** Age, Gender, Ethnicity
- **Health Metrics:** BMI, Family History of CRC, Pre-existing Conditions
- **Lifestyle:** Activity patterns (Active, Moderate Exercise, Sedentary, Smoker)
- **Nutritional Intake:** Carbohydrates, Proteins, Fats, Vitamin A, Vitamin C, Iron
- **Outcome:** CRC Risk (Binary: At Risk / No Risk)

## Technologies Used

- **R & RStudio** - Data analysis and visualization
- **Quarto** - Documentation and website generation
- **Shiny** - Interactive web applications
- **Plotly** - Interactive visualizations
- **ggplot2** - Static visualizations
- **GitHub Pages** - Static website hosting
- **shinyapps.io** - Shiny application hosting

## Key Findings

- Lifestyle patterns, particularly smoking combined with sedentary behavior, show strong associations with elevated cancer risk
- Family history acts as a risk amplifier that modifies the effects of other factors
- At-risk individuals tend toward higher carbohydrate and fat intake with lower vitamin C levels
- Interactive visualizations reveal multivariate patterns that would require dozens of static charts to capture

## Reproducibility

This project follows reproducible research principles:
1. All analysis code is embedded in Quarto documents
2. Data processing and visualization steps are documented
3. Interactive applications can be run locally or accessed via cloud deployment
4. Version control through Git/GitHub ensures transparency

## License

This project is created for educational purposes as part of coursework at Columbia University.

## Contact

For questions or collaboration:
- Kshamaa Suresh: ks4423@columbia.edu
- Rishika Mamidibathula: rm4318@columbia.edu

---

*Source files for final project. Data confidentiality maintained throughout analysis.*
