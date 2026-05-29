# Airbnb Price Prediction with Machine Learning

Predicts London Airbnb listing prices using data science and machine learning methods. Covers full data pipeline of preprocessing, EDA, modeling, and visualization. Models use OLS, Elastic Net, Random Forest, and Feedforward Neural Network. Dataset sourced from Inside Airbnb (Dec 2024). Built in R.

---

## Overview

This project applies a structured data science pipeline to predict the price of Airbnb listings in London. The analysis spans data preprocessing, exploratory data analysis, and the implementation of four model types: OLS, Elastic Net, Random Forest, Feedforward Neural Network. A key focus is the **trade-off between predictive performance and interpretability** across these models.

---

## Dataset

- **Source:** [Inside Airbnb](http://insideairbnb.com/get-the-data/)
- **Coverage:** London listings, last trimester of 2024 (collected 11/12/2024)
- **Target variable:** Listing price (log-transformed to handle positive skewness)

Place `LONlistings.csv` in the `data/` folder before running the script.

---

## Pipeline

### 1. Data Preprocessing
- Removed irrelevant variables (IDs, URLs, redundant metrics)
- Resolved collinearity groups via correlation matrices
- Converted variable types (logical → integer, character → factor/numeric)
- Split `amenities` into binary dummy variables using occurrence range filtering (25%–75%)
- Applied mean/median imputation for missing values
- Removed outliers from continuous variables

### 2. Exploratory Data Analysis (EDA)
- Feature selection across continuous, binary, and categorical variable types
- Continuous features ranked by variance × correlation with log price
- Binary features selected by mean log price difference with t-test significance
- Categorical features selected by spread in log price across categories
- Visualised distributions, scatterplots, and boxplots for selected features
- Correlation matrix used to detect multicollinearity among continuous features

### 3. Principal Component Analysis (PCA)
- Applied to continuous features to reduce dimensionality
- First 6 principal components explain over 90% of cumulative variance
- PCA-based OLS compared against standardised feature OLS (standardised variables produced higher R-squared and greater interpretability, and were used in subsequent models)

---

## Models

### Linear Models

| Model | RMSE | R² | MAE |
|---|---|---|---|
| OLS | 0.4396 | 0.6196 | 0.3342 |
| Elastic Net | 0.3960 | 0.6913 | 0.2963 |

- **OLS:** baseline linear model minimising residual sum of squares
- **Elastic Net:** combines LASSO (variable selection) and Ridge (coefficient shrinkage) penalties; tuned via five-fold cross-validation; retains and ranks individual features by importance

### Non-Linear Models

| Model | RMSE | R² | MAE |
|---|---|---|---|
| Random Forest | 0.3806 | 0.7154 | 0.2876 |
| FNN | 0.3746 | 0.7179 | 0.2801 |

- **Random Forest:** bootstrap aggregation across 300 decision trees; captures non-linear feature interactions without explicit transformation; ranks feature importance by %IncMSE
- **Feedforward Neural Network (FNN):** single hidden layer (5 units, ReLU activation); weight decay regularisation; highest predictive performance across all models

---

## Prediction vs Interpretability

Model performance improved consistently with complexity. However, predictive strength comes at the cost of interpretability:

- **FNN** achieved the highest R² (0.7179) but is a black-box model with no feature-level explanations
- **Random Forest** offers strong performance (R² = 0.7154) with feature importance rankings
- **Elastic Net** provides clear coefficient-level interpretability with competitive performance
- **OLS** has lowest complexity, but weakest in predictive power

Both Elastic Net and Random Forest identified **neighbourhood, accommodates, and bedrooms** as the most influential predictors of price.

---

## Repository Structure

```
airbnb-price-prediction-ml/
├── data/
│   └── data.md                # Link to dropbox with raw .csv dataset
├── 349Analysis.R              # Full analysis script
├── 349Markdown.pdf            # Report with figures and results
├── README.md
└── REQUIREMENTS.md            # Package and setup requirements
```

---

## Requirements

See [REQUIREMENTS.md](REQUIREMENTS.md) for full details on R version and package dependencies.
