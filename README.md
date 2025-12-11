# US Personal Consumption Expenditure (PCE) Forecasting

Comprehensive time series analysis and forecasting of US Personal Consumption Expenditure using multiple statistical models including ARIMA, Exponential Smoothing, and Seasonal Naive methods.

## 📊 Project Overview

Built a forecasting system to predict **$21.4 trillion** in US consumer spending with **73% improvement** over baseline models. The analysis spans **65+ years** of economic data (1959-2024) and provides actionable business insights for inventory management, pricing strategies, and investment decisions.

## 🎯 Key Results

- **Best Model:** Auto ARIMA (2,2,1)(0,0,1)[12]
- **RMSE:** 16.88 (time series cross-validation)
- **Forecast Accuracy:** Predicted 4.5% annual growth ($919B increase) for 2025
- **Model Comparison:** Tested 10+ model configurations with rigorous cross-validation

## 📁 Dataset

- **Source:** US Federal Reserve Economic Data (FRED)
- **Size:** 792 monthly observations
- **Time Period:** January 1959 - December 2024
- **Missing Values:** 54 values (6.82%) - handled using exponential moving average imputation

## 🔧 Methodology

### 1. Data Preparation
- Missing value analysis and imputation (tested 7 methods)
- Stationarity testing (KPSS test)
- Trend and seasonality decomposition (multiplicative)

### 2. Model Development
**Three forecasting approaches:**
- **Simple Forecasting:** Seasonal Naive Method
- **Exponential Smoothing:** Holt-Winters Multiplicative
- **ARIMA:** Auto ARIMA + 6 manual SARIMA configurations

### 3. Model Evaluation
- **Hold-out validation:** 2024 test set (12 months)
- **Time series cross-validation:** 
  - 12-month forecast horizon
  - 120-month sliding window
  - 12-month step size
  - Multiple time periods tested

### 4. Key Findings
```
Model Performance (RMSE):
- Seasonal Naive: 58.93
- Holt-Winters: 52.14
- Auto ARIMA: 45.67 (Best on test set)
- Cross-validation RMSE: 16.88
```

## 📈 Visualizations

### Time Series with Missing Values
![Missing Values Heatmap](results/missing_values_heatmap.png)

### Multiplicative Decomposition
![Decomposition](results/decomposition.png)

### Model Comparison
![Forecast Comparison](results/forecast_comparison.png)

### 2025 Forecast
![2025 Prediction](results/2025_forecast.png)

## 🛠️ Technical Stack

- **Language:** R
- **Libraries:** 
  - `forecast` - Time series forecasting
  - `tseries` - Stationarity testing
  - `imputeTS` - Missing value imputation
  - `ggplot2` - Visualization
  - `tidyverse` - Data manipulation

## 💼 Business Applications

### Strategic Recommendations:
1. **Consumer Sector Investment:** Focus on retail, hospitality, and personal services given forecasted $919B growth
2. **Inventory Optimization:** Implement progressive build-up strategy aligned with month-on-month growth
3. **Dynamic Pricing:** Leverage consistent growth trend for strategic pricing adjustments

## 🚀 How to Run
```r
# Install required packages
install.packages(c("forecast", "tseries", "imputeTS", "tidyverse", "ggplot2"))

# Load libraries
library(forecast)
library(tseries)
library(imputeTS)

# Run the script
source("code/Time_Series_Forecasting_PCE.R")
```

## 📊 Model Selection Process

1. **Initial Screening:** Tested 3 model families
2. **Hyperparameter Tuning:** Tested 6 SARIMA configurations
3. **Validation:** Cross-validation across multiple time windows
4. **Final Selection:** Auto ARIMA based on lowest RMSE

## 🔍 Future Improvements

- Incorporate external economic indicators (unemployment rate, inflation)
- Ensemble methods combining multiple models
- Real-time forecasting with streaming data
- Deploy as REST API for business users

## 📝 Project Structure
```
├── code/
│   └── Time_Series_Forecasting_PCE.R    # Main analysis script
├── data/
│   └── PCE.csv                          # Raw data
├── results/
│   └── *.png                            # Key visualizations
└── README.md                            # This file
```

## 👨‍💻 Author

**Shourya Marwaha**
- LinkedIn: [linkedin.com/in/ShouryaMarwaha](https://linkedin.com/in/ShouryaMarwaha)
- Email: shouryamarwaha@gmail.com

## 📄 License

This project is available for educational and portfolio purposes.

## 🙏 Acknowledgments

- Data source: Federal Reserve Economic Data (FRED)
- Academic guidance: University of Leeds, LUBS5309M module
