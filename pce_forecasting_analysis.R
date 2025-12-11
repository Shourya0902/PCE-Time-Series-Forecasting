#Time Series Forecasting of Personal Consumption Expenditure (PCE)

#Importing libraries
library(forecast)
library(tidyverse)   
library(tseries)    
library(imputeTS)    
library(lubridate)   
library(zoo)        
library(ggplot2)     
library(gridExtra)   
library(MASS)        
library(lmtest)      
library(grid)      
library(ggfortify)
library(naniar)
library(viridis)

#Importing Data
data = read.csv("PCE.csv")
head(data)

#Exploratory Data Analysis
str(data)
summary(data)

#Checking type of columns
typeof(data$observation_date)
typeof(data$PCE)

#Creating time series of the data
ipts = ts(data$PCE,start=c(1959, 1,1), end=c(2024, 12,1), frequency=12)
ipts

#Plotting the time series
plot(ipts, main = "Time Series with Missing Values", ylab = "PCE [Billions of Dollars]", xlab = "Year")

#Calculating number of missing values in data
na_count = sum(is.na(ipts))
na_percent = na_count / nrow(data) * 100
cat("Missing count:", na_count, "\n", "Total count", nrow(data), "\n", "Missing Percentage", na_percent)

#Creating heat map of missing values
missing_df <- data.frame( date = time(ipts),
                          missing = is.na(ipts))

missing_df$year <- floor(missing_df$date)
missing_df$month <- round((missing_df$date - missing_df$year) * 12) + 1
missing_df$month <- factor(month.abb[missing_df$month], levels = month.abb)

#Creating the heatmap
ggplot(missing_df, aes(x = month, y = year)) +
  geom_tile(aes(fill = missing), color = "white") +
  scale_fill_manual(values = c("FALSE" = "#009E73", "TRUE" = "#D55E00"),
                    labels = c("FALSE" = "Present", "TRUE" = "Missing")) +
  labs(title = "Missing Values in PCE Time Series",
       x = "Month", y = "Year", fill = "Status") +
  theme_minimal()

#*******************************************************************************

# DEALING WITH MISSING VALUES

#Evaluating Imputation techniques
ipts = ipts[!is.na(ipts)]
set.seed(123)
test_row_index = sample(1:length(ipts), 20)  
ipts_test = ipts
ipts_test[test_row_index] = NA

#Imputing values using na_interpolation(), na_kalman(), kalman_auto_arima and the na_ma()
imput_interpolation = na_interpolation(ipts_test)
imput_exponential_2 = na_ma(ipts_test, k=2, weighting = "exponential")
imput_exponential_3 = na_ma(ipts_test, k=3, weighting = "exponential")
imput_exponential_4 = na_ma(ipts_test, k=4, weighting = "exponential")
imput_exponential_5 = na_ma(ipts_test, k=5, weighting = "exponential")
imput_kalman = na_kalman(ipts_test)
imput_auto_arima = na_kalman(ipts_test, model="auto.arima")

#Making a dataframe of all imputed data to compare
test_bind =  cbind(ipts,ipts_test,imput_interpolation,imput_exponential_2,imput_exponential_3,imput_exponential_4,imput_exponential_5,imput_kalman,imput_auto_arima)
test_bind =  as.data.frame(test_bind)

#Calculating RMSE values of imputation data
rmse_interpolation = sqrt(mean((test_bind$ipts[test_row_index] - test_bind$imput_interpolation[test_row_index])^2))
rmse_exponential_2 = sqrt(mean((test_bind$ipts[test_row_index] - test_bind$imput_exponential_2[test_row_index])^2))
rmse_exponential_3 = sqrt(mean((test_bind$ipts[test_row_index] - test_bind$imput_exponential_3[test_row_index])^2))
rmse_exponential_4 = sqrt(mean((test_bind$ipts[test_row_index] - test_bind$imput_exponential_4[test_row_index])^2))
rmse_exponential_5 = sqrt(mean((test_bind$ipts[test_row_index] - test_bind$imput_exponential_5[test_row_index])^2))
rmse_imput_kalman = sqrt(mean((test_bind$ipts[test_row_index] - test_bind$imput_kalman[test_row_index])^2))
rmse_imput_auto_arima = sqrt(mean((test_bind$ipts[test_row_index] - test_bind$imput_auto_arima[test_row_index])^2))

#Printing RMSE Values for the data
cat("rmse for interpolation", rmse_interpolation,"\n")  
cat("rmse for exponential (k=2)", rmse_exponential_2, "\n") #Least RMSE
cat("rmse for exponential (k=3)", rmse_exponential_3, "\n")
cat("rmse for exponential (k=4)", rmse_exponential_4, "\n")
cat("rmse for exponential (k=5)", rmse_exponential_5, "\n")
cat("rmse for kalman", rmse_imput_kalman, "\n")
cat("rmse for kalman auto arima", rmse_imput_auto_arima, "\n")

#Imputing using Kalman imputation
ipts = ts(data$PCE,start=c(1959, 1,1), end=c(2024, 12,1), frequency=12)
ipts_imputed = na_ma(ipts, k=2, weighting = "exponential")

#Plotting imputed time series
plot(ipts_imputed, main = "PCE Time Series (Post Imputation)", ylab = "PCE [Billions of Dollars]", xlab = "Year")

#*******************************************************************************

# CHECKING FOR TREND AND SEASONALITY

#Plotting Seasonal Plot
seasonplot(ipts_imputed)

#ANOVA test for seasonality
month_factor <- factor(cycle(ipts_imputed))
season_anova <- aov(as.numeric(ipts_imputed) ~ month_factor)
summary(season_anova)

#Checking if seasonality is multiplicative or additive using rolling SD vs rolling mean
rolling_sd = rollapply(ipts_imputed, width = 12, FUN = sd, align = "right", fill = NA)
rolling_mean =  rollapply(ipts_imputed, width = 12, FUN = mean, align = "right", fill = NA)

sd_vs_mean =  data.frame(Date = time(rolling_sd), Mean = as.vector(rolling_mean), SD = as.vector(rolling_sd))
sd_vs_mean = na.omit(sd_vs_mean)

ggplot(sd_vs_mean, aes(x = Mean, y = SD)) +
      geom_point(alpha = 0.6, color = "#1F77B4") +   # nicer blue tone
      geom_smooth(method = "lm", color = "#FF5733", se = FALSE) +  
      labs( title = "Relationship Between Standard Deviation and Mean Levels",
            x = "12-Month Rolling Mean",
            y = "12-Month Rolling Standard Deviation") + theme_minimal() 

#*******************************************************************************

#Performing Decomposition using decompose
decomp_mult = stats::decompose(ipts_imputed, type="multiplicative")

#Plotting decompositions
plot(decomp_mult)

#Plotting Seasonal pot of multiplicative
plot(decomp_mult$seasonal)

#Log transform the time series object
log_ipts_imputed = log(ipts_imputed)
plot(log_ipts_imputed)

#Performing Decomposition using STL
fit = stl(log_ipts_imputed, s.window="periodic")
plot(fit)

#Splitting data into test and train
train_init = c(1959, 1)
train_end = c(2023, 12)
test_init = c(2024, 1)
test_end = c(2024, 12)

train_ipts = window(ipts_imputed,start = train_init, end = train_end)
test_ipts = window(ipts_imputed, start = test_init, end = test_end)

#*******************************************************************************

#MODEL 1: Applying Seasonal Naive Method

fc_snaive = snaive(train_ipts, h=12) 
plot(fc_snaive, main="Seasonal Naive Forecast",  ylab="PCE [Billions of Dollars]", xlab="Year")
lines(test_ipts, col="red")

legend("topleft", 
       legend = c("Actual", "Forecast", "95% Prediction Interval"),
       col = c("blue", "red", "gray80"), 
       lty = c(1, 1, 1), 
       lwd = c(2, 2, 5),
       bty = "n")

#MODEL 2: Applying Exponential Smoothing Method
# Using ets() function for automatic model selection
fc_ets = ets(train_ipts)
summary(fc_ets)
forecast_ets = forecast(fc_ets, h=12)
plot(fc_ets,ylab="PCE [Billions of Dollars]", xlab="Year")
lines(test_ipts, col="red")

# Comparing the Holts linear and Holts Winter model
fc_holt_linear = holt(train_ipts, h=12)  
fc_hw_mult = hw(train_ipts, seasonal="multiplicative", h=12)

# Calculate accuracy for Holt's Linear
acc_holt_linear <- accuracy(fc_holt_linear, test_ipts)
acc_holt_linear <- as.data.frame(acc_holt_linear)
acc_holt_linear$Model <- "Holt's Linear"

# Calculate accuracy for Holt-Winters Multiplicative
acc_hw_mult <- accuracy(fc_hw_mult, test_ipts)
acc_hw_mult <- as.data.frame(acc_hw_mult)
acc_hw_mult$Model <- "Holt-Winters Multiplicative"

# Combine results into one data frame
model_comparison <- rbind(acc_holt_linear, acc_hw_mult)

# Move 'Model' to the first column for clarity
model_comparison <- model_comparison[, c("Model", setdiff(names(model_comparison), "Model"))]

# Print the comparison table
print(model_comparison)

# Plotting side by side
par(mfrow=c(1,2))
# Plot Holt's Linear forecast
plot(fc_holt_linear, main = "Forecast: Holt's Linear", ylab = "PCE", xlab = "Time", 
     xlim = c(2023, 2025), ylim = c(18000, 20000))
lines(test_ipts, col = "red")

# Plot Holt-Winters Multiplicative forecast
plot(fc_hw_mult, main = "Forecast: Holt-Winters", ylab = "PCE", xlab = "Time", 
     xlim = c(2023, 2025), ylim = c(18000, 20000))
lines(test_ipts, col = "red")
#******************************************************************************

# STATIONARITY AND AUTOCORRELATION ANALYSIS

#Checking for stationarity and determining differencing order
diff_needed <- ndiffs(train_ipts, test = "kpss", alpha = 0.05)
diff_needed

#Taking 2 difference
ipts_diff_2 = diff(diff(train_ipts))

#Testing for stationarity
kpss_diff_result = kpss.test(ipts_diff_2)
print(kpss_diff_result)

#Dealing with NAs in ipts_diff_2
ipts_diff_2_clean = na_interpolation(ipts_diff_2)

#Plotting seasonal ACF and PACF
par(mfrow = c(2, 1))
acf(ipts_diff_2_clean, lag.max = 60, main = "Seasonal ACF (up to 5 years)")
pacf(ipts_diff_2_clean, lag.max = 60, main = "Seasonal PACF (up to 5 years)")
par(mfrow = c(1, 1))



#******************************************************************************

#MODEL 3 : ARIMA Model

#Performing Auto ARIMA 
afit = auto.arima(train_ipts, seasonal=TRUE)
summary(afit)

#Generating forecast from auto ARIMA
forecast_auto_arima = forecast(afit, h=12)

# Plotting 
par(mfrow=c(1,1))
# Plot Auto ARIMA
plot(forecast_auto_arima, main = "Forecast: Auto ARIMA", ylab = "PCE", xlab = "Time", 
     xlim = c(2023, 2025), ylim = c(18000, 20000))
lines(ipts_imputed, col = "black")  # Actual values
legend("topleft", legend = c("Actual", "Auto ARIMA Forecast"), col = c("black", "blue"), lty = 1, bty = "n")

#Performing manual ARIMA or SARIMA 
fit_sarima_1 = Arima(train_ipts, order=c(2,2,1), seasonal=list(order=c(1,2,0), period=12))
fit_sarima_2 = Arima(train_ipts, order=c(1,2,1), seasonal=list(order=c(1,2,1), period=12))
fit_sarima_3 = Arima(train_ipts, order=c(0,2,1), seasonal=list(order=c(0,2,1), period=12))
fit_sarima_4 = Arima(train_ipts, order=c(1,2,0), seasonal=list(order=c(0,2,1), period=12))
fit_sarima_5 = Arima(train_ipts, order=c(2,2,0), seasonal=list(order=c(1,2,0), period=12))
fit_sarima_6 = Arima(train_ipts, order=c(1,2,1), seasonal=list(order=c(0,2,1), period=12))
summary(fit_sarima_1)
summary(fit_sarima_2)
summary(fit_sarima_3)
summary(fit_sarima_4)
summary(fit_sarima_5)
summary(fit_sarima_6)

#Generate forecasts from manual SARIMA
forecast_sarima_1 = forecast(fit_sarima_1, h=12)
forecast_sarima_2 = forecast(fit_sarima_2, h=12)
forecast_sarima_3 = forecast(fit_sarima_3, h=12)
forecast_sarima_4 = forecast(fit_sarima_4, h=12)
forecast_sarima_5 = forecast(fit_sarima_5, h=12)
forecast_sarima_6 = forecast(fit_sarima_6, h=12)

# Create a combined plot comparing all SARIMA models against actual values
# Setting up the plotting area with appropriate size
par(mfrow=c(1,1), mar=c(4, 4, 3, 10))  # Increased right margin for legend

# Plot the actual values first
plot(test_ipts, main="Comparison of SARIMA Forecasts vs Actual Values", 
     xlab="Time", ylab="PCE [Billions of Dollars]",
     xlim = c(2024,2026), ylim=c(18000,21000), 
     lwd=2, col="black")

# Add each forecast with different colors and line types
lines(forecast_sarima_1$mean, col="red", lwd=1.5, lty=2)
lines(forecast_sarima_2$mean, col="green", lwd=1.5, lty=3)
lines(forecast_sarima_3$mean, col="purple", lwd=1.5, lty=4)
lines(forecast_sarima_4$mean, col="orange", lwd=1.5, lty=5)
lines(forecast_sarima_5$mean, col="brown", lwd=1.5, lty=6)
lines(forecast_sarima_6$mean, col="cyan", lwd=1.5, lty=1)

# Add a legend with model specifications
legend("topright",legend=c("Actual Values",
                "SARIMA(2,2,1)(1,1,0)[12]",
                "SARIMA(1,2,1)(1,1,1)[12]",
                "SARIMA(0,2,1)(0,1,1)[12]",
                "SARIMA(1,2,0)(0,1,1)[12]",
                "SARIMA(2,2,0)(1,1,0)[12]",
                "SARIMA(1,2,1)(0,1,1)[12]"),
       col=c("black", "blue", "red", "green", "purple", "orange", "brown", "cyan"),
       lty=c(1, 1, 2, 3, 4, 5, 6, 1), lwd=c(2, rep(1.5, 7)), cex=0.8)

# First, extract the AIC values from each model
aic_auto <- afit$aic
aic_sarima_1 <- fit_sarima_1$aic
aic_sarima_2 <- fit_sarima_2$aic
aic_sarima_3 <- fit_sarima_3$aic
aic_sarima_4 <- fit_sarima_4$aic
aic_sarima_5 <- fit_sarima_5$aic
aic_sarima_6 <- fit_sarima_6$aic

# Define model names
model_names <- c(
  "Auto SARIMA",
  "Manual SARIMA(2,2,1)(1,2,0)[12]",
  "Manual SARIMA(1,2,1)(1,2,1)[12]",
  "Manual SARIMA(0,2,1)(0,2,1)[12]",
  "Manual SARIMA(1,2,0)(0,2,1)[12]",
  "Manual SARIMA(2,2,0)(1,2,0)[12]",
  "Manual SARIMA(1,2,1)(0,2,1)[12]")

# Create a data frame with model information
model_info <- data.frame(
  Model = model_names,
  AIC = c(aic_auto, aic_sarima_1, aic_sarima_2, aic_sarima_3, aic_sarima_4, aic_sarima_5, aic_sarima_6))

# Sort by AIC (lower is better)
model_info_by_aic <- model_info[order(model_info$AIC), ]

# Print the table sorted by AIC
print(model_info_by_aic)




#******************************************************************************

# COMPARISON OF THE MODELS

#Setting up cross-validation parameters
h = 12  
window_size = 120  
step_size = 12 

# First, let's fix the cross-validation code by moving the model fitting inside the loop
# Reset cv_results dataframe
cv_results = data.frame(
  Method = character(),
  Window = integer(),
  RMSE = numeric(),
  MAE = numeric(),
  MAPE = numeric(),
  stringsAsFactors = FALSE
)

#Performing cross validation correctly
total_length = length(ipts_imputed)
end_position = total_length - h  

for (i in seq(window_size, end_position, by=step_size)) {
  #Defining training and test sets
  train_end = time(ipts_imputed)[i]
  test_start = time(ipts_imputed)[i+1]
  
  train_data = window(ipts_imputed, end=train_end)
  test_data = window(ipts_imputed, start=test_start)
  
  if (length(test_data) < h) next
  
  #Truncating test data to forecast horizon
  test_data = head(test_data, h)
  
  #Defining window label for results
  window_label = i
  
  #Applying Seasonal Naive model
  fc_snaive = snaive(train_data, h=h)
  metrics_snaive = accuracy(fc_snaive, test_data)[1, c("RMSE", "MAE", "MAPE")]
  
  #Storing Seasonal Naive results
  cv_results = rbind(cv_results, data.frame(
    Method = "Seasonal Naive",
    Window = window_label,
    RMSE = as.numeric(metrics_snaive["RMSE"]),
    MAE = as.numeric(metrics_snaive["MAE"]),
    MAPE = as.numeric(metrics_snaive["MAPE"])
  ))
  
  #Applying Holt-Winters model
  fc_hw_mult = hw(train_data, seasonal="multiplicative", h=h)
  metrics_hw = accuracy(fc_hw_mult, test_data)[1, c("RMSE", "MAE", "MAPE")]
  
  #Storing Holt-Winters results
  cv_results = rbind(cv_results, data.frame(
    Method = "Holt-Winters Multiplicative",
    Window = window_label,
    RMSE = as.numeric(metrics_hw["RMSE"]),
    MAE = as.numeric(metrics_hw["MAE"]),
    MAPE = as.numeric(metrics_hw["MAPE"])
  ))
  
  #Applying Auto ARIMA model
  fit_auto_arima = auto.arima(train_data, seasonal=TRUE)
  fc_auto_arima = forecast(fit_auto_arima, h=h)
  metrics_arima = accuracy(fc_auto_arima, test_data)[1, c("RMSE", "MAE", "MAPE")]
  
  #Storing Auto ARIMA results
  cv_results = rbind(cv_results, data.frame(
    Method = "Auto ARIMA",
    Window = window_label,
    RMSE = as.numeric(metrics_arima["RMSE"]),
    MAE = as.numeric(metrics_arima["MAE"]),
    MAPE = as.numeric(metrics_arima["MAPE"])
  ))
}

# Calculating average performance by method
cv_summary = aggregate(
  cbind(RMSE, MAE, MAPE) ~ Method, 
  data = cv_results, 
  FUN = mean, 
  na.rm = TRUE)

# Print cross-validation summary
print(cv_summary)

# Identify best model based on average RMSE
best_cv_model = cv_summary$Method[which.min(cv_summary$RMSE)]
cat("Best model based on time series cross-validation:", best_cv_model, "\n")

#*******************************************************************************

# COMPARISON OF ALL MODELS ON TEST DATA

fc_snaive <- snaive(train_ipts, h=12)
fc_hw_mult <- hw(train_ipts, seasonal="multiplicative", h=12)
fit_auto_arima <- auto.arima(train_ipts, seasonal=TRUE)
fc_auto_arima <- forecast(fit_auto_arima, h=12)

# Calculate accuracy metrics for each model
accuracy_snaive <- accuracy(fc_snaive, test_ipts)[2,]  
accuracy_hw <- accuracy(fc_hw_mult, test_ipts)[2,]     
accuracy_auto_arima <- accuracy(fc_auto_arima, test_ipts)[2,]

# Create a data frame for comparison
all_models_comparison <- data.frame(
  Model = c("Seasonal Naive", "Holt-Winters Multiplicative", "Auto ARIMA"),
  RMSE = c(accuracy_snaive["RMSE"], accuracy_hw["RMSE"], accuracy_auto_arima["RMSE"]),
  MAE = c(accuracy_snaive["MAE"], accuracy_hw["MAE"], accuracy_auto_arima["MAE"]),
  MAPE = c(accuracy_snaive["MAPE"], accuracy_hw["MAPE"], accuracy_auto_arima["MAPE"])
)

# Print the comparison table
print(all_models_comparison)

# Find the best model based on RMSE
best_model_index <- which.min(all_models_comparison$RMSE)
best_model_name <- all_models_comparison$Model[best_model_index]
cat("\nBest model based on RMSE for 2024 test data:", best_model_name, "\n")

#*******************************************************************************

# FORECAST FOR THE NEXT 12 MONTHS

#Performing Auto ARIMA 
auto_arima_fit_prediction = auto.arima(ipts_imputed, seasonal=TRUE)
summary(auto_arima_fit_prediction)

#Generating forecast from auto ARIMA
forecast_auto_arima_prediction = forecast(auto_arima_fit_prediction, h=12)

# Plotting 
par(mfrow=c(1,1))
# Plot Auto ARIMA
plot(forecast_auto_arima_prediction, main = "Forecast: Auto ARIMA", ylab = "PCE", xlab = "Time", 
     xlim = c(2023, 2026), ylim = c(18000, 25000))
lines(ipts_imputed, col = "black")  # Actual values
legend("topleft", legend = c("Actual", "Auto ARIMA Forecast"), col = c("black", "blue"), lty = 1, bty = "n")

print(forecast_auto_arima_prediction)


#*************************************************************************************

