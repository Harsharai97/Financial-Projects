# Install necessary packages
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tsibble")
install.packages("lubridate")
install.packages("forecast")
install.packages("tidyverse")
install.packages("tibbletime")
install.packages("zoo")
install.packages("PerformanceAnalytics")
install.packages("stargazer")
install.packages("car")  # for VIF, ncvTest
install.packages("lmtest")  # for durbinWatsonTest
install.packages("tseries")
install.packages("lmtest")


# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tsibble) # for time series data handling
library(lubridate) # for date-time operations
library(forecast) # for time series forecasting and analysis
library(tidyverse)
library(tibbletime)
library(zoo)
library(PerformanceAnalytics)
library(stargazer)
library(car)
library(lmtest)
library(tseries)


# Load your data from an Excel file
data <- read_excel("D:/6. Masters in Business Analytics/Multivariate & Regression/Final Project/Dataset/Cleaning/5/MergedData_Clean.xlsx")


# Check column names to ensure they match with your model variables
print(colnames(data))

# You might need to convert date column to Date type if it's not already
data$date <- as.Date(data$date, format="%Y-%m-%d")

# Check if the data is loaded correctly
if (!exists("data")) {
  stop("Data not loaded correctly")
}


# 'Bitcoin_Price' and 'Ethereum_Price' as independent variables
model <- lm(Dollar_index ~ Bitcoin_price + Ethereum_price, data = data)


# Check the summary of the model
summary(model)

# Descriptive Statistics
descriptive_stats <- summary(data)

# Print Descriptive Statistics
print("Descriptive Statistics:")
print(descriptive_stats)

# Correlation Analysis
# Selecting only the key variables for correlation analysis
key_variables <- data[, c("Bitcoin_price", "Ethereum_price", "Dollar_index")]

# Calculating correlation matrix
correlation_matrix <- cor(key_variables, use = "complete.obs")

# Print Correlation Matrix
print("Correlation Matrix:")
print(correlation_matrix)

## Time Series Analysis of Dollar Index and Cryptocurrency Prices

# Ensure the date column is in Date format
data$date <- as.Date(data$date)

# Convert the data into a time series 'tsibble' object
data_ts <- as_tsibble(data, index = date)


# Create separate time series objects
bitcoin_ts <- data_ts %>% select(date, Bitcoin_price)
ethereum_ts <- data_ts %>% select(date, Ethereum_price)
dollar_index_ts <- data_ts %>% select(date, Dollar_index)


# Plotting Bitcoin Price Time Series
ggplot(bitcoin_ts, aes(x = date, y = Bitcoin_price)) +
  geom_line() +
  ggtitle("Bitcoin Price Time Series") +
  xlab("Date") +
  ylab("Bitcoin Price")


# Plotting Ethereum Price Time Series
ggplot(ethereum_ts, aes(x = date, y = Ethereum_price)) +
  geom_line() +
  ggtitle("Ethereum Price Time Series") +
  xlab("Date") +
  ylab("Ethereum Price")


# Plotting Dollar Index Time Series
ggplot(dollar_index_ts, aes(x = date, y = Dollar_index)) +
  geom_line() +
  ggtitle("Dollar Index Time Series") +
  xlab("Date") +
  ylab("Dollar Index")



# Use stargazer to generate a summary table of the model
stargazer(model, type = "text")








# Check for linearity
predictor_vars <- names(coef(model))[-1]  # exclude the intercept
for (var in predictor_vars) {
  print(crPlots(model, variable = var))
}

# Check for multicollinearity
print(vif(model))

# Check for normality of residuals
hist(resid(model), main="Histogram of Residuals", xlab="Residuals")



# Check for homoscedasticity
ncvTest(model)

# Check for normality
qqPlot(model, main="QQ Plot for Linear Model")





# Assuming your time series data is in a dataframe called 'data' with columns 'date', 'Bitcoin_price', 'Ethereum_price', and 'Dollar_index'
# Make sure 'date' is of class Date
data$date <- as.Date(data$date, format = "%Y-%m-%d")  # Adjust the format as necessary to match your date format

# Assuming your data starts in January 2019 and is monthly, replace '2019' and '1' with your actual start year and month
bitcoin_ts <- ts(data$Bitcoin_price, start = c(2019, 1), frequency = 12)
ethereum_ts <- ts(data$Ethereum_price, start = c(2019, 1), frequency = 12)
dollar_index_ts <- ts(data$Dollar_index, start = c(2019, 1), frequency = 12)

# Check for stationarity using Augmented Dickey-Fuller Test
adf_bitcoin <- adf.test(bitcoin_ts, alternative = "stationary")
adf_ethereum <- adf.test(ethereum_ts, alternative = "stationary")
adf_dollar_index <- adf.test(dollar_index_ts, alternative = "stationary")

# Output the results of the ADF test
print(adf_bitcoin)
print(adf_ethereum)
print(adf_dollar_index)

# If non-stationary, difference the series to achieve stationarity
# For example, if Bitcoin is non-stationary:
diff_bitcoin_ts <- diff(bitcoin_ts)

# Re-run ADF test on differenced series
adf_diff_bitcoin <- adf.test(diff_bitcoin_ts, alternative = "stationary")
print(adf_diff_bitcoin)

# Create a combined data frame for the Granger causality test
combined_ts <- data.frame(
  date = as.Date(time(bitcoin_ts)),
  Bitcoin_price = as.numeric(bitcoin_ts),
  Ethereum_price = as.numeric(ethereum_ts),
  Dollar_index = as.numeric(dollar_index_ts)
)

# Ensure all series are aligned and of the same length
combined_ts <- na.omit(combined_ts)

# Checking the structure of the combined data frame
str(combined_ts)

# Recreate the combined data frame
combined_ts <- data.frame(
  date = as.Date(time(bitcoin_ts)),
  Bitcoin_price = as.vector(bitcoin_ts),
  Ethereum_price = as.vector(ethereum_ts),
  Dollar_index = as.vector(dollar_index_ts)
)

# Remove rows with NA values
combined_ts <- na.omit(combined_ts)



# Check the structure of the combined data frame
str(combined_ts)

# Create lagged variables manually
combined_ts$lag_Bitcoin_price <- c(NA, combined_ts$Bitcoin_price[-length(combined_ts$Bitcoin_price)])
combined_ts$lag_Ethereum_price <- c(NA, combined_ts$Ethereum_price[-length(combined_ts$Ethereum_price)])

# Remove NA values
combined_ts <- na.omit(combined_ts)

# Linear regression model for Granger Causality
model_granger_bitcoin <- lm(Dollar_index ~ lag_Bitcoin_price + Ethereum_price, data = combined_ts)
model_granger_ethereum <- lm(Dollar_index ~ Bitcoin_price + lag_Ethereum_price, data = combined_ts)

# Summary of the models
summary(model_granger_bitcoin)
summary(model_granger_ethereum)
