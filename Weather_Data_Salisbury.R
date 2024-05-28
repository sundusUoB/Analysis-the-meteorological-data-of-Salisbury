#Read the CSV file
data <- read.csv("new_df.csv", header = FALSE, stringsAsFactors = FALSE)
View(data)

# Extract date_time values from the first row
date_times <- unlist(data[1, seq(1, ncol(data), by = 10)])


# Initialize an empty data frame to store the result
result <- data.frame(date_time = character(), TSK = numeric(), PSFC = numeric(), U10 = numeric(), V10 = numeric(), Q2 = numeric(), RAINC = numeric(), RAINNC = numeric(), SNOW = numeric(), TSLB = numeric(), SMOIS = numeric(), stringsAsFactors = FALSE)

# Loop over the columns, starting from column 1 and increment by 11
for (i in seq(1, length(date_times))) {
  # Extract values from the third row
  values <- unlist(data[3, seq((i - 1) * 10 + 1, (i - 1) * 10 + 10)])
  
  # Combine date_time values with column names and values
  temp <- data.frame(date_time = date_times[i], TSK = values[1], PSFC = values[2], U10 = values[3], V10 = values[4], Q2 = values[5], RAINC = values[6], RAINNC = values[7], SNOW = values[8], TSLB = values[9], SMOIS = values[10], stringsAsFactors = FALSE)
  
  # Append the temporary data frame to the result
  result <- rbind(result, temp)
}

# Print the result
View(result)


# Use gsub() to remove the letter 'X' from entries in the first column
result$date_time <- gsub("X", "", result$date_time)

# Print the modified dataframe
View(result)

class(result)

# Check the data type of each column
column_types <- sapply(result, class)

# Print the data types
print(column_types)     

# Convert columns from character to numeric, excluding the first column
result[, -1] <- lapply(result[, -1], as.numeric)

# Print the modified dataframe
str(result)

# Check the data type of each column
column_types <- sapply(result, class)

# Print the data types
print(column_types)  

# Convert the first column from character to POSIXct
result$date_time <- as.POSIXct(result$date_time, format = "%d.%m.%Y.%H.%M")

# Print the modified dataframe
print(column_types)

# Check for missing values in the dataframe
missing_values <- sum(is.na(result$date_time))

# Print the number of missing values in the dataframe
View(missing_values)
# Define the replacement value
replacement_value <- as.POSIXct("2018-05-31 21:00:00", format = "%Y-%m-%d %H:%M:%S")

# Fill NA values in the first column with the replacement value
result$date_time[is.na(result$date_time)] <- replacement_value

# Print the dataframe with missing values filled
print(result)
View(result)



# Load the required libraries
install.packages("dplyr")
library(dplyr)

# make a copy of the result in newdata
newdata <- data.frame(result)

# Create a function that calculates the mean of the predecessor and successor for a given vector with missing values
mean_pred_succ <- function(x) {
  # Find indices of NA values
  na_indices <- which(is.na(x))
  
  # If there are no NA values, return x as is
  if (length(na_indices) == 0) {
    return(x)
  }
  
  # Replace NA values with the mean of the predecessor and successor
  for (i in na_indices) {
    left_val <- ifelse(i == 1, NA, x[i - 1])  # Predecessor
    right_val <- ifelse(i == length(x), NA, x[i + 1])  # Successor
    x[i] <- mean(c(left_val, right_val), na.rm = TRUE)
  }
  
  return(x)
}
# Replace missing values in columns 2 through 6 with the mean of the predecessor and successor
columns_to_fill <- c(2:6, 10, 11)
newdata[, columns_to_fill] <- lapply(newdata[, columns_to_fill], mean_pred_succ)

install.packages("zoo")
library(zoo)
# Function to fill NA values with the last occurred non-missing value
fill_na_with_last_occurred_value <- function(x) {
  last_non_na_value <- na.locf(x, na.rm = FALSE, fromLast = TRUE)
  x[is.na(x)] <- last_non_na_value[is.na(x)]
  return(x)
}

# Apply the function to columns 7, 8, and 9
newdata[, 7:9] <- lapply(newdata[, 7:9], fill_na_with_last_occurred_value)

# Print the modified dataframe
View(newdata)

# Check for missing values in the dataframe
missing_values <-sum( is.na(newdata))


# Print the result
print(missing_values)
View(missing_values)

install.packages("ggplot2")
library(ggplot2)

summary(result)

# Calculate the wind speed using Pythagorean theorem
newdata$Windspeed <- sqrt(newdata$U10^2 + newdata$V10^2)

# Convert temperature from Kelvin to Fahrenheit
newdata <- newdata %>%
  mutate(Temperature_F = (TSK - 273.15) * 1.8 + 32)

# Function to estimate relative humidity from specific humidity and surface pressure
saturation_vapor_pressure <- function(T) {
  Tc <- T - 273.15
  es <- 6.112 * exp((17.67 * Tc) / (Tc + 243.5))
  return(es)
}
specific_humidity_to_rh <- function(q, T, P) {
  es <- saturation_vapor_pressure(T)
  e <- (q * P) / (0.622 + 0.378 * q)
  RH <- (e / es) * 100
  return(RH)
}

# Estimate relative humidity
newdata <- newdata %>%
  mutate(Relative_Humidity = specific_humidity_to_rh(Q2, TSK, PSFC / 100))

# Function to calculate heat index
calculate_heat_index <- function(T, RH) {
  HI <- -42.379 + 2.04901523 * T + 10.14333127 * RH - 0.22475541 * T * RH - 6.83783e-3 * T^2 - 
    5.481717e-2 * RH^2 + 1.22874e-3 * T^2 * RH + 8.5282e-4 * T * RH^2 - 1.99e-6 * T^2 * RH^2
  return(HI)
}
# Calculate heat index
newdata <- newdata %>%
  mutate(Heat_Index = calculate_heat_index(Temperature_F, Relative_Humidity))
# Convert heat index from Fahrenheit to Kelvin
newdata <- newdata %>%
  mutate(Heat_Index_K = (Heat_Index - 32) / 1.8 + 273.15)
newdata <- newdata %>%
  select(-Heat_Index, -Temperature_F, -Relative_Humidity)

View(newdata)

# Print the updated dataframe with the new Windspeed column
View(newdata)
 summary(newdata)
# 4. Correlation Analysis

# Remove the "SNOW" column from newdata as it was giving a warning for SD 0
newdata <- newdata[, !colnames(newdata) %in% "SNOW"]
View(newdata)

correlation_matrix <- cor(newdata[, c("TSK", "PSFC", "U10", "V10", "Q2","RAINC", "RAINNC", "TSLB", "SMOIS","Windspeed")])
print(correlation_matrix)


numeric_cols <- sapply(newdata, is.numeric)

# Subset the dataframe to include only numeric columns
numeric_data <- newdata[, numeric_cols]

# Display the first few rows of the numeric data to confirm
head(numeric_data)


# Ensure all necessary packages are installed and loaded
install.packages("corrplot")
library(corrplot)

# Assuming 'data' is your dataframe
# Select only numerical columns if needed
numeric_data <- newdata[sapply(newdata, is.numeric)]

# Calculate the Spearman correlation for all numerical columns in the dataset
correlation_matrix <- cor(numeric_data, method = "spearman")

# Display the correlation matrix in the console
print(correlation_matrix)

# Visualize the correlation matrix with the corrplot package
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "RED", tl.srt = 60, 
         title = "Spearman Correlation Matrix", 
         addCoef.col = "black")  # Show correlation coefficients in the plot
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
library(tidyr)

# Select only numerical columns if needed
numeric_data <- newdata[sapply(newdata, is.numeric)]
View(numeric_data)

install.packages("gridExtra")
library(gridExtra)

# Generate a list of box plots for each variable
plots <- lapply(names(numeric_data), function(column) {
  ggplot(numeric_data, aes_string(y = column)) +
    geom_boxplot() +
    labs(title = paste("Box plot of", column), x = "", y = column) +
    theme_minimal()
})

# Arrange the plots in a grid layout
grid.arrange(grobs = plots, ncol = 2)


# Box plot for TSK
ggplot(numeric_data, aes(x = "", y = TSK)) +
  geom_boxplot() +
  labs(title = "Box plot of TSK", x = "", y = "TSK")

# Box plot for PSFC
ggplot(newdata, aes(x = "", y = PSFC)) +
  geom_boxplot() +
  labs(title = "Box plot of PSFC", x = "", y = "PSFC")

# Box plot for U10
ggplot(newdata, aes(x = "", y = U10)) +
  geom_boxplot() +
  labs(title = "Box plot of U10", x = "", y = "U10")

# Box plot for V10
ggplot(newdata, aes(x = "", y = V10)) +
  geom_boxplot() +
  labs(title = "Box plot of V10", x = "", y = "V10")

# Box plot for Q2
ggplot(newdata, aes(x = "", y = Q2)) +
  geom_boxplot() +
  labs(title = "Box plot of Q2", x = "", y = "Q2")

# Box plot for RAINC
ggplot(newdata, aes(x = "", y = RAINC)) +
  geom_boxplot() +
  labs(title = "Box plot of RAINC", x = "", y = "RAINC")

# Box plot for RAINNC
ggplot(newdata, aes(x = "", y = RAINNC)) +
  geom_boxplot() +
  labs(title = "Box plot of RAINNC", x = "", y = "RAINNC")

# Box plot for TSLB
ggplot(newdata, aes(x = "", y = TSLB)) +
  geom_boxplot() +
  labs(title = "Box plot of TSLB", x = "", y = "TSLB")

# Box plot for SMOIS
ggplot(newdata, aes(x = "", y = SMOIS)) +
  geom_boxplot() +
  labs(title = "Box plot of SMOIS", x = "", y = "SMOIS")

# Box plot for Heat_index
ggplot(newdata, aes(x = "", y = Heat_Index_K)) +
  geom_boxplot() +
  labs(title = "Box plot of SMOIS", x = "", y = "SMOIS")

install.packages("outliers")
library(outliers)

# Compute the IQR for each column
# Select only numeric columns from the dataframe
numeric_df <- newdata[, sapply(newdata, is.numeric)]
View(numeric_df)
numeric_vector <- unlist(numeric_df)

# Print the resulting numeric vector
print(numeric_vector)
missing_values<-colSums(is.na(numeric_df))
print(missing_values)
IQR_value <- IQR(numeric_vector)

Q1 <- quantile(numeric_vector, 0.25)
Q3 <- quantile(numeric_vector, 0.75)
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
outliers <- numeric_vector[numeric_vector < lower_bound | data > upper_bound]

plot(outliers)


install.packages("ggplot2")
library(ggplot2)
install.packages("qqplotr")
library(qqplotr)


# Create QQ plot for TSK
qq_tsk <- qplot(sample = newdata$TSK, geom = "qq") +
  ggtitle("QQ Plot of TSK") +
  geom_qq_line()

# Print the QQ plot
print(qq_tsk)
install.packages("stats")
library(stats)
newdata_SW<-newdata

# Function to perform the Shapiro-Wilk test for normality on each column
shapiro_wilk_test <- function(newdata_SW) {
  results_sw <- list()
  for (column in names(newdata_SW)) {
    if (is.numeric(newdata_SW[[column]]) && !is.null(newdata_SW[[column]])) {
      test_result <- shapiro.test(newdata_SW[[column]])
      results_sw[[column]] <- c('Statistic' = test_result$statistic, 'p-value' = test_result$p.value)
    }
  }
  return(results_sw)
}
# Drop the date_time column and perform the Shapiro-Wilk test on the remaining columns
newdata_SW <- subset(newdata_SW, select = -c(date_time))  # Drop the date_time column
shapiro_results <- shapiro_wilk_test(newdata_SW)
shapiro_results

# Function to create and show histogram for each variable in numeric_df
create_histograms <- function(numeric_df) {
  par(mfrow=c(ceiling(ncol(numeric_df)/2), 2), mar=c(4, 4, 2, 1))  # Set up the layout for plots
  
  for (col in names(numeric_df)) {
    hist_data <- numeric_df[[col]]
    if (is.numeric(hist_data)) {
      hist(hist_data, breaks = 30, col = 'blue', main = col)
    } else {
      cat("Non-numeric data found in column:", col, "\n")
    }
  }
}

# Call the function to create and show histograms for each variable in numeric_df
create_histograms(numeric_df)


# Plotting the combined histogram for all numeric data
par(mfrow=c(1, 1), mar=c(4, 4, 2, 1))  # Reset the layout to a single plot

# Flatten the numeric data into a vector
numeric_values <- unlist(numeric_df)

# Plot the combined histogram
hist(numeric_values, breaks=100, col='blue', main='Combined Histogram of All Numeric Variables', xlab='Values', ylab='Frequency')


#BIVARIATE ANALYSIS

# Load the required library
install.packages("corrplot")
library(corrplot)
install.packages("RColorBrewer")
library(RColorBrewer)


# Correlation analysis for Question 1
# Selecting the relevant variables
rain_soil_data <- newdata[c('RAINC', 'RAINNC', 'SMOIS')]

# Calculating correlation matrix
correlation_matrix <- cor(rain_soil_data)

# Creating a heatmap to visualize the correlations
corrplot(correlation_matrix, method="color", col=brewer.pal(n=8, name="RdBu"), 
         addCoef.col = "black", number.cex = 0.7, tl.cex = 0.8,
         title='Correlation Matrix for Rainfall and Soil Moisture', 
         mar=c(1,1,1,1))
print(corrplot)

#MULTiVARIATE 

# Load required libraries
library(ggplot2)
library(dplyr)

# Step 1: Data Preparation

# Generate synthetic data
set.seed(100)  # for reproducibility

# Load necessary libraries
install.packages("tidyverse")
library(tidyverse)
data<-data.frame(newdata$Windspeed,newdata$Heat_Index_K, newdata$Q2)
print(names(data))
View(data)


# Fit the linear regression model
model <- lm(newdata.Heat_Index_K ~ newdata.Windspeed +newdata.Q2, data=data)

# Summary of the model to view coefficients and statistics
summary(model)

# Predict and measure performance
predictions <- predict(model, data)
mse <- mean((data$Heat_Index_K - predictions)^2)
r_squared <- summary(model)$r.squared

# Output MSE and R-squared
print(paste("MSE:", mse))
print(paste("R-squared:", r_squared))

# Optionally, plot the residuals to check for patterns
plot(residuals(model))
abline(h = 0, col = "red")

# Load necessary libraries
library(ggplot2)

# Assuming your model and data are already loaded
# Create a new data frame for plotting that includes original data and predictions
data$predictions <- predict(model, data)

# Plotting soil moisture against north-south wind component (V10)
ggplot(data, aes(x = newdata.Windspeed, y = newdata.Q2)) +
  geom_point(aes(color = 'Actual'), alpha = 0.5) +  # Actual data points
  geom_line(aes(y = predictions, color = 'Predicted'), size = 1) +  # Regression line
  labs(x = 'Windspeed', y = 'Humidity Q2', title = 'Regression Analysis') +
  scale_color_manual("", 
                     breaks = c("Actual", "Predicted"),
                     values = c("blue", "red")) +
  theme_minimal()

# You can replace V10 with U10 or Windspeed to see the effects of other variables



#Load necessary libraries
install.packages("caret")
library(caret)
# Load necessary libraries
library(dplyr)
library(lubridate)

# Calculate wind direction in degrees
newdata <- newdata %>%
  mutate(WindDirection = (atan2(V10, U10) * 180 / pi + 360) %% 360)

# Extract month and day from date_time
newdata <- newdata %>%
  mutate(
    date_time = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M:%S"),
    Month = month(date_time),
    Day = day(date_time)
  )

# Check the updated dataframe
head(newdata)


# Load necessary libraries
library(caret)
library(randomForest)
install.packages("Metrics")
library(Metrics)

# Define features and target variables
features <- c('Month', 'Day', 'U10', 'V10', 'TSK', 'PSFC', 'Q2', 'RAINC', 'RAINNC', 'TSLB', 'SMOIS')
target_speed <- 'Windspeed'
target_direction <- 'WindDirection'

# Split the data into training and testing sets for wind speed
set.seed(42)
train_index_speed <- createDataPartition(newdata[[target_speed]], p = 0.8, list = FALSE)
train_data_speed <- newdata[train_index_speed, ]
test_data_speed <- newdata[-train_index_speed, ]

# Split the data into training and testing sets for wind direction
train_index_direction <- createDataPartition(newdata[[target_direction]], p = 0.8, list = FALSE)
train_data_direction <- newdata[train_index_direction, ]
test_data_direction <- newdata[-train_index_direction, ]

# Initialize and train the Random Forest Regressor for wind speed
rf_regressor_speed <- randomForest(as.formula(paste(target_speed, "~", paste(features, collapse = "+"))),
                                   data = train_data_speed, ntree = 100, random_state = 42)

# Predict and evaluate the model for wind speed
y_pred_speed <- predict(rf_regressor_speed, test_data_speed)
mse_speed <- mse(test_data_speed[[target_speed]], y_pred_speed)

# Initialize and train the Random Forest Regressor for wind direction
rf_regressor_direction <- randomForest(as.formula(paste(target_direction, "~", paste(features, collapse = "+"))),
                                       data = train_data_direction, ntree = 100, random_state = 42)

# Predict and evaluate the model for wind direction
y_pred_direction <- predict(rf_regressor_direction, test_data_direction)
mse_direction <- mse(test_data_direction[[target_direction]], y_pred_direction)

# Print mean squared errors
print(mse_speed)
print(mse_direction)

# Load necessary libraries
library(Metrics)

# Calculate evaluation metrics for wind speed
mae_speed <- mae(test_data_speed[[target_speed]], y_pred_speed)
rmse_speed <- rmse(test_data_speed[[target_speed]], y_pred_speed)
r2_speed <- R2(test_data_speed[[target_speed]], y_pred_speed)

# Calculate evaluation metrics for wind direction
mae_direction <- mae(test_data_direction[[target_direction]], y_pred_direction)
rmse_direction <- rmse(test_data_direction[[target_direction]], y_pred_direction)
r2_direction <- R2(test_data_direction[[target_direction]], y_pred_direction)

# Compile the evaluation results
evaluation_results <- list(
  "Wind Speed" = list(
    "MAE" = mae_speed,
    "MSE" = mse_speed,
    "RMSE" = rmse_speed,
    "R-squared" = r2_speed
  ),
  "Wind Direction" = list(
    "MAE" = mae_direction,
    "MSE" = mse_direction,
    "RMSE" = rmse_direction,
    "R-squared" = r2_direction
  )
)

evaluation_results

# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Create the data frames for actual and predicted values
results_speed <- data.frame(Actual = test_data_speed[[target_speed]], Predicted = y_pred_speed)
results_direction <- data.frame(Actual = test_data_direction[[target_direction]], Predicted = y_pred_direction)

# Actual vs Predicted Wind Speed
p1 <- ggplot(results_speed, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = 'blue') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = 'Random Forest: Actual vs Predicted Wind Speed',
       x = 'Actual Wind Speed', y = 'Predicted Wind Speed') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Residual plot for Wind Speed
residuals_speed <- results_speed$Actual - results_speed$Predicted
p2 <- ggplot(results_speed, aes(x = Predicted, y = residuals_speed)) +
  geom_point(alpha = 0.5, color = 'red') +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = 'Random Forest: Residuals for Wind Speed',
       x = 'Predicted Wind Speed', y = 'Residuals') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Actual vs Predicted Wind Direction
p3 <- ggplot(results_direction, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = 'blue') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = 'Random Forest: Actual vs Predicted Wind Direction',
       x = 'Actual Wind Direction', y = 'Predicted Wind Direction') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Residual plot for Wind Direction
residuals_direction <- results_direction$Actual - results_direction$Predicted
p4 <- ggplot(results_direction, aes(x = Predicted, y = residuals_direction)) +
  geom_point(alpha = 0.5, color = 'red') +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = 'Random Forest: Residuals for Wind Direction',
       x = 'Predicted Wind Direction', y = 'Residuals') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange the plots in a 2x2 grid
grid.arrange(p1, p2, p3, p4, ncol = 2)



#SVR
install.packages("e1071")
library(e1071)
library(caret)

# Selecting the variables for the regression analysis
humidity_pressure_data <- newdata[, c("Q2", "TSLB", "RAINC")]

# Predictor variables: humidity and pressure
X_hum_pres <- humidity_pressure_data[, c("TSLB", "Q2")]

# Response variable: soil moisture (as a weather pattern proxy)
y_weather <- humidity_pressure_data$RAINC

set.seed(42)  # for reproducibility
train_index <- createDataPartition(y_weather, p=0.7, list=FALSE)
X_hum_pres_train <- X_hum_pres[train_index, ]
X_hum_pres_test <- X_hum_pres[-train_index, ]
y_weather_train <- y_weather[train_index]
y_weather_test <- y_weather[-train_index]

svr_model <- svm(x = X_hum_pres_train, y = y_weather_train, type = "eps-regression", kernel = "radial")

# Making predictions
y_svr_pred <- predict(svr_model, X_hum_pres_test)

#Calculate Mean Squared Error
svr_mse <- mean((y_weather_test - y_svr_pred)^2)

# Calculate R-squared - manually or using caret's postResample function
# Manual calculation of R-squared
sse <- sum((y_svr_pred - y_weather_test)^2)
sst <- sum((y_weather_test - mean(y_weather_test))^2)
svr_r2 <- 1 - sse/sst

# Using caret's postResample for both MSE and R2
results <- postResample(pred = y_svr_pred, obs = y_weather_test)
svr_mse <- results[1]  # MSE
svr_r2 <- results[3]  # R-squared

# Print results
print(paste("R² score:", svr_r2))
print(paste("MSE:", svr_mse))

# Assuming 'predictions' is your vector of predictions from the SVR model
# and 'actual_values' is the actual values of the target variable
library(ggplot2)

data <- data.frame(Actual = y_weather_test, Predicted = y_svr_pred)

ggplot(data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', color = 'red', se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
  labs(title = "SVR Predictions vs. Actual Values",
       x = "Actual Values",
       y = "Predicted Values") +
  theme_minimal()
#RESIDUAL PLOT
data$Residuals <- data$Actual - data$Predicted

ggplot(data, aes(x = Predicted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals of SVR Model",
       x = "Predicted Values",
       y = "Residuals") +
  theme_minimal()

#RANDOM FOREST
# Load required libraries
install.packages("randomForest")
library(randomForest)

# Random Forest Regressor for the relationship between multiple weather conditions and soil moisture

# Step 1: Selecting variables for the regression analysis (using multiple weather conditions as predictors)
weather_data <- newdata[c('TSK', 'PSFC', 'U10', 'V10', 'Q2', 'RAINC', 'RAINNC', 'Windspeed', 'SMOIS')]


# Step 2: Preparing data for regression
X_weather <- weather_data[, c('TSK', 'PSFC', 'U10', 'V10', 'Q2', 'RAINC', 'RAINNC', 'Windspeed')]  # All weather conditions as predictors
y_efficiency <- weather_data$SMOIS  # Soil moisture as a proxy for operational efficiency

# Step 3: Splitting the data into training and testing sets
set.seed(42)  # for reproducibility
train_indices <- sample(1:nrow(X_weather), 0.7 * nrow(X_weather))  # 70% train, 30% test
X_weather_train <- X_weather[train_indices, ]
y_efficiency_train <- y_efficiency[train_indices]
X_weather_test <- X_weather[-train_indices, ]
y_efficiency_test <- y_efficiency[-train_indices]

# Step 4: Creating and training the Random Forest model
rf_model <- randomForest(x = X_weather_train, y = y_efficiency_train, ntree = 100, mtry = 4, importance = TRUE)

# Step 5: Making predictions
y_rf_pred <- predict(rf_model, newdata = X_weather_test)

# Step 6: Evaluating the model
rf_r2 <- cor(y_rf_pred, y_efficiency_test)^2  # R-squared
rf_mse <- mean((y_rf_pred - y_efficiency_test)^2)  # Mean Squared Error

# Output performance metrics
rf_r2  # R-squared
rf_mse  # Mean Squared Error

library(ggplot2)

# Assuming 'y_test' contains the actual values and 'y_pred' contains the predicted values from your model
data <- data.frame(Actual = y_efficiency_test, Predicted = y_rf_pred)

# Scatter plot for actual vs. predicted values
ggplot(data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +  # Plots the points with some transparency
  ggtitle("Actual vs. Predicted Values") +
  xlab("Actual Values") +
  ylab("Predicted Values") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 1.5) +  # Diagonal line
  theme_minimal()  # Using a minimal theme for aesthetics

# Display the plot
ggsave("Actual_vs_Predicted.png", width = 10, height = 6, dpi = 300)

# Extract feature importances
importances <- importance(rf_model)

# Sort feature importances
importances <- importances[order(importances, decreasing = TRUE)]

# Plot feature importances
barplot(importances, horiz = TRUE, main = "Feature Importances in Random Forest Model",
        xlab = "Importance", ylab = "Features")
# Calculate residuals
residuals <- y_rf_pred - y_efficiency_test

# Plot residual plot
plot(y_rf_pred, residuals, pch = 19, col = "blue", main = "Residual Plot",
     xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Add horizontal line at y = 0
print(newdata$date_time)

myts <- ts(data=newdata$Heat_Index_K, start=c(2018,5,1), frequency = 8)
myts
View(myts)
decompose(myts)
plot(decompose(myts))

# Load required libraries
library(zoo)
library(tseries)

# Function to perform Dickey-Fuller test and plot rolling statistics
test_stationarity <- function(timeseries) {
  # Determining rolling statistics
  rolmean <- rollmean(timeseries, k = 12, fill = NA)
  rolstd <- rollapply(timeseries, width = 12, FUN = sd, fill = NA)
  
  # Plot rolling statistics
  plot(timeseries, col = "blue", main = "Rolling Mean & Standard Deviation")
  lines(rolmean, col = "red", lwd = 2)
  lines(rolstd, col = "black", lwd = 2)
  legend("topright", legend = c("Original", "Rolling Mean", "Rolling Std"), col = c("blue", "red", "black"), lty = 1, lwd = 2)
  
  # Perform Dickey-Fuller test
  adf_result <- adf.test(timeseries, alternative = "stationary", k = 12)
  print(adf_result)
}

# Testing stationarity of the 'TSK' series
test_stationarity(myts)



# Fit ARIMA model (p=1, d=1, q=1)
model_arima <- arima(myts, order=c(1, 1, 1))

# Fit SARIMA model (p=1, d=1, q=1) (P=1, D=1, Q=1, s=12)
model_sarima <- arima(myts, order=c(1, 1, 1), seasonal=list(order=c(1, 1, 1), period=12))

# Auto ARIMA
library(forecast)
auto_model <- auto.arima(myts, seasonal=TRUE, stepwise=TRUE, approximation=FALSE)

# Summaries
results_arima_summary <- summary(model_arima)
results_sarima_summary <- summary(model_sarima)
auto_arima_summary <- summary(auto_model)

list(results_arima_summary, results_sarima_summary, auto_arima_summary)
# Model diagnostics for the SARIMA model
sarima_residuals <- residuals(model_sarima)

# Plotting residuals to check for no pattern (white noise)
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))  # Set up the layout for subplots
plot(sarima_residuals, main = "Residuals of the SARIMA Model", ylab = "Residuals")
hist(sarima_residuals, breaks = 20, main = "Density of Residuals", xlab = "Residuals")

# Conduct Ljung-Box test on residuals to test for overall randomness
library(forecast)
ljung_box_result <- Box.test(sarima_residuals, lag = 10, type = "Ljung-Box")
ljung_box_result

# Load required libraries if not already loaded
library(forecast)

myts <- ts(data=myts,  start=c(2018,5,1),frequency = 8)
myts


# Load necessary libraries
install.packages("xts")
library(xts)

# Fit auto ARIMA model
auto_arima_model <- auto.arima(myts)

# Generate forecasts
forecast_result <- xts(auto_arima_model, h = 24)# Forecast for 100 periods (adjust as needed)

# Plot the forecast
plot(forecast_result, xlab = "Date", ylab = "Temperature (K)", main = "Surface Temperature Forecast")

# Load required library
library(stats)

# Plotting ACF and PACF for the differenced data
par(mfrow=c(1,2))  # Set up a 1x2 grid for plotting

# ACF plot
acf(myts, lag.max=40, main='Autocorrelation Function (ACF)', ylab='Autocorrelation')

# PACF plot
pacf(myts, lag.max=40, main='Partial Autocorrelation Function (PACF)', ylab='Partial Autocorrelation')

# Fit SARIMA model 
model_sarima <- arima(myts, order=c(1,0, 1), seasonal=list(order=c(1, 0, 1)))

# Forecasting the next 24 periods (72 hours ahead)
forecast_periods <- 24
forecast_result_manual <- forecast(model_sarima , h=forecast_periods)

# Plotting the forecast
plot(forecast_result_manual, main='Heat Index (1,0,1)', xlab='Date', ylab='Temperature (K)')

#arima (4,1,2)
model_sarima <- arima(myts, order=c(2, 0,1), seasonal=list(order=c(2, 0, 1)))

# Forecasting the next 24 periods (72 hours ahead)
forecast_periods <- 24
forecast_result_manual <- forecast(model_sarima , h=forecast_periods)
plot(forecast_result_manual, main='Heat Index (2,0,1)', xlab='Date', ylab='Temperature (K)')


#arima (3,1,2)
model_sarima3 <- arima(myts, order=c(3, 0,1), seasonal=list(order=c(3, 0, 1)))

# Forecasting the next 24 periods (72 hours ahead)
forecast_periods <- 24
forecast_result_manual <- forecast(model_sarima3 , h=forecast_periods)
plot(forecast_result_manual, main='Heat Index (3,0,1)', xlab='Date', ylab='Temperature (K)')

summary(model_sarima3)
# Plotting residuals
residuals <- residuals(model_sarima3)
plot(residuals, main="Residuals of ARIMA(1, 0, 1)", ylab="Residuals")
abline(h=0, col='red')

# Optionally, check the normality of residuals
hist(residuals, main="Histogram of Residuals", xlab="Residuals", breaks=30, col="blue")

# Assuming you have the necessary libraries installed
library(ggplot2)
library(dplyr)

# Calculate correlation coefficients
correlation_temp <- cor(newdata$Q2, newdata$TSK)


# Print the correlation coefficients
print(correlation_temp)
summary(correlation_temp)

# Visualize the trends of temperature and humidity over the months
# Temperature trend
temp_plot <- ggplot(newdata, aes(x = Q2, y = TSK)) +
  geom_line() +
  geom_point() +
  ggtitle('Temperature Trend Over Humidity') +
  xlab('Specific Humidity') +
  ylab('Temperature')


# Arrange the plots side by side
library(gridExtra)
grid.arrange(temp_plot, ncol = 2)

install.packages("rpart")
library(rpart)
install.packages("caret")
# Load necessary libraries
library(caret)
library(rpart)
library(e1071)

# Load the data (assuming newdata is already loaded)
# newdata <- read.csv("path_to_your_file.csv")

# Split the data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(newdata$RAINNC, p = .8, 
                                  list = FALSE, 
                                  times = 1)
trainData <- newdata[ trainIndex,]
testData  <- newdata[-trainIndex,]

# Train a Decision Tree Regressor
dt_model <- rpart(RAINNC ~ RAINC + Q2+PSFC, data = trainData)

# Predict RAINNC on the test set
predictions <- predict(dt_model, testData)

# Evaluate the model
mse <- mean((testData$RAINNC - predictions)^2)
mae <- mean(abs(testData$RAINNC - predictions))
r_squared <- cor(testData$RAINNC, predictions)^2

# Print evaluation metrics
print(paste("MSE:", mse))
print(paste("MAE:", mae))
print(paste("R-squared:", r_squared))

# Plot actual vs predicted values
plot(testData$RAINNC, predictions, 
     main="Actual vs Predicted RAINNC",
     xlab="Actual RAINNC",
     ylab="Predicted RAINNC",
     pch=19, col="blue")
abline(0, 1, col="red")


