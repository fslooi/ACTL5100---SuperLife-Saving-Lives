# ARIMA Modelling for economy data

# Inputs
setwd("/Users/fslooi/Documents/UNSW/Term 1 2024/ACTL5100/SOA Case Study/Dataset")
input_max_p <- 10
input_max_d <- 5
input_max_q <- 10
input_sim_year <- 30
simulation_set_seed <- 100
input_forecast_year <- 100

##############################################################################################################################################################################
#Do not edit codes beyond this line

# Import libraries #
## Install packages if necessary ##
#install.packages("forecast")
#install.packages("openxlsx")
library("readxl")
library("forecast")
library("openxlsx")

# Import Datset
eco_data <- read_excel("srcsc-2024-lumaria-economic-data.xlsx", skip = 11)

# Setting variabels
inflation <- eco_data$Inflation
year <- eco_data$Year
overnight <- eco_data$`Government of Lumaria Overnight Rate`
first_year_rf <- eco_data$`1-yr Risk Free Annual Spot Rate`
ten_year_rf <- eco_data$`10-yr Risk Free Annual Spot Rate`

# Check missing values
anyNA(inflation)
anyNA(year)
anyNA(overnight)
anyNA(first_year_rf)
anyNA(ten_year_rf)

# Check data format
class(inflation)
class(year)
class(overnight)
class(first_year_rf)
class(ten_year_rf)

# Plotting
plot(inflation ~ year, type = "l", xlab = 'Year', ylab = 'Inflation', main = 'Inflation vs Year', lwd = 2, col = 'maroon')
plot(overnight ~ year, type = "l", xlab = 'Year', ylab = 'Government of Lumaria Overnight Rate', 
     main = 'Government of Lumaria Overnight Rate vs Year', lwd = 2, col = 'forestgreen')
plot(first_year_rf ~ year, type = "l", xlab = 'Year', ylab = 'Risk Free Annual Spot Rate', 
     main = 'Risk Free Annual Spot Rate vs Year', lwd = 2, col = 'firebrick', ylim = c(min(first_year_rf, ten_year_rf), max(first_year_rf, ten_year_rf)))
lines(ten_year_rf ~ year, lwd = 2, col = 'dodgerblue4')
legend("topleft", legend = c("1-year", "10-year"), col = c("firebrick", "dodgerblue4"), lty = 1, lwd = 2)

# Summary statistics
summary(inflation)
summary(overnight)
summary(first_year_rf)
summary(ten_year_rf)

# Fit ARIMA models
max_p <- input_max_p
max_d <- input_max_d
max_q <- input_max_q
inflation_model <- auto.arima(inflation, max.p = max_p, max.d = max_d, max.q = max_q, max.order = 20, stationary = TRUE, seasonal = FALSE)
overnight_model <- auto.arima(overnight, max.p = max_p, max.d = max_d, max.q = max_q, max.order = 20, stationary = TRUE, seasonal = FALSE)
first_year_rf_model <- auto.arima(first_year_rf, max.p = max_p, max.d = max_d, max.q = max_q, max.order = 20, stationary = TRUE, seasonal = FALSE)
ten_year_rf_model <- auto.arima(ten_year_rf, max.p = max_p, max.d = max_d, max.q = max_q, max.order = 20, stationary = TRUE, seasonal = FALSE)

# Accuracy and Residual Checks
accuracy(inflation_model)
residuals(inflation_model)
checkresiduals(inflation_model)

accuracy(overnight_model)
residuals(overnight_model)
checkresiduals(overnight_model)

accuracy(first_year_rf_model)
residuals(first_year_rf_model)
checkresiduals(first_year_rf_model)

accuracy(ten_year_rf_model)
residuals(ten_year_rf_model)
checkresiduals(ten_year_rf_model)

##############################################################################################################################################################################
# Simulate data
num_year_sim <- input_sim_year
new_year <- seq(year[length(year)] + 1, year[length(year)] + num_year_sim, 1)
all_year <- c(year, new_year)

set.seed(simulation_set_seed)
inflation_sim <- simulate(inflation_model, nsim = num_year_sim)
overnight_sim <- simulate(overnight_model, nsim = num_year_sim)
first_year_rf_sim_data <- simulate(first_year_rf_model, nsim = num_year_sim)
ten_year_rf_sim_data <- simulate(ten_year_rf_model, nsim = num_year_sim)

# Merge data
all_inflation <- c(inflation, inflation_sim)
all_overnight <- c(overnight, overnight_sim)
all_first_year_rf <- c(first_year_rf, first_year_rf_sim_data)
all_ten_year_rf <- c(ten_year_rf, ten_year_rf_sim_data)

# Plot simulated data
plot(all_inflation[1:length(inflation)] ~ all_year[1:length(year)], xlim = c(min(all_year), max(all_year)), ylim = c(min(all_inflation), max(all_inflation)), 
     type = "l", xlab = 'Year', ylab = 'Inflation', main = 'Simulated Inflation vs Year', lwd = 2, col = 'maroon')
lines(all_inflation[length(inflation):length(all_inflation)] ~ all_year[length(year):length(all_year)], lty = 2, lwd = 2)
plot(all_overnight[1:length(inflation)] ~ all_year[1:length(year)], xlim = c(min(all_year), max(all_year)), ylim = c(min(all_overnight), max(all_overnight)), 
     type = "l", xlab = 'Year', ylab = 'Government of Lumaria Overnight Rate', main = 'Simulated Government of Lumaria Overnight Rate vs Year', lwd = 2, col = 'forestgreen')
lines(all_overnight[length(overnight):length(all_overnight)] ~ all_year[length(year):length(all_year)], lty = 2, lwd = 2)
plot(all_first_year_rf[1:length(first_year_rf)] ~ all_year[1:length(year)], xlim = c(min(all_year),max(all_year)), ylim = c(min(all_first_year_rf), max(all_first_year_rf)), 
     type = "l", xlab = 'Year', ylab = '1-yr Risk Free Annual Spot Rate', main = 'Simulated 1-yr Risk Free Annual Spot Rate vs Year', lwd = 2, col = 'firebrick')
lines(all_first_year_rf[length(first_year_rf):length(all_first_year_rf)] ~ all_year[length(year):length(all_year)], lty = 2, lwd = 2)
plot(all_ten_year_rf[1:length(ten_year_rf)] ~ all_year[1:length(year)], xlim = c(min(all_year),max(all_year)), ylim = c(min(all_ten_year_rf), max(all_ten_year_rf)), 
     type = "l", xlab = 'Year', ylab = '10-yr Risk Free Annual Spot Rate', main = 'Simulated 10-yr Risk Free Annual Spot Rate vs Year', lwd = 2, col = 'dodgerblue4')
lines(all_ten_year_rf[length(ten_year_rf):length(all_ten_year_rf)] ~ all_year[length(year):length(all_year)], lty = 2, lwd = 2)

##############################################################################################################################################################################
# Forecast data
forecast_year <- input_forecast_year   #Set the number of points to forecast
forecast_new_year <- seq(year[length(year)] + 1, year[length(year)] + forecast_year, 1)
forecast_all_year <- c(year, forecast_new_year)
year_seq <- seq(1, length(forecast_all_year), 20)
year_lab <- rep(0, length(year_seq))
for (i in 1:length(year_seq)){
  val = year_seq[i]
  year_lab[i] <- forecast_all_year[val]
}

#Forecast using the best model identified
forecast_inflation_model <- forecast(inflation_model, h = forecast_year)
forecast_overnight_model <- forecast(overnight_model, h = forecast_year)
forecast_first_year_rf_model <- forecast(first_year_rf_model, h = forecast_year)
forecast_ten_year_rf_model <- forecast(ten_year_rf_model, h = forecast_year)

#Plot forecast
plot(forecast_inflation_model, main = 'Forecast Inflation vs Year', ylab = 'Inflation', xaxt = "n")
axis(1, at = year_seq, labels = year_lab)  # Adjust 'at' and 'labels' according to your data
xlabel <- 'Year'
mtext(side = 1, line = 3, xlabel)

plot(forecast_overnight_model, main = 'Forecast Government of Lumaria Overnight Rate vs Year', ylab = 'Government of Lumaria Overnight Rate', xaxt = "n")
axis(1, at = year_seq, labels = year_lab)  # Adjust 'at' and 'labels' according to your data
xlabel <- 'Year'
mtext(side = 1, line = 3, xlabel)

plot(forecast_first_year_rf_model, main = 'Forecast 1-yr Risk Free Annual Spot Rate vs Year', ylab = '1-yr Risk Free Annual Spot Rate', xaxt = "n")
axis(1, at = year_seq, labels = year_lab)  # Adjust 'at' and 'labels' according to your data
xlabel <- 'Year'
mtext(side = 1, line = 3, xlabel)

plot(forecast_ten_year_rf_model, main = 'Forecast 10-yr Risk Free Annual Spot Rate vs Year', ylab = '10-yr Risk Free Annual Spot Rate', xaxt = "n")
axis(1, at = year_seq, labels = year_lab)  # Adjust 'at' and 'labels' according to your data
xlabel <- 'Year'
mtext(side = 1, line = 3, xlabel)

##############################################################################################################################################################################
##############################################################################################################################################################################
# Export Excel file
forecast_new_year
df_inflation <- data.frame(forecast_new_year, forecast_inflation_model$mean, forecast_inflation_model$lower[, 2], forecast_inflation_model$upper[, 2])
names(df_inflation) <- c("Forecast Year", "Inflation Mean", "Inflation Lower 95%", "Inflation Upper 95%")
df_overnight <- data.frame(forecast_new_year, forecast_overnight_model$mean, forecast_overnight_model$lower[, 2], forecast_overnight_model$upper[, 2])
names(df_overnight) <- c("Forecast Year", "Overnight Mean", "Overnight Lower 95%", "Overnight Upper 95%")
df_first_year_rf <- data.frame(forecast_new_year, forecast_first_year_rf_model$mean, forecast_first_year_rf_model$lower[, 2], forecast_first_year_rf_model$upper[, 2])
names(df_first_year_rf) <- c("Forecast_Year", "1-yr rf Mean", "1-yr rf Lower 95%", "1-yr rf Upper 95%")
df_ten_year_rf <- data.frame(forecast_new_year, forecast_ten_year_rf_model$mean, forecast_ten_year_rf_model$lower[, 2], forecast_ten_year_rf_model$upper[, 2])
names(df_ten_year_rf) <- c("Forecast_Year", "10-yr rf Mean", "10-yr rf Lower 95%", "10-yr rf Upper 95%")

wb <- createWorkbook()
addWorksheet(wb, "Inflation")
writeData(wb, sheet = "Inflation", x = df_inflation)
addWorksheet(wb, "Overnight")
writeData(wb, sheet = "Overnight", x = df_overnight)
addWorksheet(wb, "1-yr rf")
writeData(wb, sheet = "1-yr rf", x = df_first_year_rf)
addWorksheet(wb, "10-yr rf")
writeData(wb, sheet = "10-yr rf", x = df_ten_year_rf)
saveWorkbook(wb, "Forecast Economic Data.xlsx", overwrite = TRUE)

##############################################################################################################################################################################