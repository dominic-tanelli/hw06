# Dominic Tanelli
# Prof. Kropp
# ENVST 325
# 26 September 2024

# Homework 6: Modeling and analyzing data

# In-class prompts
# Prompt 1
# install.packages(c("lubridate", "ggplot2", "forecast", "dplyr", "tidyverse"))
# install.packages(c("broom", "zoo"))
library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)
library(tidyverse)
library(broom)
library(zoo)

# read ETdat
ETdat <- read.csv("/cloud/project/ETdata.csv")

# unique crops in ETdat
unique(ETdat$crop)

# average fields for each month for almonds
almond <- ETdat %>% # ET data
  filter(crop == "Almonds") %>% # only use almond fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE)) # average fields

# visualize the data
ggplot(almond, aes(x = ymd(date), y = ET.in)) + geom_point() + geom_line() + labs(
  x = "year", y = "Monthy evapotranspiration (in)")

# almond ET time series
almond_ts <- ts(almond$ET.in, # data
                start = c(2016,1), #start year 2016, month 1. first number is 
                # unit of time and second is observations within a unit
                frequency = 12) # frequency of observations in a unit

# decompose almond ET time series
almond_dec <- decompose(almond_ts)

# plot decomposition
plot(almond_dec)

# autocorrection
acf(na.omit(almond_ts), # remove missing data
    lag.max = 24) # look at 2 years (24 months)

# partial ACF can better represent potential lag periods
pacf.plot <- pacf(na.omit(almond_ts))

# omit the NAs to run the model in almond_ts before running the model
almond_y <- na.omit(almond_ts)
model1 <- arima(almond_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers 
                # get a 0 to keep AR format
model1

# 4th order AR
model4 <- arima(almond_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers 
                # get a 0 to keep AR format
model4

# calculate fit
AR_fit1 <- almond_y - residuals(model1) 
AR_fit4 <- almond_y - residuals(model4)

# plot data
plot(almond_y)

# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"), lty = c(1,2,2), lwd=c(1,2,2), col = 
         c("black", "tomato3","darkgoldenrod4"), bty = "n")

# AR4 Model Forecast
newAlmond <- forecast(model4)
newAlmond

# make dataframe for plotting
newAlmondF <- data.frame(newAlmond)

# set up dates
years <- c(rep(2021,4), rep(2022,12), rep(2023,8))
month <- c(seq(9,12), seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+  # Plotting original data
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast), col="red") + 
  # Plotting model forecasts
  geom_ribbon(data = newAlmondF, aes(x = dateF, ymin = Lo.95, ymax = Hi.95), 
              fill = rgb(0.5,0.5,0.5,0.5)) + theme_classic() + 
  labs(x = "year", y = "Evapotranspiration (in)") # uncertainty interval

# Homework
# Question 1
# Read in greenhouse gas data from reservoirs
ghg <- read.csv("/cloud/project/Deemer_GHG_Data.csv")

# Transform CO2 data
ghg <- ghg %>%
  mutate(transformed_CO2 = 1 / (co2 + 1000))

# Fit the OLS regression model
# Predictors include depth, surface area, volume, residence time, chlorophyll-a, 
# air temperature, runoff, precipitation, and age
model <- lm(transformed_CO2 ~ mean.depth + surface.area + volume + 
              Residence.Time..days. + chlorophyll.a + airTemp + runoff + 
              precipitation + age, data = ghg)

# Summary of the model
summary(model)

# Extracts information for the table
model_results <- tidy(model)
model_stats <- glance(model) # This provides R-squared and other statistics

# Formats the output
output_table <- model_results %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  bind_rows(tibble(term = "R-squared", estimate = model_stats$r.squared, 
                   std.error = NA, statistic = NA, p.value = NA), 
            tibble(term = "Sample Size", estimate = nrow(ghg), std.error = NA, 
                   statistic = NA, p.value = NA))
output_table

# Question 2
# Gets unique crop names
crops <- unique(ETdat$crop)

# Loops through each crop to filter, average, and decompose
for (crop in crops) {
  # Filters data for the current crop and calculate monthly ET average
  crop_df <- ETdat %>%
    filter(crop == crop) %>%
    group_by(date) %>%
    summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))
  
  # Converts date column to time series (assuming monthly data starting in 2016)
  crop_ts <- ts(crop_df$ET.in, start = c(2016, 1), frequency = 12)
  
  # Decomposes the time series
  crop_dec <- decompose(crop_ts)
  
  # Plots decomposition for each crop
  plot(crop_dec)
  title(main = paste("ET Decomposition for", crop))  # Add title separately
}

# Question 3
# This function processes/fits AR model and forecast for a given crop with the variable crop_name
forecast_crop <- function(crop_name) {
  # Filters and prepare time series data
  crop_df <- ETdat %>%
    filter(crop == crop_name) %>%
    mutate(date = ymd(date)) %>%
    group_by(date) %>%
    summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))
  
  # Exchanges missing values with na.approx and ensure length consistency
  crop_df$ET.in <- na.approx(crop_df$ET.in, na.rm = FALSE)
  
  # Checks if there are still NAs and remove them
  crop_df <- crop_df %>% filter(!is.na(ET.in))
  
  # Creates time series starting from 2016 with monthly data
  crop_ts <- ts(crop_df$ET.in, start = c(2016, 1), frequency = 12)
  
  # Fits an AR(4) model for the time series
  model <- arima(crop_ts, order = c(4, 0, 0))
  
  # Forecasts future values (next 24 months)
  crop_forecast <- forecast(model, h = 24)
  
  # Converts forecast to a data frame for plotting
  forecast_df <- data.frame(
    date = seq(ymd("2021-09-01"), by = "1 month", length.out = 24),
    ET.in = crop_forecast$mean,
    Lo.95 = crop_forecast$lower[, 2],
    Hi.95 = crop_forecast$upper[, 2]
  )
  
  # Plots historical and forecasted ET
  ggplot() +
    geom_line(data = crop_df, aes(x = date, y = ET.in), color = "blue") +
    geom_line(data = forecast_df, aes(x = date, y = ET.in), color = "red") +
    geom_ribbon(data = forecast_df, aes(x = date, ymin = Lo.95, ymax = Hi.95), 
                fill = "grey", alpha = 0.3) +
    labs(title = paste("Historical and Forecasted ET for", crop_name),
         x = "Year", y = "Evapotranspiration (in)") +
    theme_minimal()
}

# Runs forecasting function for pistachios and fallow/idle fields
forecast_crop("Pistachios")
forecast_crop("Fallow/Idle Cropland")
