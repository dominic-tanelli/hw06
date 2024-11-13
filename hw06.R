

# chapter 9

# required libraries
# install.packages(c("lubridate", "ggplot2", "forecast", "dplyr"))
library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)

# read ETdat
ETdat <- read.csv("/cloud/project/ETdata.csv")

# unique crops in ETdat
unique(ETdat$crop)

# average fields for each month for almonds
almond <- ETdat %>% # ET data
  filter(crop == "Almonds") %>% # only use almond fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# almond ET time series
almond_ts <- ts(almond$ET.in, # data
                start = c(2016,1), #start year 2016, month 1. first number is 
                # unit of time and second is observations within a unit
                frequency= 12) # frequency of observations in a unit

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

#plot data
plot(almond_y)

# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"), lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"), bty="n")

# AR4 Model Forecast
newAlmond <- forecast(model4)
newAlmond

#make dataframe for plotting
newAlmondF <- data.frame(newAlmond)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+  # Plotting original data
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast), col="red") + 
  # Plotting model forecasts
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95, ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ 
              # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")
