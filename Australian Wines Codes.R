
#Importing the Dataset
dataset <- read.csv("C:/Users/dpshj/Downloads/dataset.csv")
View(dataset)
head(dataset)

#Selecting the Dataset Columns
dataset_1 = select(dataset,-2,-3,-4,-5,-6)
View(dataset_1)
head(dataset_1)

#Partitioning
#Splitting the Dataset into Training Set
#install.packages('dplyr')
library(dplyr)
train = slice(dataset_1,1:168)
View(train)
head(train)
train_1 = select(train, -1)
View(train_1)
head(train_1)
ts_object = ts(train_1,start = 1980, frequency = 12, end = 1993)
plot(ts_object, xlab = 'Time',ylab = 'Wine Sales', main = 'Time Series Plot for Dry White')

#Decomposing the Australian Wines Sales Dataset
plot(decompose(ts_object))

#Fitting regression models to sales with a linear trend and seasonality.
#install.packages('forecast')
library(forecast)
#par(mfrow = c(1,2))
lm.trend = tslm(ts_object~trend)
lm.trend
summary(lm.trend)
plot(forecast(lm.trend,h=2), xlab = 'Time',ylab = 'Wine Sales', main = 'Linear Model Trend Forecast for Dry White')
lm.season = tslm(ts_object ~ season)
lm.season
summary(lm.season)
plot(forecast(lm.season,h=2), xlab = 'Time',ylab = 'Wine Sales', main = 'Linear Model Seasonal Forecast for Dry White')
lm.trend.season = tslm(ts_object~trend + season)
lm.trend.season
summary(lm.trend.season)
plot(forecast(lm.trend.season,h=2), xlab = 'Time',ylab = 'Wine Sales', main = 'Linear Model Forecast for Dry White')

#Actual vs Trend Forecast
par(mfrow = c(1,2))
plot(ts_object, xlab = 'Time',ylab = 'Wine Sales', main = 'Time Series Plot for Dry White')
plot(forecast(lm.trend,h=2), xlab = 'Time',ylab = 'Wine Sales', main = 'Linear Model Trend Forecast for Dry White')
accuracy(forecast(lm.trend,h=2))

#Actual vs Seasonal Forecast
par(mfrow = c(1,2))
plot(ts_object, xlab = 'Time',ylab = 'Wine Sales', main = 'Time Series Plot for Dry White')
plot(forecast(lm.season,h=2), xlab = 'Time',ylab = 'Wine Sales', main = 'Linear Model Seasonal Forecast for Dry White')
accuracy(forecast(lm.season,h=2))

#Actual vs Forecast
par(mfrow = c(1,2))
plot(ts_object, xlab = 'Time',ylab = 'Wine Sales', main = 'Time Series Plot for Dry White')
plot(forecast(lm.trend.season,h=2), xlab = 'Time',ylab = 'Wine Sales', main = 'Linear Model Forecast for Dry White')
accuracy(forecast(lm.trend.season,h=2))

#Regression Model to Forecast Sales in January and February 1994     
fit = tslm(ts_object~trend)
fit
summary(fit)
forecast(fit,h=2)
par(mfrow=c(1,2))
plot(ts_object, xlab = 'Time',ylab = 'Wine Sales', main = 'Time Series Plot for Dry White')
plot(forecast(fit,h=2), xlab = 'Time', ylab = 'Wine Sales',main = 'Forecast for Sales in January and February 1994') 

# Create an ACF and PACF Plots for the Residuals from the above Regression Model until lag 1-2
residual=fit$residuals
par(mfrow=c(1,2))
Acf(residual, xlab = 'Time', ylab = 'Wine Sales',main = 'ACF Plot for the Residuals of the LRM')
Pacf(residual, xlab = 'Time', ylab = 'Wine Sales',main = 'PACF Plot for the Residuals of the LRM')

#ARIMA (p,d,q) and plotting the residuals, ACF and PACF.
fit_2=auto.arima(ts_object)
fit_2
summary(fit_2)
plot(forecast(fit_2,h=2), xlab = 'Time', ylab = 'Wine Sales',main = 'Forecasts for ARIMA(0,0,1)(0,1,1)[12] with Drift')
Acf(plot(forecast(fit_2,h=2), xlab = 'Time', ylab = 'Wine Sales',main = 'ACF Plot for ARIMA'))
Pacf(plot(forecast(fit_2,h=2), xlab = 'Time', ylab = 'Wine Sales',main = 'PACF Plot for ARIMA'))