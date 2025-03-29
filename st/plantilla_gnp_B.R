###############################################################
#Building and Evaluating an ARIMA Model
###############################################################

#install.packages("forecast")       # install, if necessary
library(forecast)


#load input
gnp_input <- as.data.frame(read.csv("C:/Users/hugoz/OneDrive/Desktop/cd_pantalla/cd_pantalla/st/annual-gnp-deflator-us-1889-to-1.csv")) 

# create a time series object
gnp <- ts(as.numeric(as.character(gnp_input[1:82,2])),start=1889)

#examine the time series
plot(gnp, xlab = "Time (years)",
     ylab = "gnp_deflator")


# check for conditions of a stationary time series
plot(gnp)
mean(gnp)

plot(diff(gnp,differences=1))
abline(a=0, b=0)
mean(diff(gnp,differences=1))

plot(diff(gnp,differences=2))
abline(a=0, b=0)
mean(diff(gnp,differences=2))

var(diff(gnp,differences=1))
var(diff(gnp,differences=2))
# d=2


# examine ACF and PACF of differenced series
acf(diff(gnp,differences=2), lag.max=40, main="")
pacf(diff(gnp,differences=2), lag.max=40, main="")

# fit a (0,2,1)
arima_1 <- arima (gnp,order=c(0,2,1))
arima_1

# BIC 
# (para AICc: http://stats.stackexchange.com/questions/76761/extract-bic-and-aicc-from-arima-object)
AIC(arima_1,k = log(length(gnp)))   #BIC


# examine ACF and PACF of the (0,2,1) residuals
acf(arima_1$residuals, lag.max=40, main="")
pacf(arima_1$residuals, lag.max=40, main="")

# Comparar con (1,2,1)
arima_2 <- arima (gnp,order=c(1,2,1))
arima_2
AIC(arima_2,k = log(length(gnp)))   #BIC

# examine ACF and PACF 
acf(arima_2$residuals, lag.max=60, main="")
pacf(arima_2$residuals, lag.max=60, main="")

# Compare with autoarima
arima_3 <- auto.arima(gnp)
arima_3

# Normality and Constant Variance
plot(arima_2$residuals, ylab = "Residuals")
abline(a=0, b=0)

hist(arima_2$residuals, xlab="Residuals", xlim=c(-15,15))

qqnorm(arima_2$residuals, main="")
qqline(arima_2$residuals)

# Forecasting

plot(gnp)
#predict the next 10 years
arima_2.predict <- predict(arima_2,n.ahead=10)
plot(gnp, xlim=c(1890,1980),
     xlab = "Time (years)",
     ylab = "GNPdeflation",
     ylim=c(0,180))
lines(arima_2.predict$pred,col=2)
lines(arima_2.predict$pred+1.96*arima_2.predict$se, col=3, lty=2)
lines(arima_2.predict$pred-1.96*arima_2.predict$se, col=3, lty=2)
