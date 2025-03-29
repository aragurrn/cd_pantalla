###############################################################
# Series temporales. Ejemplo: Empleados
# 
###############################################################

install.packages("tseries")
install.packages("forecast")       # install, if necessary
library(forecast)

#load input
emp_input <- as.data.frame( read.csv("C:/Users/hugoz/OneDrive/Desktop/cd_pantalla/cd_pantalla/st/monthly-number-of-employed-perso.csv",header=T,sep=",",dec=".") )
emp_input_train <- emp_input[1:132,]
emp_input_test <- emp_input[133:159,]


# create a time series object
emp <- ts(as.numeric(as.character(emp_input_train[,2])),start=1978,frequency=12)
emp_test <- ts(as.numeric(as.character(emp_input_test[,2])),start=1989,frequency=12)


#examine the time series
plot(emp, xlab = "Time (months)",
     ylab = "employed persons")


# check for conditions of a stationary time series
plot(emp)
mean(emp)

plot(diff(emp,differences=1))
abline(a=0, b=0)
mean(diff(emp,differences=1))

plot(diff(diff(emp,differences=1),lag=12,differences=1))
abline(a=0, b=0)
mean(diff((diff(emp,lag=12))))

var(diff(diff(emp,differences=1),lag=12,differences=1))
var(diff(diff(emp,differences=1),lag=12,differences=2))
#D=1, d=1


# examine ACF and PACF of differenced series
acf(diff(diff(emp,differences=1),lag=12,differences=1), lag.max=48, main="")
pacf(diff(diff(emp,differences=1),lag=12,differences=1),  lag.max=48, main="")

#Componente estacional
# fit a (0,1,0)x(0,1,1)12 ARIMA model
arima_1 <- arima (emp,order=c(0,1,0),
                  seasonal = list(order=c(0,1,1),period=12))
arima_1
AIC(arima_1,k = log(length(emp)))   #BIC


# examine ACF and PACF of the (0,1,0)x(0,1,1)12 residuals
acf(arima_1$residuals, lag.max=48, main="")
pacf(arima_1$residuals, lag.max=48, main="")

# fit a (3,1,3)x(0,1,1)12 ARIMA model
arima_2 <- arima (emp,order=c(3,1,3),
                seasonal = list(order=c(0,1,1),period=12))
arima_2

# it may be necessary to calculate AICc and BIC 
# http://stats.stackexchange.com/questions/76761/extract-bic-and-aicc-from-arima-object
AIC(arima_2,k = log(length(emp)))   #BIC


# examine ACF and PACF of the (3,1,3)x(0,1,1)12 residuals
acf(arima_2$residuals, lag.max=48, main="")
pacf(arima_2$residuals, lag.max=48, main="")

# Compare with autoarima
arima_3=auto.arima(emp,d=1,D=1,max.order=4,trace=TRUE,approx=TRUE,allowdrift=FALSE,stepwise=FALSE)
arima_3
arima_2
acf(arima_3$residuals, lag.max=48, main="")
pacf(arima_3$residuals, lag.max=48, main="")


# Normality and Constant Variance
plot(arima_3$residuals, ylab = "Residuals")
abline(a=0, b=0)

hist(arima_3$residuals, xlab="Residuals", xlim=c(-80,80))

qqnorm(arima_3$residuals, main="")
qqline(arima_3$residuals)


# Forecasting
#predict the next  month
arima_3.predict <- predict(arima_3,n.ahead=30)

plot(emp, xlim=c(1978,1991),
     xlab = "Time (years)",
     ylab = "employees",
     ylim=c(6000,9000))
lines(arima_3.predict$pred,col=2)
lines(arima_3.predict$pred+1.96*arima_3.predict$se, col=3, lty=2)
lines(arima_3.predict$pred-1.96*arima_3.predict$se, col=3, lty=2)
lines(emp_test,col=4)


