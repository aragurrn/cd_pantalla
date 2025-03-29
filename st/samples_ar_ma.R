###############################################################
# Series temporales. series AR y MA "puras"
# 
###############################################################

install.packages("tseries")
install.packages("forecast")       # install, if necessary

library(forecast)

input.ar1<-numeric(1000)
input.ar1[1]<- rnorm(1)
for (i in 2:1000) input.ar1[i]<-0.8*input.ar1[i-1]+0.2*rnorm(1)

input.ar3<-numeric(1000)
input.ar3[1:3]<- c(58,54,56)
for (i in 4:1000) input.ar3[i]<-0.5*input.ar3[i-1]+0.3*input.ar3[i-2]+0.2*input.ar3[i-3]+rnorm(1)

input.rnd<-rnorm(1003)

input.ma3<-numeric(1000)
for (i in 1:1000) input.ma3[i]<- 10*input.rnd[i+1]+10*input.rnd[i+2]+10*input.rnd[i+3]+input.rnd[i+3]


# create a time series object
ar1 <- ts(input.ar1)
ar3 <- ts(input.ar3)
ma3 <- ts(input.ma3)


#examine the time series
plot(ar1)
mean(ar1)
abline(a=mean(ar1), b=0)

# check for conditions of a stationary time series
plot(diff(ar1,differences=1))
abline(a=0, b=0)
# d=0

# examine ACF and PACF 
acf(ar1, xaxp = c(0, 40, 5), lag.max=40, main="")
pacf(ar1, xaxp = c(0, 40, 5), lag.max=40, main="")
# p=1

#idem ar3
plot(ar3)
abline(a=mean(ar3), b=0)

plot(diff(ar3,1))
mean(diff(ar3,1))
abline(a=0, b=0)
# d=1

acf(ar3, xaxp = c(0, 40, 5), lag.max=40, main="")
pacf(ar3, xaxp = c(0, 40, 5), lag.max=40, main="")
# p=3


#idem ma3
plot(ma3)
abline(a=mean(ma3), b=0)
plot(diff(ma3,1))
abline(a=0, b=0)
# d=0


acf(ma3, xaxp = c(0, 40, 5), lag.max=40, main="")
pacf(ma3, xaxp = c(0, 40, 5), lag.max=40, main="")
# q=2


# fit a (1,0,0) ARIMA model for ar1
arima_1 <- arima (ar1,order=c(1,0,0))
arima_1

# it may be necessary to calculate AICc and BIC 
# http://stats.stackexchange.com/questions/76761/extract-bic-and-aicc-from-arima-object
AIC(arima_1,k = log(length(ar1)))   #BIC


# examine ACF and PACF of residuals

acf(arima_1$residuals, xaxp = c(0, 40, 5), lag.max=40, main="")
pacf(arima_1$residuals, xaxp = c(0, 40, 5), lag.max=40, main="")

# Normality and Constant Variance
plot(arima_1$residuals, ylab = "Residuals")
abline(a=0, b=0)

hist(arima_1$residuals, xlab="Residuals", xlim=c(-1,1))

qqnorm(arima_1$residuals, main="")
qqline(arima_1$residuals)



# fit a (3,1,0) ARIMA model for ar3
arima_2 <- arima (ar3,order=c(3,1,0))
arima_2

# it may be necessary to calculate AICc and BIC 
# http://stats.stackexchange.com/questions/76761/extract-bic-and-aicc-from-arima-object
AIC(arima_2,k = log(length(ar3)))   #BIC


# examine ACF and PACF

acf(arima_2$residuals, xaxp = c(0, 40, 5), lag.max=40, main="")
pacf(arima_2$residuals, xaxp = c(0, 40, 5), lag.max=40, main="")


plot(arima_2$residuals, ylab = "Residuals")
abline(a=0, b=0)

hist(arima_2$residuals, xlab="Residuals", xlim=c(-4,4))

qqnorm(arima_2$residuals, main="")
qqline(arima_2$residuals)


# fit a (0,0,2) ARIMA model for ma3
arima_3 <- arima (ma3,order=c(0,0,2))
arima_3

# it may be necessary to calculate AICc and BIC 
# http://stats.stackexchange.com/questions/76761/extract-bic-and-aicc-from-arima-object
AIC(arima_3,k = log(length(ma3)))   #BIC


# examine ACF and PACF

acf(arima_3$residuals, xaxp = c(0, 40, 5), lag.max=40, main="")
pacf(arima_3$residuals, xaxp = c(0, 40, 5), lag.max=40, main="")


# Normality and Constant Variance
plot(arima_3$residuals, ylab = "Residuals")
abline(a=0, b=0)

hist(arima_3$residuals, xlab="Residuals", xlim=c(-40,40))

qqnorm(arima_3$residuals, main="")
qqline(arima_3$residuals)




