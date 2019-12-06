library("TTR")
library("forecast")
library("data.table")
library("dplyr")

sales <- read.csv("D:/MonthlySales.csv")
money <- sales$sales

moneyTimeSeries <- ts(money, frequency = 12, start=c(2013), end=c(2016))
moneyTimeSeries
plot.ts(moneyTimeSeries, ylab="Money")

moneytimeseriesSMA3 <- SMA(moneyTimeSeries,n=3)
plot.ts(moneytimeseriesSMA3, ylab="Money")

moneytimeseriesSMA5 <- SMA(moneyTimeSeries,n=5)
plot.ts(moneytimeseriesSMA5, ylab="Money")

moneyTimeSeriescomponents <- decompose(moneyTimeSeries)
plot(moneyTimeSeriescomponents)

acfRes <- acf(moneyTimeSeries, lag = 20) # autocorrelation
pacfRes <- pacf(moneyTimeSeries, lag = 20)  # partial autocorrelation

logmoneytimeseries <- log(moneyTimeSeries)
moneytimeseriesforecasts <- HoltWinters(logmoneytimeseries)
moneytimeseriesforecasts
plot(moneytimeseriesforecasts)

moneytimeseriesforecasts2 <- forecast(moneytimeseriesforecasts, h=24)
autoplot(moneytimeseriesforecasts2)

acf(moneytimeseriesforecasts2$residuals, na.action = na.pass, lag.max = 20)
Box.test(moneytimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")
hist(moneytimeseriesforecasts2$residuals, breaks="FD",main = "Histogram of residuals", xlab="Residuals")

moneyseriesdiff1 <- diff(moneyTimeSeries, differences=1)
plot.ts(moneyseriesdiff1)

moneyseriesdiff2 <- diff(moneyTimeSeries, differences=2)
plot.ts(moneyseriesdiff2)

acfRes <- acf(moneyTimeSeries, plot = FALSE, lag = 20)
print(acfRes)

pacfRes <- pacf(moneyTimeSeries, plot = FALSE, lag = 20)
print(pacfRes)

auto.arima(moneyTimeSeries)
moneyArima <- arima(moneyTimeSeries, order=c(0, 1, 0))

plot(forecast(moneyArima))
acf(moneyArima$residuals, na.action = na.pass, lag.max = 20)
Box.test(moneyArima$residuals, lag=20, type="Ljung-Box")
hist(moneyArima$residuals)