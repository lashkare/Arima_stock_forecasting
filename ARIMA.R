rm(list=ls())
cat("\14")

library(quantmod)
library(xts)
library(tseries)
library(forecast)
library(timeSeries)


stock.symbols <- c("NwN") 

stock.symbols.returned <- getSymbols(stock.symbols, from = "2007-01-01", to = "2018-01-06")

stock.data <- get(stock.symbols.returned[1])

chartSeries(stock.data, theme = "white", name = stock.symbols[1])

stock.data.monthly <- to.monthly(stock.data)
adj <- Ad(stock.data.monthly)

freq <- 12
adj.ts <- ts(adj, frequency = freq)

whole.periods <- floor(nrow(adj.ts) / freq)
partial.periods <- nrow(adj.ts) %% freq

desired.test <- 3
training.end.row <- whole.periods + 1
training.end.col <- ifelse(partial.periods == 0, freq - desired.test, freq - partial.periods - desired.test)
if(partial.periods < desired.test){
  training.end.row <- whole.periods
  training.end.col <- freq - (desired.test - partial.periods)
}
training.ts <- window(adj.ts, c(1,1), c(training.end.row,training.end.col))
testing.ts <- window(adj.ts, c(training.end.row, training.end.col + 1))

fit.stl <- stl(training.ts[,1], s.window = "period")

plot(fit.stl, main="STL Decomposition")

forecasted.adj <- stlf(training.ts[,1], s.window = "period", method="arima", h=desired.test)
plot(forecasted.adj, main="Forecasts of NWN from STL and ARIMA (w/o month data)")

accuracy(forecasted.adj, testing.ts)

# Plot the residuals
res <- residuals(forecasted.adj)
plot(res, main="Residuals W/O Month Data and no Tuning")

# Check the correlation of the residuals
Acf(res, main="Autocorrelation W/O Month Data and no Tuning")

mean(res)

forecasted.adj <- stlf(training.ts[,1], s.window = "period", method="arima", h=desired.test, lambda = 1.9, robust = TRUE, biasadj = TRUE)
plot(forecasted.adj, main="Forecasts of NWN from tuned STL and ARIMA (w/o month data)")

accuracy(forecasted.adj, testing.ts)

# Plot the residuals
res <- residuals(forecasted.adj)

# Check the correlation of the residuals
Acf(res, main="Autocorrelation W/O Month Data and with Tuning")

mean(res)

dates <- index(adj)
months <- format(dates, "%b")

xreg.months <- model.matrix(~as.factor(months))[, 2:12]
colnames(xreg.months) <- gsub("as\\.factor\\(months\\)", "", colnames(xreg.months))

training.xtra <- xreg.months[1:nrow(training.ts),]
testing.xtra <- xreg.months[nrow(training.ts) + 1:nrow(testing.ts),]


forecasted.adj <- stlf(training.ts[,1], s.window = "period", method="arima", h=nrow(testing.xtra), xreg = training.xtra, newxreg = testing.xtra, lambda = 1.5)
plot(forecasted.adj, main="Forecasts of NWN from STL and ARIMA (with month data)")

accuracy(forecasted.adj, testing.ts)

# Plot the residuals
res <- residuals(forecasted.adj)

# Check the correlation of the residuals
Acf(res, main="Autocorrelation with Month Data")

mean(res)

fore <- arima(training.ts[,1], order = c(1,2,1))
forecasted.adj1 <- forecast(fore, f= 12)
plot(forecasted.adj1, include = 180)

