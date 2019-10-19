setwd("/Users/anggunberlian/Desktop/CS112")
install.packages("forecast")
install.packages("tidyr")
install.packages("forecast v8.9")
library(forecast)
library(tidyr)
library(reshape2)
install.packages("MLmetrics v1.1.1")

salesforce <- read.csv("Salesforce Dataset.csv", stringsAsFactors = FALSE)
salesforce
salesforce2 <- salesforce[1:(dim(salesforce)[1]-4),]
salesforce2$Profit <- as.numeric(gsub(",", "", salesforce2$Profit))
salesforce$Profit <- as.numeric(gsub(",", "", salesforce$Profit))
profit_history_mid19 <- salesforce2$Profit
profit_history_mid19

# naive method
next_profit_naive <- tail(profit_history_mid19, n = 1)
profit_history_mid19[length(profit_history_mid19)]
next_profit_naive

# RMSE function
RMSE <- function(pred_profit, act_profit) {
  return(sqrt(mean((pred_profit - act_profit)^2)))
}

# RMSE for naive
RMSE(next_profit_naive, salesforce3$Profit[39])


# simple method
profit_history_mid19 <- as.numeric(gsub(",", "", profit_history_mid19))
class(profit_history_mid19)
next_profit_simple <- sum(profit_history_mid19)/length(profit_history_mid19)
next_profit_simple

# RMSE for simple
RMSE(next_profit_simple, (salesforce3$Profit[39]))


# moving average
simple_mov_avg <- function(profit.column) {
  last.value <- sum(tail(profit.column, 4)) / length(profit.column)
  new.column = c(profit.column, last.value)
  return(new.column)
}
simple_mov_avg(profit_history_mid19)

# RMSE for moving average
RMSE(simple_mov_avg(profit_history_mid19)[39], salesforce3$Profit[39])

# weighted moving average
weight <- c(0.1, 0.2, 0.3, 0.4)
weighted_mov_avg <- weighted.mean(tail(profit_history_mid19, 4), weight)

# RMSE for weighted moving average
RMSE(weighted_mov_avg, salesforce3$Profit[39])

# single exponential smoothing
single_ex_smoothing <- ses(profit_history_mid19, h = 1)
class(single_ex_smoothing)
as.numeric(single_ex_smoothing)
single_ex_smoothing$mean

# RMSE for single exponential smoothing
RMSE(single_ex_smoothing$mean, salesforce3$Profit[39])

# Ploting naive forecast, simple forecast, moving average, weighted moving average, and single exponential smoothing
plot(2000 + seq(from = 10, to = 19.25, by = 0.25), salesforce2$Profit / 1000000, type="l", col="blue", lwd = 3,
     ylab = "Profit ($1M)", xlab = "Year", xlim = c(2010, 2019.5), main = "Single Point Forecasting Methods")
lines(2000 + c(19.25, 19.5),
      c(salesforce2$Profit[length(salesforce2$Profit)], next_profit_naive) / 1000000,
      col = "red", lwd = 3)
lines(2000 + c(19.25, 19.5),
      c(salesforce2$Profit[length(salesforce2$Profit)], next_profit_simple) / 1000000,
      col = "green", lwd = 3)
lines(2000 + c(19.25, 19.5),
      c(salesforce2$Profit[length(salesforce2$Profit)], tail(simple_mov_avg(profit_history_mid19), n=1)) / 1000000,
      col = "black", lwd = 3)
lines(2000 + c(19.25, 19.5),
      c(salesforce2$Profit[length(salesforce2$Profit)], weighted_mov_avg) / 1000000,
      col = "pink", lwd = 3)
lines(2000 + c(19.25, 19.5),
      c(salesforce2$Profit[length(salesforce2$Profit)], single_ex_smoothing$mean) / 1000000,
      col = "brown", lwd = 3)

################################# Let's Get A Little More Complicated ############################

# Double Exponential Smoothing
double_ex_smoothing <- holt(salesforce2$Profit, h=4)
double_ex_smoothing$mean
plot(2000 + seq(from = 10, to = 19.25, by = 0.25), salesforce2$Profit / 1000000, xlab ="Year", 
     ylab = "Profit ($1M)", type="l", col="blue", lwd = 3, xlim = c(2010, 2020.25), main = "Double Exponential Smoothing")
lines(2000 + c(19.25, 19.5, 19.75, 20, 20.25), 
      c(salesforce2$Profit[length(salesforce2$Profit)], double_ex_smoothing$mean) / 1000000, col= "red", lwd = 3)

# RMSE for double exponential smoothing
RMSE(double_ex_smoothing$mean, salesforce$Profit[39:42])


# Centered Moving Average
cma_salesforce <- ma(salesforce2$Profit, order=4, centre = TRUE)
plot(2000 + seq(from = 10, to = 19.25, by = 0.25), cma_salesforce / 1000000, xlab ="Year",
     ylab = "Profit ($1M)", col="blue", lwd = 3, xlim = c(2010, 2019.5), main = "Centered Moving Average and Trend Line")
cma_salesforce.valid <- (cma_salesforce / 1000000)[!is.na(cma_salesforce)]
year_seq.valid <- (2000 + seq(from = 10, to = 19.25, by = 0.25))[!is.na(cma_salesforce)]
regr.cma <- lm(cma_salesforce.valid ~ year_seq.valid)
lines(year_seq.valid, regr.cma$fitted, col="red", lwd = 1)


# Triple Exponential Smoothing

salesforce.matrix <- ts(salesforce2$Profit, frequency = 4, start = c(2010, 1))
salesforce.hw <- HoltWinters(salesforce.matrix)
plot(fitted(salesforce.hw) / 1000000, xlab = "Year", main = "Tripple Exponential Smoothing", col = "blue", lw = 2, lty=3)
hw(salesforce.matrix, h=4, seasonal="additive")


# Another way to compute Tripe Exponential Smoothing
autoplot(hw(salesforce.matrix, h=4, seasonal="additive"), xlab= "Year", ylab = "Profit")

# RMSE for Triple Exponential Smoothing
sqrt((salesforce.hw$SSE)/42)

