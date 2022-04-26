library(tseries)
library(ggplot2)
library(forecast)
setwd('D:/時間序列')

# daily
data2 <- read.csv("PM2.5 日區間.csv");
PM2.5_day <- ts(data2[,2], frequency = 1, start = c(2010,01));
PM2.5_day2 <- ts(data2[,2], frequency = 365, start = c(2010,01));
PM2.5_day_2014 <- ts(data2[1462:1826,2], frequency = 365, start = c(2014,01));
plot.ts(PM2.5_day2)
# ACF & PACF
acf(PM2.5_day, lag=12)
pacf(PM2.5_day, lag=12)
summary(PM2.5_day)

gg_acf <- acf(PM2.5_day, plot = FALSE);
gg_acf_df <- with(gg_acf, data.frame(lag, acf));
ggplot(gg_acf_df, aes(x = lag, y = acf)) +
  geom_segment(aes(xend = lag, yend = 0), color = 'blue', size = 2, alpha=I(1/2)) +
  geom_hline(aes(yintercept = 0.05), linetype = 2, color = 'darkblue') +
  geom_hline(aes(yintercept = -0.05), linetype = 2, color = 'darkblue') +
  geom_hline(aes(yintercept = 0))
round(c(acf(PM2.5_day, plot=FALSE)$acf)[2:11], 2)
round(c(pacf(PM2.5_day, plot=FALSE)$acf)[2:11], 2)
.rs.restartR()
model1 <- arima(PM2.5_day, c(3,0,0))
model1
model2 <- arima(PM2.5_day, c(11, 0, 0))
model2
model3 <- arima(PM2.5_day, c(2, 0, 0))
model3
plot(model3$residual, ylab ="residual")
acf(c(model3$residual), main='Residuals', lag=12)
pacf(c(model3$residual), main='Residuals', lag=12)
