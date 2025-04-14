
suppressPackageStartupMessages(library(forecast)) 
suppressPackageStartupMessages(library(dlm))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(gridExtra))

library(tsm)
library(dlm)
library(tidyverse)
library(lubridate)
require(caTools)


#Read data
data <- read_csv("C:/Users/RUXI/Desktop/Wizz/State Space Models/Hotel price.csv",col_names = TRUE)
y <- data[2]

plot.ts(y)

bound <- floor((nrow(data)/10)*7)  #295
train <- ts(y[1:bound,])
test <- ts(y[(bound + 1):(nrow(data) ),])
plot.ts(train)

y <- as.numeric (unlist (y))


# Build model
model.build <- function(p) {
  return(
    dlmModPoly(2, dV=p[1], dW=p[2:3]) + 
      dlmModSeas(12, dV=p[4])
  )
}

model.mle <- dlmMLE(train, parm=c(1, 1, 1, 1), build=model.build)
model.fit <- model.build(model.mle$par)
model.filtered <- dlmFilter(train, model.fit)
model.smoothed <- dlmSmooth(train, model.fit)
resids <- residuals(model.filtered, sd = FALSE)

# Extracting components
mu <- dropFirst(model.smoothed$s[, 1])
upsilon <- dropFirst(model.smoothed$s[, 2])
gammas <- dropFirst(model.smoothed$s[,3])
mu.1 <- mu[1]
mu.end <- mu[length(mu)]
ups.1 <- upsilon[1]
ups.end <- upsilon[length(mu)]
gammas.1 <- gammas[1]
gammas.end <- gammas[length(mu)]


# Draw graph
par(mfrow = c(4, 1),
    mar = c(2.2, 2.2, 1, 1),
    cex = 0.8)
plot.ts(
  y,
  col = "darkgrey",
  xlab = "",
  ylab = "",
  lwd = 2
)
lines(mu , col = "black")
legend(
  "topleft",
  legend = c("Observed ", "Stochastic level"),
  lwd = c(2, 1),
  col = c("darkgrey", "black"),
  bty = "n"
)
plot.ts(
  upsilon,
  col = "darkgrey",
  xlab = "",
  ylab = "",
  lwd = 2
)
legend(
  "topleft",
  legend = "Slope",
  lwd = 2,
  col = "darkgrey",
  bty = "n"
)
plot.ts(gammas, col="darkgrey", xlab="", ylab="", lwd=2)
legend("topleft", legend="Seasonal", lwd=2, col="darkgrey", bty="n")
plot.ts(
  resids,
  ylab = "",
  xlab = "",
  col = "darkgrey",
  lwd = 2
)
abline(h = 0)
legend(
  "topleft",
  legend = "Residuals",
  lwd = 2,
  col = "darkgrey",
  bty = "n"
)

#Joined
alpha <- mu + gammas + upsilon
par(mfrow=c(1,1),mar=c(2.2,2.2,1,1),cex=0.8)
plot.ts(y, col="darkgrey", xlab="", ylab="", lwd=2)
lines(alpha , col="black")
legend("topleft", legend=c("Observed","State Components"),
       lwd=c(2,1), col=c("darkgrey","black"), bty="n")


# Diagnostics
ac(resids)  # acf
Box.test(resids,
         lag = 12,
         type = "Ljung",
         fitdf = 2)  # joint autocorrelation
# No autocorrelation when p-val bigger than 5%
shapiro.test(resids)  # normality
# Residuals normal when p-val bigger than 5%


#----------------Forecast-----------------------------
n <- length(test)
model.forecast <- dlmForecast(model.filtered, nAhead=n)

x <- index(y)
a <- drop(model.forecast$a%*%t(FF(model.fit)))

df <- rbind(
  data.frame(x=index(y), y=as.numeric(y), series="original"),
  data.frame(x=x[1:bound], y=apply(model.filtered$m[-1,1:2], 1, sum), series="filtered"),
  data.frame(x=x[1:bound], y=apply(model.smoothed$s[-1,1:2], 1, sum), series="smoothed"),
  data.frame(x=x[(bound+1):nrow(data)], y=a, series="forecast")
)
g.dlm <- ggplot(subset(df, x>20), aes(x=x, y=y, colour=series)) + geom_line()
g.dlm

#Accuracy
t <- as.numeric (unlist (test))
f <- as.numeric (unlist (model.forecast$f))
accuracy(f, t)    #Test accuracy

sum = mu + upsilon + gammas
tr <- as.numeric (unlist (train))
accuracy(sum, tr) #Train accuracy


# forecasting using arima
model <- auto.arima(train)
summary(model)
coef.test(model)
model.forecast <- forecast(model, h = n)
plot(model.forecast)

accuracy(model.forecast)



