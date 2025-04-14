
# Loading the required packages
library(fGarch)
library(xts)
library(dplyr)
library(forecast)
library(skimr)
library(tseries)
library(urca)

library(FinTS)
library(rugarch)
library(rmgarch)
library(tidyquant)
library(data.table)
library(readr)
library(lmtest)
library(psych)
library(readxl)
library(tidyverse)
library(stargazer)
library(openxlsx)

#Import the data set 
Date <- read_excel(file.choose())
summary(Date) 

Date$RYA <- na.interp(Date$RYA)
Date$TUI <- na.interp(Date$TUI)
Date$KLM <- na.interp(Date$KLM)

write.xlsx(Date, "Inputed_DCC_Garch_Data.xlsx", colNames = TRUE, append = FALSE)

# Part 1 - Descriptive Statistics -----------------------------------------

# Create ts and plot 
RYA <- Date %>% dplyr::select(Date, RYA)
RYA <- as.xts(as.data.table(RYA))
windows()
plot(RYA, main = 'RYA', col="black")

TUI <- Date %>% dplyr::select(Date, TUI)
TUI <- as.xts(as.data.table(TUI))
windows()
plot(TUI, main = 'TUI', col="black")

KLM <- Date %>% dplyr::select(Date, KML)
KLM <- as.xts(as.data.table(KLM))
windows()
plot(KLM, main = 'KLM', col="black")

# All plots into a window
windows()
par(mfrow=c(3,1))
plot(RYA, main = 'Ryanair',col="darkgreen")
plot(TUI, main = 'TUI Airlines',col="darkgreen")
plot(KLM, main = 'KLM',col="darkgreen")
mtext("Airlines companies", side = 3, line = -1, outer = TRUE, col = "midnightblue",font = 4,cex=1)

# Ggplot
windows()
ggplot(mapping = aes(x = Index, y = Value)) +
  geom_line(data = fortify(RYA, melt = TRUE), aes(color = "RYA")) + 
  geom_line(data = fortify(TUI, melt = TRUE), aes(color = "TUI")) + 
  geom_line(data = fortify(KLM, melt = TRUE), aes(color = "KLM")) +
  scale_color_manual(values=c('darkgreen','darkseagreen3','lightseagreen'))+
  xlab("An") + ylab("Valoare")+
  ggtitle("Airlines companies")+
  theme_bw()+
  theme(plot.title=element_text(face = "bold",size=16,color="dodgerblue4",hjust = .5))



# Daily return computation of the series
# Log returns price
rRYA <- dailyReturn(RYA) * 100
rTUI<- dailyReturn(TUI) * 100
rKLM <- dailyReturn(KLM) * 100


# We put all data into a data frame for use in the multivariate model
rX <- data.frame(rRYA, rTUI, rKLM)
rX <- rX[-1,]
names(rX)[1] <- "rRYA"
names(rX)[2] <- "rTUI"
names(rX)[3] <- "rKLM"


# Descriptive statistics
summary(rX)
skim(rX) 
describe(rX)


# Plot returns in the same window
windows()
par(mfrow=c(3,1))
plot(rRYA, main = 'Ryanair',col="darkgreen")
plot(rTUI, main = 'TUI Airlines',col="darkgreen")
plot(rKLM, main = 'KLM',col="darkgreen")


# Jarque Berra test for normality
jarque.bera.test(rRYA)
jarque.bera.test(rTUI)
jarque.bera.test(rKLM)

# Arch LM test
ArchTest(rX$rRYA,lags=12) 
ArchTest(rX$rTUI,lags=12)
ArchTest(rX$rKLM,lags=12) # ARCH effects in all the daily returns => GARCH modelling

#Ljung Box test
Box.test(rX$rRYA,lag = 1, type = c("Ljung-Box"))
Box.test(rX$rTUI,lag = 1, type = c("Ljung-Box")) 
Box.test(rX$rKLM,lag = 1, type = c("Ljung-Box")) 


# ADF test to check the stationary of daily returns
# H0: non-stationary series
adf.test(rX$rRYA)
adf.test(rX$rTUI)
adf.test(rX$rKLM)
# all the series are stationary 


# Correlations of daily returns
Cor_dp <-cor(rX,method = ("pearson"))
View(as.matrix(Cor_dp))


# Univariate Garch --------------------------------------------------------

#RYA
# Part 1.1 - mean equation estimation
ggtsdisplay(rX$rRYA, lag.max = 36) #no lags 
arima000 <- Arima(rRYA, order = c(0,0,0))
coeftest(arima000)  
summary(arima000)

# Part 1.2 - checking the ARCH effects in residuals
ArchTest(residuals(arima000), lag = 1) # p < 0.1 => ARCH effects 
arima000_residuals_squared <- residuals(arima000)^2
ggPacf(arima000_residuals_squared,10) # at most 10 lags

# Partea 2.1 - GARCH estimation
garch.fit <- garchFit(~arma(0,0) + garch(1,1), data = rRYA, trace = F)
summary(garch.fit) # all coefficient significant 
# no serial correlation, no ARCH effect so GARCH(1,1) captures the information 


#TUI
# Part 1.1 - mean equation estimation
ggtsdisplay(rX$rTUI, lag.max = 36) # maximal order would be AR(1), MA(1) 
arima100 <- Arima(rTUI, order = c(1,0,0))
coeftest(arima100)  # AR(1) significant
summary(arima100) # AIC=29915.39

# Part 1.2 - checking the ARCH effects in residuals
ArchTest(residuals(arima100), lag = 1) # p < 0.1 => ARCH effects 
arima100_residuals_squared <- residuals(arima101)^2
ggPacf(arima100_residuals_squared,10) # at most 6 lags

# Partea 2.1 - GARCH estimation
garch.fit <- garchFit(~arma(1,0) + garch(1,1), data = rTUI, trace = F)
summary(garch.fit) # all coefficient significant 
# no serial correlation, no ARCH effect so GARCH(1,1) captures the information 


#KLM
# Part 1.1 - mean equation estimation
ggtsdisplay(rX$rKLM, lag.max = 36) # maximal order would be AR(1), MA(1) 
arima100 <- Arima(rKLM, order = c(1,0,0))
coeftest(arima100) # AR(1) significant
summary(arima100)  # AIC=29577.01 

# Part 1.2 - checking the ARCH effects in residuals
ArchTest(residuals(arima100), lag = 1) # p < 0.1 => ARCH effects 
arima100_residuals_squared <- residuals(arima100)^2
ggPacf(arima100_residuals_squared,10) # at most 1 lag

# Partea 2.1 - GARCH estimation
garch.fit <- garchFit(~arma(1,0) + garch(1,1), data = rKLM, trace = F)
summary(garch.fit) # all coefficient significant except AR(1)
# no serial correlation, no ARCH effect so GARCH(1,1) captures the information 


# DCC GARCH ---------------------------------------------------------------


# DCC Garch libraries 
library(rugarch)
library(rmgarch)
library(urca)
library(FinTS)
library(dplyr)
library(quantmod)

# Prepare specification with AR(1) and MA(1) to check if there is significance 
uspec.n = multispec(replicate(3, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf = multifit(uspec.n, rX) 
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')

# Fit the DCC model 
fit1 = dccfit(spec1, data = rX, fit.control = list(eval.se = TRUE), fit = multf)
fit1

# Persistance RYA
0.077833+0.894909
# Persistance TUI
0.067410 + 0.931590
# Persistance KLM
0.017916  + 0.960951

# Get the model based time varying covariance (arrays) and correlation matrices
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
cor1
# Correlation matrix for the last day 
dim(cor1)
cor1[,,dim(cor1)[3]]

# Plot the correlations
par(mfrow=c(1,1))
cor_RYA_KLM <- cor1[1,3,] 
cor_RYA_KLM <- as.xts(cor_RYA_KLM)
plot(cor_RYA_KLM)

cor_TUI_KLM <- cor1[2,3,] 
cor_TUI_KLM <- as.xts(cor_TUI_KLM)
plot(cor_TUI_KLM)


# Plot all 2 assets correlations between KLM
windows()
par(mfrow=c(2,2))
plot(cor_RYA_KLM, main = 'RYA and KLM',col="darkgreen")
plot(cor_TUI_KLM, main = 'TUI and KLM',col="darkgreen")


# Part 4 - DCC Forecast ---------------------------------------------------

# Forecast the correlations for the next 15 days
dccforecast <- dccforecast(fit1, n.ahead = 15)
dccforecast

# The actual forecasts for the correlations
Rf <- dccforecast@mforecast$R
str(Rf)

# Correlation forecasts
corf_RYA_KLM <- Rf[[1]][1,3,] # correlations between RYA and KLM
corf_TUI_KLM <- Rf[[1]][2,3,] # correlations between TUI and KLM

mean(corf_RYA_KLM)
mean(corf_TUI_KLM)


# Plot the forecasts
c_RYA_KLM <- c(tail(cor1[1,3,],50),rep(NA,15))  # gets the last 50 correlation observations
cf_RYA_KLM <- c(rep(NA,50),corf_RYA_KLM) # gets the 15 forecasts

c_TUI_KLM <- c(tail(cor1[2,3,],50),rep(NA,15))  # gets the last 50 correlation observations
cf_TUI_KLM <- c(rep(NA,50),corf_TUI_KLM) # gets the 15 forecasts


windows()
par(mfrow=c(2,2))
plot(c_RYA_KLM,type = "l",main="Correlation RYA and KLM")
lines(cf_RYA_KLM,type = "l", col = "orange")
plot(c_TUI_KLM,type = "l",main="Correlation TUI and KLM")
lines(cf_TUI_KLM,type = "l", col = "orange")
mtext("Airlines", side = 3, line = -1, outer = TRUE, col = "midnightblue",font = 4,cex=1)

















