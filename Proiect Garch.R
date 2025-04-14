# ARCH - GARCH

# Load packages
library(tidyverse)
library(fGarch)
library(forecast)
library(fDMA)
library(lmtest)
library(FinTS)
library(tseries)
library(readxl)
library(lubridate)
library(nortsTest)
library(urca)
library(gt)


#Import the data set 
Date <- read_excel(file.choose())
summary(Date) 

Date$RYA <- na.interp(Date$RYA)
Date$TUI <- na.interp(Date$TUI)
Date$KML <- na.interp(Date$KML)


# RYA ---------------------------------------------------------------------

# Declararea seriei de timp 
y <- ts(Date$RYA, start = decimal_date(as.Date("2000-03-01")), frequency = 260)

# Graficul RYA
autoplot(y) + theme_bw() +
  ylab('RYA') + 
  ggtitle('Monthly RYA price from February 2000 to May 2023') +
  theme(plot.title = element_text(hjust = 0.5))

# Calcularea rentabilitatii
y_returns <- diff(log(y))

# Graficul rentabilitatii 
autoplot(y_returns) + theme_bw() +
  ylab('RYA') + 
  ggtitle('Daily  return of RYA from february 2000 to May 2023') +
  theme(plot.title = element_text(hjust = 0.5))

# Partea 1 - pas 1: verificam daca seria este stationara cu ADF (trend + intercept)
# deoarece seria noastra originala prezinta trend 
adf_trend <- ur.df(y, type='trend', selectlags = c("AIC"))
summary(adf_trend) # serie nestationara deoarece |test statistic| < |critical values|

# verificam acum daca seria rentabilitatilor este stationara si o si logaritmam inainte
# pentru ca este cu siguranta nestationara in dispersie
adf_none <- ur.df(y_returns, type='none', selectlags = c("AIC")) # verificam pentru type = none deoarece nu mai are trend seria
summary(adf_none) # serie stationara |test statistic| > |valori critice|

#ADF
adf.test(y)  #p = 0.39 > 0.1 nonstationary ts
summary(ur.df(y, type='none', selectlags = c("AIC"))) # nonstationary ts   |0.12| < |-2.58| / |-1.95| / |-1.62|
summary(ur.df(y, type='drift', selectlags = c("AIC")))# nonstationary 
summary(ur.df(y, type='trend', selectlags = c("AIC")))# nonstationary
# KPSS
y %>% ur.kpss(type = c("mu")) %>% summary()  #Constant: t > critical values => nonstationary ts
y %>% ur.kpss(type = c("tau")) %>% summary() #Trend:    t > critical values => nonstationary ts
# Philips-Perron
PP.test(y)  # p = 0.25 > 0.05 => nonstationary ts
tseries::pp.test(y,type = c("Z(alpha)"), lshort = TRUE)
tseries::pp.test(y,type = c("Z(t_alpha)"), lshort = TRUE)


#Stationarity tests for rentability
#ADF
summary(ur.df(y_returns, type='none', selectlags = c("AIC"))) # stationary ts  
summary(ur.df(y_returns, type='drift', selectlags = c("AIC")))# stationary 
summary(ur.df(y_returns, type='trend', selectlags = c("AIC")))# stationary
# KPSS
y_returns %>% ur.kpss(type = c("mu")) %>% summary()  #Constant: t < critical values =>stationary ts
y_returns %>% ur.kpss(type = c("tau")) %>% summary() #Trend:    t < critical values =>stationary ts
# Philips-Perron
PP.test(y_returns)  # p = 0.01 < 0.05 => stationary ts
tseries::pp.test(y_returns,type = c("Z(alpha)"), lshort = TRUE)
tseries::pp.test(y_returns,type = c("Z(t_alpha)"), lshort = TRUE)

#Table with stationarity tests
`Unit Root` <- c("ADF level","PP level",  "KPSS level", "ADF returns","PP returns","KPSS returns")
`T&C` <- c("-3.38",    "0.255",    "3.042",    "-54.824***",   "0.01***",        "0.0271***" )
`C` <- c("-1.4603",    "0.04842*", "41.5082",  "-54.8213***",  "0.01***",        "0.0732 ***" )
`None` <- c("-0.0091", "",          "",        "-54.7987***",  "",        "" )

unit_root <- as.data.frame(cbind(`Unit Root`,`T&C`,`C`,`None`))
unit_root %>% gt() %>% tab_header(
  title = md("**Unit root analysis of RYA price and rentability**")) %>%
  tab_source_note(
    source_note = "Note: ***, **, * means stationary at 1%, 5% and 10%; T&C represents the most general model with a constant and trend; C is the model with a constant and without trend; None is the most restricted model without a drift and trend"
  )


# Partea 1 - pas 2: estimam ecuatia mediei cu ARIMA
ggtsdisplay(y_returns, lag.max = 36)
# Nu par sa existe componente AR, MA

arima000 <- Arima(y_returns, order = c(0,0,0), include.constant = TRUE)
coeftest(arima000) 
summary(arima000)

options(scipen = 999)   
# Partea 2 - pas 1: Verificam daca avem efecte ARCH
ArchTest(residuals(arima000), lag = 1) # p < 0.1 => efecte ARCH 
# putem alege mai multe laguri in specificarea modelului ARCH doar ca 
# este important ca valoarea efectelor ARCH sa nu fie negativa, iar pentru asta
# ne putem uite pe PACF si observam ca maximum admis ar putea fi lagul 7
# trebuie ridicate reziduurile la patrat cand rulam 
ggPacf(residuals(arima000)^2, 12) 
# testam si la lagul 2 daca avem efecte ARCH
ArchTest(residuals(arima001), lag = 7) # p < 0.1 => efecte ARCH 
# insa ne dorim ca modelul nostru sa fie cat mai simplu si vom ramane pe 1 laguri
# modelul GARCH ne da o solutie alternativa in alegerea lagurilor


# Partea 2 - pas 2: estimam modelul ARCH
arch.fit <- garchFit(~arma(0,0) + garch(1,0), data = y_returns, trace = F)
summary(arch.fit) # mu este constanta din ecuatia mediei
# omega este constanta din ecuatia variantei
# din outputul modelului arch, pe langa semnificatia si valoarea coef
# avem si testele pentru ipotezele pe reziduuri 
# nu avem autocorelare in medie deoarece Ljung Box > 0.1, hint ca modelul e bun
# avem autocorelate in varianta (R^2) doarece Lj < 0.1, dar nu ne incurca 
# reziduurile nu sunt normal distribuite deaorece JB si Shapiro < 0.1
# Inca avem efecte ARCH in medie => am mai putea adauga laguri in model ca sa il imbunatatim


# GARCH: generalisez autoregresive conditional heteroschedasticity
# Modelul GARCH este o extensie a modelului ARCH care permite o alterinativa 
# pentru estimarea modelelor ARCH cu ordin mare

# Partea 2 - pas 2: estimam modelul GARCH
# Incepem cu un ARCH cu 2 laguri
arch.fit <- garchFit(~arma(0,0) + garch(7,0), data = y_returns, trace = F)
summary(arch.fit) # model cu coef semnificativi, dar inca avem hetero
# LM Arch Test p < 0.1

# GARCH(1,1)
garch.fit <- garchFit(~arma(0,0) + garch(1,1), data = y_returns, trace = F)
summary(garch.fit) # model cu coef semnificativi, fara heteroschedasticitate
# si fara autocorelare

garch.fit <- garchFit(~arma(1,0) + garch(1,1), data = y_returns, trace = F)
summary(garch.fit) # model cu coef semnificativi, fara heteroschedasticitate
# si fara autocorelare

predict(garch.fit, n.ahead = 5)
predict(garch.fit, n.ahead = 5, plot=TRUE, crit_val = 2)


# TUI ---------------------------------------------------------------------


# Declararea seriei de timp 
y <- ts(Date$TUI, start = decimal_date(as.Date("2000-03-01")), frequency = 260)

# Graficul TUI
autoplot(y) + theme_bw() +
  ylab('TUI') + 
  ggtitle('Monthly TUI price from February 2000 to May 2023') +
  theme(plot.title = element_text(hjust = 0.5))

# Calcularea rentabilitatii
y_returns <- diff(log(y))

# Graficul rentabilitatii 
autoplot(y_returns) + theme_bw() +
  ylab('TUI') + 
  ggtitle('Daily  return of TUI from february 2000 to May 2023') +
  theme(plot.title = element_text(hjust = 0.5))


# Partea 1 - pas 1: verificam daca seria este stationara cu ADF (trend + intercept)
# deoarece seria noastra originala prezinta trend 
adf_trend <- ur.df(y, type='trend', selectlags = c("AIC"))
summary(adf_trend) # serie nestationara deoarece |test statistic| < |critical values|

# verificam acum daca seria rentabilitatilor este stationara si o si logaritmam inainte
# pentru ca este cu siguranta nestationara in dispersie
adf_none <- ur.df(y_returns, type='none', selectlags = c("AIC")) # verificam pentru type = none deoarece nu mai are trend seria
summary(adf_none) # serie stationara |test statistic| > |valori critice|



# Partea 1 - pas 2: estimam ecuatia mediei cu ARIMA
ggtsdisplay(y_returns, lag.max = 36)

arima100 <- Arima(y_returns, order = c(1,0,0), include.constant = TRUE)
coeftest(arima100) #AR1 semnificativ
summary(arima100)  #AIC=-25653.65   AICc=-25653.65   BIC=-25633.53

arima001 <- Arima(y_returns, order = c(0,0,1), include.constant = TRUE)
coeftest(arima001) #MA1 semnificativ
summary(arima001)  #AIC=-25652.54   AICc=-25652.54   BIC=-25632.42

arima101 <- Arima(y_returns, order = c(1,0,1), include.constant = TRUE)
coeftest(arima101) #nesemnificativ
summary(arima101)

options(scipen = 999)   
# Partea 2 - pas 1: Verificam daca avem efecte ARCH
ArchTest(residuals(arima001), lag = 1) # p < 0.1 => efecte ARCH 
# putem alege mai multe laguri in specificarea modelului ARCH doar ca 
# este important ca valoarea efectelor ARCH sa nu fie negativa, iar pentru asta
# ne putem uite pe PACF si observam ca maximum admis ar putea fi lagul 1
# trebuie ridicate reziduurile la patrat cand rulam 
ggPacf(residuals(arima001)^2, 12) 
#  ne dorim ca modelul nostru sa fie cat mai simplu si vom ramane pe 1 laguri
# modelul GARCH ne da o solutie alternativa in alegerea lagurilor


# Partea 2 - pas 2: estimam modelul ARCH
arch.fit <- garchFit(~arma(0,1) + garch(1,0), data = y_returns, trace = F)
summary(arch.fit) 


# GARCH(1,1)
garch.fit <- garchFit(~arma(1,0) + garch(1,1), data = y_returns, trace = F)
summary(garch.fit) # model cu coef semnificativi, fara heteroschedasticitate
# si fara autocorelare

# Serie variantei conditionate
garch_conditional_variance <- as.ts(garch.fit@h.t) 

# Graficul variantei conditionate
autoplot(garch_conditional_variance) + theme_bw() +
  ylab('Varianta conditionata') + 
  ggtitle('Estimarea modelului GARCH(1,1)') +
  theme(plot.title = element_text(hjust = 0.5))


predict(garch.fit, n.ahead = 5)
predict(garch.fit, n.ahead = 5, plot=TRUE, crit_val = 2)


# KLM ---------------------------------------------------------------------

# Declararea seriei de timp 
y <- ts(Date$KML, start = decimal_date(as.Date("2000-03-01")), frequency = 260)

# Graficul KLM
autoplot(y) + theme_bw() +
  ylab('KLM') + 
  ggtitle('Monthly KLM price from February 2000 to May 2023') +
  theme(plot.title = element_text(hjust = 0.5))

# Calcularea rentabilitatii
y_returns <- diff(log(y))


# Graficul rentabilitatii 
autoplot(y_returns) + theme_bw() +
  ylab('KLM') + 
  ggtitle('Daily  return of KLM from february 2000 to May 2023') +
  theme(plot.title = element_text(hjust = 0.5))

# Partea 1 - pas 1: verificam daca seria este stationara cu ADF (trend + intercept)
# deoarece seria noastra originala prezinta trend 
adf_trend <- ur.df(y, type='trend', selectlags = c("AIC"))
summary(adf_trend) # serie nestationara deoarece |test statistic| < |critical values|

# verificam acum daca seria rentabilitatilor este stationara si o si logaritmam inainte
# pentru ca este cu siguranta nestationara in dispersie
adf_none <- ur.df(y_returns, type='none', selectlags = c("AIC")) # verificam pentru type = none deoarece nu mai are trend seria
summary(adf_none) # serie stationara |test statistic| > |valori critice|



# Partea 1 - pas 2: estimam ecuatia mediei cu ARIMA
ggtsdisplay(y_returns, lag.max = 36)

arima100 <- Arima(y_returns, order = c(1,0,0), include.constant = TRUE)
coeftest(arima100) #AR1 semnificativ
summary(arima100)  #AIC=-25890.51   AICc=-25890.51   BIC=-25870.39

arima001 <- Arima(y_returns, order = c(0,0,1), include.constant = TRUE)
coeftest(arima001) #MA1 semnificativ
summary(arima001)  #AIC=-25889.64   AICc=-25889.64   BIC=-25869.52

arima101 <- Arima(y_returns, order = c(1,0,1), include.constant = TRUE)
coeftest(arima101) #nesemnificativ
summary(arima101)

options(scipen = 999)   
# Partea 2 - pas 1: Verificam daca avem efecte ARCH
ArchTest(residuals(arima001), lag = 1) # p < 0.1 => efecte ARCH 
# putem alege mai multe laguri in specificarea modelului ARCH doar ca 
# este important ca valoarea efectelor ARCH sa nu fie negativa, iar pentru asta
# ne putem uite pe PACF si observam ca maximum admis ar putea fi lagul 1
# trebuie ridicate reziduurile la patrat cand rulam 
ggPacf(residuals(arima001)^2, 12) 
#  ne dorim ca modelul nostru sa fie cat mai simplu si vom ramane pe 1 laguri
# modelul GARCH ne da o solutie alternativa in alegerea lagurilor


# Partea 2 - pas 2: estimam modelul ARCH
arch.fit <- garchFit(~arma(1,1) + garch(1,0), data = y_returns, trace = F)
summary(arch.fit) 


# GARCH(1,1)
garch.fit <- garchFit(~arma(0,0) + garch(1,1), data = y_returns, trace = F)
summary(garch.fit) # model cu coef semnificativi, fara heteroschedasticitate
# si fara autocorelare

# Serie variantei conditionate
garch_conditional_variance <- as.ts(garch.fit@h.t) 

# Graficul variantei conditionate
autoplot(garch_conditional_variance) + theme_bw() +
  ylab('Varianta conditionata') + 
  ggtitle('Estimarea modelului GARCH(1,1)') +
  theme(plot.title = element_text(hjust = 0.5))


predict(garch.fit, n.ahead = 5)
predict(garch.fit, n.ahead = 5, plot=TRUE, crit_val = 2)
