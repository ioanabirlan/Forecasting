library(forecast)
library(uroot)
library(stats)
library(readxl)
library(tseries)
library(urca)
library(vars)
library(mFilter)
library(tidyverse)
library(stargazer)
library(Metrics)
library(tsDyn)
library(dynlm)
library(aTSA)
library(quantmod)
library(tidyquant)

RYAN <- read_csv("C:/Users/Tiberius/Desktop/ST MASTER/proiect lol/RYAAY lunar.csv")
oilprice <- read_csv("C:/Users/Tiberius/Desktop/ST MASTER/proiect lol/MCOILBRENTEU.csv")
IPC<- read_csv("C:/Users/Tiberius/Desktop/ST MASTER/proiect lol/IPC air transport.csv")
IP<- read_csv("C:/Users/Tiberius/Desktop/ST MASTER/proiect lol/industrial production index.csv")

ryanmare<- ts(RYAN$Close, start = c(1997,6),  end=c(2023,4), frequency = 12)
ipc<- ts(IPC$CP0733NLM086NEST, start = c(1996, 1), end=c(2023, 2), frequency=12) 
ip<- ts(IP$NLDPROINDMISMEI, start = c(1960, 1), end=c(2023, 2), frequency=12) 
oilbrent<- ts(oilprice$MCOILBRENTEU, start = c(1987,5),  end=c(2023,4), frequency = 12)

oilbrent96<-window(oilbrent, start=c(1996, 1), end=c(2023,2))
ip96<-window(ip, start=c(1996, 1))
oilbrent97<-window(oilbrent, start=c(1997, 6))


# VAR ---------------------------------------------------------------

autoplot(cbind(oilbrent97, ryanmare))+
  ggtitle("Pretul petrolului si al Ryanair")+
  ylab("Preturi")+
  theme_bw()
lryanmare<-log(ryanmare)
ggtsdisplay(lryanmare)
summary(ur.df(lryanmare, type = "trend", selectlags = "AIC")) 
summary(ur.df(lryanmare, type = "drift", selectlags = "AIC"))
summary(ur.df(lryanmare, type = "none", selectlags = "AIC")) 
summary(ur.kpss(lryanmare)) 
PP.test(lryanmare) 
ggtsdisplay(diff(lryanmare))
summary(ur.df(diff(lryanmare), type = "trend", selectlags = "AIC")) #stat
summary(ur.df(diff(lryanmare), type = "drift", selectlags = "AIC")) #stat
summary(ur.df(diff(lryanmare), type = "none", selectlags = "AIC")) #nestat
summary(ur.kpss(diff(lryanmare)))
PP.test(diff(lryanmare)) 

loilbrent97<-log(oilbrent97)
ggtsdisplay(loilbrent97)
summary(ur.df(loilbrent97, type = "trend", selectlags = "AIC")) #stat
summary(ur.df(loilbrent97, type = "drift", selectlags = "AIC")) #stat
summary(ur.df(loilbrent97, type = "none", selectlags = "AIC")) #nestat
summary(ur.kpss(loilbrent97))
PP.test(loilbrent97)
ggtsdisplay(diff(loilbrent97))
summary(ur.df(diff(loilbrent97), type = "trend", selectlags = "AIC")) #stat
summary(ur.df(diff(loilbrent97), type = "drift", selectlags = "AIC")) #stat
summary(ur.df(diff(loilbrent97), type = "none", selectlags = "AIC")) #nestat
summary(ur.kpss(diff(loilbrent97)))
PP.test(diff(loilbrent97)) 

dfVARnou<-cbind(diff(log(oilbrent97)), diff(log(ryanmare)))
colnames(dfVARnou) <- cbind("oil", "ryan")
lagselectnou <- VARselect(dfVARnou,lag.max = 8, type = 'const')
lagselectnou
# Implementarea VAR
modelnou <- vars::VAR(dfVARnou, p = 1, type = 'const', season = NULL, exog = NULL)
summary(modelnou)
library(stargazer)
stargazer(modelnou[['varresult']], type = 'text')

# Autocorelarea
Serialnou <- serial.test(modelnou, lags.pt = 12, type = 'PT.asymptotic')
Serialnou #0.7701
# Heteroscedasticitate
Arch1 <- vars::arch.test(modelnou,lags.multi = 12,multivariate.only = TRUE)
Arch1 #0.05631
# Normalitatea reziduurilor
Norm1 <- normality.test(modelnou, multivariate.only = TRUE)
Norm1 #

# Testarea pentru rupturi in serie
Stabilitynou <- stability(modelnl,type = 'OLS-CUSUM')
plot(Stabilitynou) # model stabil deoarece seriile noastre nu depasesc intervalul rosu

Grangernou1<- causality(modelnou, cause = 'oil')
Grangernou1 
Grangernou2 <- causality(modelnou, cause = 'ryan')
Grangernou2 

irfnou1 <- irf(modelnou, impulse = 'oil', response = 'ryan', 
               n.ahead = 20, boot = TRUE, ci=0.95)
plot(irfnou1, ylab = 'Ryanair Close Price', main = 'Raspunsul preturilor actiunilor Ryan la socurile pretului petrolului')

irfnou2 <- irf(modelnou, impulse = 'ryan', response = 'oil', 
               n.ahead = 20, boot = TRUE, ci=0.90)
plot(irfnou2, ylab = 'Crude Oil Brent Europe', main = 'Raspunsul pretului petrolului la socurile Ryanair')

FEVD <- fevd(modelnou, n.ahead = 10)
plot(FEVD)

# Prognoza VAR
forecastnou <- predict(modelnou, n.ahead = 12, ci = 0.90) 
plot(forecastnou, name = 'oil')
plot(forecastnou, name = 'ryan')
#exp(diffinv(forecastnou[["fcst"]][["oil"]][,1], lag = 1))


# SVAR --------------------------------------------------------------------
autoplot(cbind(ipc,  ip96, oilbrent96))+
  ggtitle("IPC transport aerian, productia industriala si crude oil price europe")+
  ylab("")+
  theme_bw()
ggtsdisplay(oilbrent96)
summary(ur.df(oilbrent96, type = "trend", selectlags = "AIC")) #nestat
summary(ur.df(oilbrent96, type = "drift", selectlags = "AIC")) #nestat
summary(ur.df(oilbrent96, type = "none", selectlags = "AIC")) #nestat
summary(ur.kpss(oilbrent96)) #serie stationara
PP.test(oilbrent96) #serie nestationara
ndiffs(oilbrent96)
ggtsdisplay(diff(oilbrent96,1))
summary(ur.df(diff(oilbrent96,1), type = "trend", selectlags = "AIC"))
summary(ur.df(diff(oilbrent96,1), type = "drift", selectlags = "AIC"))
summary(ur.df(diff(oilbrent96,1), type = "none", selectlags = "AIC"))
summary(ur.kpss(diff(oilbrent96,1))) #serie stationara
PP.test(diff(oilbrent96, 1))
ndiffs(diff(oilbrent96, 1))

ggtsdisplay(ipc)
summary(ur.df(ipc, type = "trend", selectlags = "AIC")) #stat
summary(ur.df(ipc, type = "drift", selectlags = "AIC")) #nestat
summary(ur.df(ipc, type = "none", selectlags = "AIC")) #nestat
summary(ur.kpss(ipc)) #serie nestationara
PP.test(ipc) #serie stationara
ndiffs(ipc)
ggtsdisplay(diff(ipc,1))
summary(ur.df(diff(ipc,1), type = "trend", selectlags = "AIC"))
summary(ur.df(diff(ipc,1), type = "drift", selectlags = "AIC"))
summary(ur.df(diff(ipc,1), type = "none", selectlags = "AIC"))
summary(ur.kpss(diff(ipc,1))) #serie stationara
PP.test(diff(ipc, 1))
ndiffs(diff(ipc, 1))

ggtsdisplay(ip96)
summary(ur.df(ip96, type = "trend", selectlags = "AIC")) #stat
summary(ur.df(ip96, type = "drift", selectlags = "AIC")) #nestat
summary(ur.df(ip96, type = "none", selectlags = "AIC")) #nestat
summary(ur.kpss(ip96)) #serie nestationara
PP.test(ip96) #serie stationara
ndiffs(ip96)
ggtsdisplay(diff(ip96,1))
summary(ur.df(diff(ip96,1), type = "trend", selectlags = "AIC"))
summary(ur.df(diff(ip96,1), type = "drift", selectlags = "AIC"))
summary(ur.df(diff(ip96,1), type = "none", selectlags = "AIC"))
summary(ur.kpss(diff(ip96,1))) #serie stationara
PP.test(diff(ip96, 1))
ndiffs(diff(ip96, 1))

difoilbrent<-diff(oilbrent96)
difipc<-diff(ipc)
difip<-diff(ip96)
dfVARSV<-cbind(difoilbrent, difipc, difip)
colnames(dfVARSV) <- cbind("oil", "ipc", "indprod")
lagselectSV <- VARselect(dfVARSV,lag.max = 8, type = 'const')
lagselectSV
modelSV <- vars::VAR(dfVARSV, p = 7, type = 'const', exog = NULL)
summary(modelSV)
library(stargazer)
stargazer(modelSV[['varresult']], type = 'text')

amat<-diag(3)
amat[2,1]<-NA
amat[3,1]<-NA
amat[3,2]<-NA
SVARmod<-SVAR(modelSV, Amat=amat, estmethod = c("scoring", "direct"))
summary(SVARmod)

irf1 <- irf(SVARmod, impulse = 'oil', response = 'ipc', 
              n.ahead = 20, boot = TRUE, ci=0.90)
plot(irf1, ylab = 'ipc transport aerian')

irf2 <- irf(SVARmod, impulse = 'oil', response = 'indprod', 
           n.ahead = 20, boot = TRUE, ci=0.90)
plot(irf2, ylab = 'productia industriala')

irf3 <- irf(SVARmod, impulse = 'ipc', response = 'indprod', 
           n.ahead = 20, boot = TRUE, ci=0.90)
plot(irf3, ylab = 'productia industriala')

irf4 <- irf(SVARmod, impulse = 'indprod', response = 'ipc', 
           n.ahead = 20, boot = TRUE, ci=0.90)
plot(irf4, ylab = 'ipc transport aerian')

FEVD <- fevd(SVARmod, n.ahead = 10)
plot(FEVD)

# ARIMA --------------------------------------------------------------

data <- read_csv("C:/Users/Tiberius/Desktop/ST MASTER/proiect lol/Hotel price.csv",col_names = TRUE)

hot<- ts(data$CP1100NLM086NEST, start = c(1996, 1),  end = c(2023,4), frequency = 12)
autoplot(hot)+
  ggtitle("IPC hotels NL")+
  ylab("IPC hotels")+
  theme_bw()

###impartire train si test
length(hot) #328
training_hot <- window(hot, start=c(1996, 1), end=c(2015, 2))
length(training_hot) #230
test_hot <- tail(hot, 98)
length(test_hot) #98

hegy.test(log(training_hot))

lt<-log(training_hot)
ggtsdisplay(log(training_hot))
summary(ur.df(log(training_hot), type = "trend", selectlags = "AIC"))
summary(ur.df(log(training_hot), type = "drift", selectlags = "AIC"))
summary(ur.df(log(training_hot), type = "none", selectlags = "AIC"))
summary(ur.kpss(log(training_hot)))
PP.test(log(training_hot))
nsdiffs(log(training_hot))

trainingdsezlog<-diff(lt,12)
ggtsdisplay(trainingdsezlog)
summary(ur.df(trainingdsezlog, type = "trend", selectlags = "AIC"))
summary(ur.df(trainingdsezlog, type = "drift", selectlags = "AIC"))
summary(ur.df(trainingdsezlog, type = "none", selectlags = "AIC"))
summary(ur.kpss(trainingdsezlog)) 
PP.test(trainingdsezlog)
ndiffs(trainingdsezlog)

ggtsdisplay(diff(trainingdsezlog))
summary(ur.df(diff(trainingdsezlog), type = "trend", selectlags = "AIC"))
summary(ur.df(diff(trainingdsezlog), type = "drift", selectlags = "AIC"))
summary(ur.df(diff(trainingdsezlog), type = "none", selectlags = "AIC"))
summary(ur.kpss(diff(trainingdsezlog))) 
PP.test(diff(trainingdsezlog))
ndiffs(diff(trainingdsez))

#pacf--1 laguri-->ar(2)
#acf-->1 lag-->ma(1)
#laguri sez neg-->comp sma

auto.arima(training_hot, trace=TRUE)

fit0 <- Arima(training_hot,order=c(1,1,1), seasonal=c(1,1,1))
coeftest(fit0) 
summary(fit0)

fit1 <- Arima(training_hot,order=c(1,1,0), seasonal=c(1,1,1))
coeftest(fit1) 
summary(fit1) #AIC=258.05

fit2 <- Arima(training_hot,order=c(0,1,1), seasonal=c(0,1,1))
coeftest(fit2) 
summary(fit2) #AIC=260.24

fit3 <- Arima(training_hot,order=c(1,1,0), seasonal=c(0,1,1))
coeftest(fit3) 
summary(fit3) # AIC=259.37

fit4 <- Arima(training_hot,order=c(1,1,0), seasonal=c(1,1,0))
coeftest(fit4) 
summary(fit4) #AIC=262.09 

fit5 <- Arima(training_hot,order=c(0,1,1), seasonal=c(1,1,1))
coeftest(fit5) 
summary(fit5) #AIC=259.39  

fit6 <- Arima(training_hot,order=c(0,1,1), seasonal=c(1,1,0))
coeftest(fit6) 
summary(fit6) #AIC=262.64 

fit7 <- Arima(training_hot,order=c(0,1,1), seasonal=c(0,1,1))
coeftest(fit7) 
summary(fit7) #AIC=260.24


checkresiduals(fit1)
jarque.bera.test(residuals(fit1)) 
Box.test(residuals(fit1), lag=1)
Box.test(residuals(fit1), lag=2)
Box.test(residuals(fit1), lag=3)
Box.test(residuals(fit1), lag=4)
Box.test(residuals(fit1), lag=5)
Box.test(residuals(fit1), lag=12)
Box.test(residuals(fit1), lag=24) 
Box.test(residuals(fit1), lag=1, type = 'Lj')
Box.test(residuals(fit1), lag=2, type = 'Lj')
Box.test(residuals(fit1), lag=3, type = 'Lj')
Box.test(residuals(fit1), lag=4, type = 'Lj')
Box.test(residuals(fit1), lag=5, type = 'Lj')
Box.test(residuals(fit1), lag=12, type = 'Lj')
Box.test(residuals(fit1), lag=24, type = 'Lj') 
ArchTest(residuals(fit1), lags = 1)
ArchTest(residuals(fit1), lags = 2)
ArchTest(residuals(fit1), lags = 3)
ArchTest(residuals(fit1), lags = 4)
ArchTest(residuals(fit1), lags = 5)
ArchTest(residuals(fit1), lags = 12)
ArchTest(residuals(fit1), lags = 24) 

# Testam acuratetea si pe zona de test
arima1_acc<- fit1%>% forecast::forecast(h=98) 
summary(arima1_acc) 
forecast::accuracy(arima1_acc, test_hot) 

# Prognoza modelului ARIMA
fit1 %>% forecast::forecast(., h=98) %>% autoplot()
plot(fit1%>%forecast::forecast(h=98))
lines(fitted(fit1), col = "red", lwd = 2.5)

# Prognoza prin ETS
ets <- ets(training_hot)
ets_acc<-ets%>%forecast::forecast(h=98)
summary(ets_acc) 
forecast::accuracy(ets_acc,test_hot) #19.0751244 RMSE

autoplot(ets_acc)
plot(ets%>%forecast::forecast(h=98))
lines(fitted(ets), col = "red", lwd = 2.5)

#Holt linear method
holtlm <- holt(training_hot,h=98)
summary(holtlm)
forecast::accuracy(holtlm,test_hot) #10.9999954 RMSE
autoplot(holtlm)
plot(holtlm)
lines(fitted(holtlm), col = "red", lwd = 2.5)

#hw
holtwint <- hw(training_hot,h=98, seasonal = "multiplicative")
summary(holtwint)
forecast::accuracy(holtwint,test_hot) 

autoplot(hot) +
  autolayer(ets_acc, series="ETS", PI=FALSE) +
  autolayer(arima1_acc, series="ARIMA", PI=FALSE) +
  autolayer(holtlm, series="Holt linear met", PI=FALSE) +
  autolayer(holtwint, series="HW multip", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast")) +
  xlab("Time") + ylab(" ") +
  ggtitle("Final forecasts") +
  theme_bw()

