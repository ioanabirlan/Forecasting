install.packages("prophet")
library(prophet)
library(readxl)
RYAAY <- read_excel("C:/Users/Lenovo/Desktop/RYAAY.xlsx")
attach(RYAAY)

RYAAY <- data.frame(ds = RYAAY$Date, y = RYAAY$Open)

model <- prophet(RYAAY)
# Grafic Prognoza
future <- make_future_dataframe(model, periods = 147)
forecast <- predict(model, future)
plot(model, forecast)

# Componenetele prognozei
prophet_plot_components(model, forecast)

dyplot.prophet(model, forecast)

#cross validation
df.cv <- cross_validation(model, initial = 730, period = 180, horizon = 365, units = 'days')
head(df.cv)
plot_cross_validation_metric(df.cv, metric = 'rmse')
df.p <- performance_metrics(df.cv)
df.p
#valoare rmse
rmse_value <- df.p$rmse[1]
rmse_value


