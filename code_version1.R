library(TSA)
library(tseries)
library(forecast)
library(Metrics)
  
#------------------ Importing the data----------------------
data = read.table("trafquoti.txt",
                  header = FALSE,
                  quote ="",
                  colClasses=c('numeric', 'character'), 
                  col.names = c('trafic', 'date'))

#------------------- Data Preparation -------------------------
data
summary(data)
plot(ts(data))

first_date = data$date[1]
first_date

last_date = data$date[nrow(data)]
last_date

#which date has the lowest number of passenger
data$date[data$trafic == 1915]

data$trafic[data$date == "2001-09-11" ]
data[data$date == "2001-09-14", ]
nrow(data)

# transform the string date to actual date and see if we have some missing value
date.1 = as.Date(data$date)
date.2 = seq(from = as.Date(first_date), to = as.Date(last_date), by = "day")
c(length(date.1), length(date.2)) # no missing value

sum(any(is.na(data)))

#Daily time series
trafic_daily <- ts(data$trafic, start = c(1993, 1), frequency = 365)
plot(trafic_daily, main = "Daily trafic", xlab="year", ylab = "Trafic")

#yearly time series 
year  = substr(data$date, 1, 4)
trafic_year = aggregate(data$trafic, list(year = year), sum)
trafic_year = ts(trafic_year$x, start = 1993, frequency = 1)
str(trafic_year)
plot(trafic_year, main = "Yearly trafic", xlab = "year", ylab = "Trafic")

#monthly time series 
month = substr(data$date, 6, 7)
month.year = as.numeric(paste(year,month,sep ="."))
trafic_month = aggregate(data$trafic, list (month = month.year), sum)
trafic_month = ts(trafic_month$x, start = c(1993,1), frequency = 12)
str(trafic_month)
plot(trafic_month, main = "Monthly trafic", xlab = "year", ylab = "Trafic")


#----------- Data exploration ----------------------------------

# additive  decomposition
decomp.add = decompose(trafic_month, type = "additive")
plot(decomp.add)

# Plot of the time series with additive trend and saisonnality
plot(trafic_month, xlab = "Year", ylab = "Monthly Trafic", main = "decomp additif")
points(decomp.add$trend, type = "l", col = "red")
points(decomp.add$trend + decomp.add$seasonal, type = "l", col = "blue")

ljung_box_test <- Box.test(decomp.add$random, lag = 5, type = "Ljung-Box")
ljung_box_test

# the p_value is very small that mean the noise is correlate therefore
# this decomposition is not accurate


# multiplicative decomposition
decomp.multi = decompose(trafic_month, type = "multiplicative")
plot(decomp.multi)

# Plot of the time series with multicative trend and saisonnality
plot(trafic_month, xlab = "Year", ylab = "Monthly Trafic", main = "decomp multiplicative")
points(decomp.multi$trend, type = "l", col = "red")
points(decomp.multi$trend * decomp.multi$seasonal, type = "l", col = "blue")

ljung_box_test_multi <- Box.test(decomp.multi$random, lag = 5, type = "Ljung-Box")
ljung_box_test_multi

# the p_value is very small that mean the noise is correlate therefore
# this decomposition is not accurate
monthplot(trafic_month)


# Serie before 9/11
trafic_before = window(trafic_month, start  = c(1993,1), end = c(2001,8))
trafic_after = window(trafic_month, start = c(2001,9), end = c(2007,10))

lag.plot(rev(trafic_before), set.lags = 1:12, asp=1, diag = TRUE, diag.col = "red",
         type ="p", do.lines = FALSE, main = "lag before 9/11")

lag.plot(rev(trafic_after), set.lags = 1:12, asp=1, diag = TRUE, diag.col = "red",
         type ="p", do.lines = FALSE, main = "lag after 9/11")

plot(trafic_month, main = "Monthly trafic", xlab = "year", ylab = "Trafic")
abline(v=2001 + 8/12, col="red", lwd=2, lty=2)


#--------------------- Time series before 9/11 -----------------------------
train_data_before = window(trafic_month, start  = c(1993,1), end = c(2000,12))
test_data_before = window(trafic_before, start = c(2001,1), end = c(2001,8))

plot(train_data_before,
     main = "Monthly trafic before 9/11",
     xlab = "year", ylab = "Trafic")

# additive  decomposition
decomp.add = decompose(train_data_before, type = "additive")
plot(decomp.add)

# Plot of the time series with additive trend and saisonnality
plot(trafic_before,
     xlab = "Year",
     ylab = "Monthly Trafic before 09/11",
     main = "decomp additif")

points(decomp.add$trend, type = "l", col = "red")
points(decomp.add$trend + decomp.add$seasonal, type = "l", col = "blue")

ljung_box_test <- Box.test(decomp.add$random, lag = 50, type = "Ljung-Box")
ljung_box_test

# this decomposition is accurate because the pvalue is greater than 0.05

# ACF et PACF to determine the model

# first difference to remove the trend
train_data_before_d1 = diff(train_data_before, differences = 1)
plot(train_data_before_d1, xlab = "Year", ylab = "trafic",
     main = "Monthly Trafic withount trend before 09/11")

acf(train_data_before_d1, lag.max = 100)
pacf(train_data_before_d1, lag.max = 100)


train_data_before_diff_order_12 = diff(train_data_before_d1,
                                       lag = 12,
                                       differences = 1)

plot(train_data_before_diff_order_12, xlab = "Year", ylab = "trafic",
     main = "Monthly Trafic withount trend and saisonality before 09/11")
acf(train_data_before_diff_order_12, lag.max = 100)
pacf(train_data_before_diff_order_12, lag.max = 100)

adf.test(train_data_before_diff_order_12)

# the pvalue is smaller than 0.1 therefore we reject the nulle hypothesis,
#this time series is Stationnary

model_0 = Arima(train_data_before_diff_order_12,
                order = c(0,0,1),
                seasonal=list(order=c(1,0,0),
                period=12,
                include.drift=TRUE))

summary(model_0)

acf(model_0$residuals, lag.max = 50)
pacf(model_0$residuals, lag.max = 50)

#  Ljung-Box on the residus
Box.test(model_0$residuals, lag = 10, type = "Ljung-Box")

# the Pvalue is greater than 0.05 therefore this model is accurate but we can improve it

#Best model we found
model_1 = Arima(train_data_before, order = c(0,1,1),
                seasonal=list(order=c(1,1,0),method='ML',
                period=12,
                include.drift=TRUE))
summary(model_1)
acf(model_1$residuals, lag.max =100)
pacf(model_1$residuals, lag.max = 100)
Box.test(model_1$residuals, lag = 10, type = "Ljung-Box")

# the Pvalue is greater than 0.05 therefore this mode

best_model = auto.arima(train_data_before, stepwise = FALSE, approximation = FALSE)
best_model
Box.test(best_model$residuals, lag = 10, type = "Ljung-Box")


n_forecast = 8  # number of month to forecast
forecasted_values = forecast(model_1, h = n_forecast)

# plot forecast
plot(forecasted_values, main = "Prévisions du modèle SARIMA", ylab = "Valeurs du trafic", xlab = "Temps")

# Add the data since  1993
lines(test_data_before, col = "red", lwd = 2)

# Add legend
legend("topleft", legend = c("Données réelles", "Prévisions"), 
       col = c("red", "blue"), lwd = 2, lty = c(1, 2))

ec80 = model_1$sigma2^.5 * qnorm(0.90)
vajust = fitted(model_1)
matri = as.ts(cbind(trafic_before,vajust-ec80,vajust+ec80),
              start=c(1993,1),
              frequency=12)

par(oma=rep(0,4))
#plot(matri, plot.type='single', lty=c(1,2,2), xlab="temps",
 #    ylab='trafic',main="",cex.main=0.8 )

plot(train_data_before, xlab = "Year", ylab = "trafic",
     main = "Monthly Trafic withount trend before 09/11")

# Ajouter les valeurs ajustées en rouge
lines(vajust, col = "red", lwd = 2)  # Tracer les valeurs ajustées

#legend( par("usr")[1], par("usr")[4], c("Valeur observ´ee","Bande de pr´ediction"),lwd=1,lty=c(1,2))

indi = (trafic_before - (vajust-ec80))>0 & (vajust+ec80 - trafic_before) > 0
prop = 100*sum(indi)/length(indi)
prop
#86.45 % of the prediction are in the confidence interval this mean the model is not
#underfiteed or overfitted the data which is good

predicted_values <- forecasted_values$mean

metrics_SARIMA_before = c(
  MAPE = mape(test_data_before, predicted_values),
  MAE  = mae(test_data_before, predicted_values),
  RMSE = rmse(test_data_before, predicted_values)
)

metrics_SARIMA_before


final_model_before = Arima(trafic_before, order = c(0,1,1),
                           seasonal=list(order=c(1,1,0),method='ML',
                                         period=12,
                                         include.drift=TRUE))

# comparing the model with the real value after 09/11
data_2002 = window(trafic_month, start  = c(2001,9), end = c(2002,12))

forecasted_values_2002 = forecast(final_model_before, h = 16,level = 80)

plot(data_2002, type = "l", col = "blue", lwd = 2, 
     xlab = "Temps", ylab = "Trafic", main = "Comparaison of label and predicted values",
     ylim = range(c(data_2002, forecasted_values_2002$mean)))

lines(forecasted_values_2002$mean, col = "red", lwd = 2)

lines(forecasted_values_2002$lower[,1], col = "black", lty = 2)  # Lower bound
lines(forecasted_values_2002$upper[,1], col = "black", lty = 2)  # upper bound

# Ajouter une légende pour indiquer quelle courbe correspond aux valeurs observées et prédites
legend("bottomright", legend = c("labels", "predicted values"), 
       col = c("blue", "red"), lwd = 2)


loss_month <- data_2002 - forecasted_values_2002$mean
loss_month
#--------------------- Time series after 9/11 -----------------------------

plot(trafic_after,
     main = "Monthly trafic after 9/11",
     xlab = "year", ylab = "Trafic")

train_data_after = window(trafic_after, start = c(2004,1), end = c(2006,12))
test_data_after = window(trafic_after, start = c(2007,1), end = c(2007,10))


decomp.add = decompose(train_data_after, type = "additive")
plot(decomp.add)
plot(train_data_after,
     main = "Monthly trafic after 9/11 between 2004 and 2006",
     xlab = "year", ylab = "Trafic")

points(decomp.add$trend, type = "l", col = "red")
points(decomp.add$trend + decomp.add$seasonal, type = "l", col = "blue")

ljung_box_test <- Box.test(decomp.add$random, lag = 5, type = "Ljung-Box")
ljung_box_test

# pval greater than 0.05 therefore this decomposition is accurate
acf(train_data_after, lag.max = 100)
# no visible trend we only have saisonnality
plot(train_data_after_d1)
train_data_after_diff_order_12 = diff(train_data_after,
                                       lag = 12,
                                       differences = 1)


plot(train_data_after_diff_order_12)
acf(train_data_after_diff_order_12, lag.max  = 50)
best_model_after = auto.arima(train_data_after, stepwise = FALSE, approximation = FALSE)
best_model_after

model_1_after = Arima(train_data_after, order = c(0,1,0),
                seasonal=list(order=c(0,1,0),method='ML',
                              period=12,
                              include.drift=TRUE))
summary(model_1_after)
n_forecast = 10  # number of month to forecast
forecasted_values_model_1_after = forecast(model_1_after, h = n_forecast)

# plot forecast
plot(forecasted_values_model_1_after, main = "Prévisions du modèle SARIMA", ylab = "Valeurs du trafic", xlab = "Temps")

# Add the data since  1993
lines(test_data_after, col = "red", lwd = 2)

# Add legend
legend("topleft", legend = c("labels", "Prediction"), 
       col = c("red", "blue"), lwd = 2, lty = c(1, 2))

# plot forecast
forecasted_values_best_model_after = forecast(best_model_after, h = n_forecast)

plot(forecasted_values_best_model_after, main = "Prévisions du modèle SARIMA", ylab = "Valeurs du trafic", xlab = "Temps")
lines(test_data_after, col = "red", lwd = 2)


vajust = fitted(best_model_after)
plot(train_data_after, xlab = "Year", ylab = "trafic",
     main = "Monthly Trafic withount trend after 09/11")

# Ajouter les valeurs ajustées en rouge
lines(vajust, col = "red", lwd = 2)  # Tracer les valeurs ajustées

# Calcul of MAPE, MAE, et RMSE r
metrics_SARIMA_after = c(
  MAPE = mape(test_data_after, forecasted_values_best_model_after$mean),
  MAE  = mae(test_data_after, forecasted_values_best_model_after$mean),
  RMSE = rmse(test_data_after, forecasted_values_best_model_after$mean)
)

metrics_SARIMA_after


# --------------------------- Holt Winters method -------------------------------
hw_model = HoltWinters(train_data_after)
hw_model

n_forecast = 10  # number of month to forecast
forecasted_values_hw_model = forecast(hw_model, h = n_forecast)

# plot forecast
plot(forecasted_values_hw_model, main = "Forecast HoltWinters", ylab = "Values of trafic", xlab = "Times")

# Add the data since  1993
lines(test_data_after, col = "red", lwd = 2)

# Add legend
legend("topleft", legend = c("labels", "Prediction"), 
       col = c("red", "blue"), lwd = 2, lty = c(1, 2))

# Calcul of MAPE, MAE, et RMSE r
metrics_holtwinters <- c(
  MAPE = mape(test_data_after, forecasted_values_hw_model$mean),
  MAE  = mae(test_data_after, forecasted_values_hw_model$mean),
  RMSE = rmse(test_data_after, forecasted_values_hw_model$mean)
)

metrics_holtwinters


