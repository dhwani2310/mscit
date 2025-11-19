#Practice TS Modeling

#Load libraries ======================

      library(pdfetch)
      library(forecast)
      library(lubridate)
      library(TSstudio)
      library(tidyverse)
      library(cowplot)
      library(tsbox) #convert xts to tsibble
      library(TSstudio)
      library(rio)
      library(forecast)
      library(pdfetch)
      library(quantmod)
      library(tsibble)
      library(dygraphs)
      library(vars)

# ===================  #

      #The autoregressive Model AR(p)
      
      oil = pdfetch_FRED("DCOILWTICO")
      ts_info(oil)
      oil = xts::to.monthly(oil$DCOILWTICO)[,4]
      oil.ts = ts(oil, start = c(1986,1), frequency = 12)
      oil.ts = window(oil.ts, start = c(2010,1), frequency = 12)
      ts_info(oil.ts)
      
      
      myoil = ts_split(oil.ts, 30)
      train_oil = myoil$train
        ts_info(train_oil)
      test_oil = myoil$test
        ts_info(test_oil)
      ts_plot(oil.ts)
      lm(oil.ts ~ stats::lag(oil.ts,n=1))
      ar_model = ar(train_oil)
      ar_model
      h=30
      ar_pred = predict(ar_model, n.ahead = h )
      
      ts_plot(cbind(train_oil, test_oil, ar_pred$pred))
      
      

#' AR Model Signature:
#' An AR(p) model is characterized by a PACF plot that cuts off 
#' (becomes insignificant) after lag p and an ACF plot that gradually decays. 

# OLD school
tsdisplay(oil.ts)

mypacf = pacf(oil.ts)
mypacf

#' Determining the Order (p):
#' The order of the AR process is determined by the lag at which the 
#' PACF becomes statistically insignificant. 


# In Class =======


#Problem 1
#'Load the Cass Freight Index: Shipments (ticker = FRGSHPUSM649NCIS). 
#'transform it the the ts type - using TSBox, or old school using ts(); 
#'and plot it.
#'Examine the series: use the following:

freight <- pdfetch_FRED("FRGSHPUSM649NCIS")
freight = xts::to.monthly(freight$FRGSHPUSM649NCIS)[,4]
freight.ts <- tsbox::ts_ts(freight)
freight.ts <- window(freight.ts, start = c(2010, 1), frequency = 12)
ts_info(freight.ts)
TSstudio::ts_plot(freight.ts,
                  title = "Cass Freight Index: Shipments",
                  Ytitle = "Index Value",
                  Xtitle = "Year")

TSstudio::ts_seasonal(freight.ts, type = "all")
ggseasonplot(freight.ts, main = "Seasonal Plot: Cass Freight Index Shipments")
ggmonthplot(freight.ts, main = "Monthly Plot: Cass Freight Index Shipments")



# Problem 2
#' Split into training and testing.  
#' Plot the two series
freight = ts_split(freight.ts, 30)
train_freight = freight$train
ts_info(train_freight)

test_freight = freight$test
ts_info(test_freight)

library(TSstudio)
TSstudio::ts_plot(
  cbind(train_freight, test_freight),
  title = "Cass Freight Index: Train vs Test Split",
  Ytitle = "Index Value",
  Xtitle = "Year"
)


#Problem 3
#' Use the ses function from the forecast package to get a forecast 
#' based on simple exponential smoothing fit of the training data, 
#' plot the forecast and compare to the testing data.
#' Plot all series
#' determine the accuracy

train_freight <- ts_ts(train_freight)
test_freight <- ts_ts(test_freight)
ses_fit <- ses(train_freight, h = length(test_freight))
autoplot(ses_fit) +
  autolayer(test_freight, series = "Test Data") +
  ggtitle("SES Forecast vs Test Data") +
  ylab("Index Value") + xlab("Year")
TSstudio::ts_plot(
  cbind(train_freight, test_freight, ses_fit$mean),
  title = "Cass Freight Index: SES Forecast vs Actual",
  Ytitle = "Index Value",
  Xtitle = "Year"
)
accuracy(ses_fit, test_freight)

# Problem 4
# Fit an exponential smoothing model using the ets function - again on the
# training set
#' Then pass the model as input to the forecast function to get a 
#' forecast for the next h months, 
#' and plot the forecast, testing and training sets.

train_freight <- ts_ts(train_freight)
test_freight <- ts_ts(test_freight)
ets_fit <- ets(train_freight)
ets_forecast <- forecast(ets_fit, h = length(test_freight))
autoplot(ets_forecast) +
  autolayer(test_freight, series = "Test Data") +
  ggtitle("ETS Forecast vs Test Data") +
  ylab("Index Value") + xlab("Year")

TSstudio::ts_plot(
  cbind(train_freight, test_freight, ets_forecast$mean),
  title = "Cass Freight Index: ETS Forecast vs Actual",
  Ytitle = "Index Value",
  Xtitle = "Year"
)


# Problem 5
#' Fit a 12-month trailing moving average model on the training set.
#' predict h periods above; plot all series.  the actual
#' the training, the testing and the moving average prediction

train_freight <- ts_ts(train_freight)
test_freight <- ts_ts(test_freight)
ma_fit <- stats::filter(train_freight, filter = rep(1/12, 12), sides = 1)
h <- length(test_freight)
last_value <- tail(ma_fit, 1)
ma_forecast <- ts(rep(last_value, h),
                  start = end(train_freight) + c(0, 1),
                  frequency = frequency(train_freight))
TSstudio::ts_plot(
  cbind(train_freight, test_freight, ma_fit, ma_forecast),
  title = "Cass Freight Index: 12-Month Moving Average Forecast vs Actual",
  Ytitle = "Index Value",
  Xtitle = "Year"
)

# Problem 6
#' Print a summary of the model estimated in the previous exercise, 
#' and find the automatically estimated structure of the model. 
#' Does it include trend and seasonal components? 

summary(ets_fit)
ets_fit

# Problem 7
#' Use the accuracy function from the forecast package to get accuracy 
#' measures for the forecast obtained in the previous exercise. 
#' Note the RMSE.
forecast::accuracy(ets_forecast, test_freight)

#====================== #

      
      #=================== #
      # Deliverable: 
      # using the CASS Shipments Index data. 
      # fit an AR model
      # onto the training series.  
      # forecast onto the testing series
      # and plot all series.
      
      # Upload the graph to the bucket labeled AR.
      # by Friday October 24.
      # ===================  # 

# AR Model on Cass Freight Index
library(forecast)
library(TSstudio)
library(tsbox)
library(stats)
train_freight <- ts_ts(train_freight)
test_freight <- ts_ts(test_freight)
ar_model <- ar(train_freight)
h <- length(test_freight)
ar_forecast <- predict(ar_model, n.ahead = h)
ar_pred <- ts(ar_forecast$pred,
              start = end(train_freight) + c(0, 1),
              frequency = frequency(train_freight))
TSstudio::ts_plot(
  cbind(train_freight, test_freight, ar_pred),
  title = "Cass Freight Index: AR Model Forecast",
  Ytitle = "Index Value",
  Xtitle = "Year"
)
forecast::accuracy(ets_forecast, test_freight)
      