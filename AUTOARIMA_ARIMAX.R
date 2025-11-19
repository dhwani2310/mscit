#ARIMAX
# AutoArima and Arimax

options(digits = 3, scipen = 9999)
remove(list = ls())
graphics.off()
setwd("D:/UNH/SEMESTER 3/Business Forecastiing/class9") 
#REV: 10.25.25

            library(tidyverse)
            library(lubridate)
            library(ggthemes)
            library(tsbox) #convert xts to tsibble
            library(TSstudio)
            library(tsutils)
            library(rio)
            library(quantmod)
            library(forecast)
            library(pdfetch)
install.packages("janitor")
            library(janitor)
            library(smooth)

# Introduction ================= 

# An Autoregressive Integrated Moving Average model (ARIMA) - is, at its core,
# a multiple regression model with autoregressive (AR) terms and one or more 
# moving average (MA) terms.

# When an exogenous (independent) explanatory variable is added to the standard 
# ARIMA model the model is known as ARIMAX.


# Data ==================
getSymbols(                     # personal consumption expenditures &
  c("PCE","DSPI"),              # disposable personal income
  src = "FRED", 
  freq = "monthly",
  return.class = "xts",
  #from = "2010-01-01",
  to = Sys.Date()
)

      pce = PCE
      dspi = DSPI

                    #alternatively
                    pce = pdfetch::pdfetch_FRED("PCE")
                    dspi = pdfetch::pdfetch_FRED("DSPI")
      
                  
                ts_info(pce); ts_info(dspi)
                
                plot(cbind(pce, dspi))

pce.ts = ts(pce, start = c(1959,1), frequency = 12)
dspi.ts = ts(dspi, start = c(1959,1), frequency = 12)
                ts_info(pce.ts); ts_info(dspi.ts)
                
                autoplot(pce.ts) + autolayer(dspi.ts)
                autoplot(cbind(pce.ts, dspi.ts))
                
                ts_plot(cbind(pce.ts, dspi.ts))
    
                
                #=======================#
                
                # different ggplot visual
                usdat = broom::tidy(cbind(pce, dspi))  # notice the tidy format
                head(usdat)
                usdat   =     broom::tidy(cbind(pce.ts, dspi.ts))
                
                usdat  %>% ggplot(aes(x = index, y = value, col = series))+
                  geom_line(linewidth = 1.4)
                
                usdat_df = cbind(pce, dspi)  #
                usdat_df = cbind(pce.ts, dspi.ts)  #
                apply(usdat_df, 2, ndiffs)  # how much differentiation?
                
                            
# =================================================== #
# Here we attempt to forecast personal consumption expenditures
# First with an arima model.
# Then via an arimax model relying on the relationship between 
# disposible personal income
# and personal consumption expenditures. presumably, the positive 
# relationship between these two will enhance predictive capabilities 
# via ARIMAX using disposable income as an independent predictor.

    # ARIMA==============
    fit1 = auto.arima(pce)  # but pre-treatment is not necessary
    autoplot(forecast(fit1, 24)) + xlim(750,800) + ylim(15000, 22000)
    checkresiduals(fit1)
    
    autoplot(forecast(auto.arima(pce.ts)))
    autoplot(forecast(auto.arima(pce.ts))) + xlim(2022,2027) + ylim(16000,22000)
    
          # ARIMAX========
          #Using disposable personal income as a predictor
          #fiting an ARIMAX model
          # two  year horizon
        fit2 = auto.arima(pce.ts, 
                            xreg = dspi.ts)    
        
          fit2
          
          length(dspi.ts)
          548-24
        mypred = forecast(fit2, xreg = dspi.ts[501:524] )
        mypred = forecast(fit2, xreg = rep(mean(dspi.ts),24))
              
          autoplot(pce.ts) + autolayer(mypred, PI = FALSE) + 
            xlim(2020, 2027) + ylim(5000,21000)
          
      
      # splitting into train and test to examine performance of ARIMA vs ARIMAX========
      pce_split = ts_split(pce.ts)
      autoplot(pce_split$train, linewidth = 1.6) + 
        autolayer(pce_split$test, linewidth = 1.6) 
      
      mod_train = auto.arima(pce_split$train)
          length(pce_split$test)
          length(pce_split$train)
      myforecast = forecast(mod_train, 240)
          str(myforecast)
          length(myforecast$fitted)
          length(myforecast$mean)
          
      forecast::accuracy(myforecast$mean, pce_split$test) # ARIMA Accuracy
      
      autoplot(myforecast)
      autoplot(myforecast, PI = FALSE) + autolayer(pce_split$test)
      autoplot(myforecast) + autolayer(pce_split$test)
      
      
      autoplot(pce_split$train, linewidth = 1.5) + 
      autolayer(myforecast$fitted, color = "purple") + 
        autolayer(pce_split$test, linewidth = 1.5)+ 
      autolayer(myforecast$mean, color = "darkgreen")
      
      
      autoplot(pce_split$train) + 
          autolayer(pce_split$test) + 
              autolayer(myforecast$mean) 
  
  # Now arimax        
      head(usdat_df)
      length(dspi.ts)
      length(pce_split$train)
      length(pce_split$test)
      
      dspi_split = ts_split(dspi.ts)
            length(dspi_split$train)
            length(dspi_split$test)
            
  mod_train_x = auto.arima(pce_split$train, xreg = dspi_split$train)
        
  myforecast = forecast(mod_train_x, 240, xreg =dspi_split$test )
  forecast::accuracy(myforecast, pce_split$test)
  forecast::accuracy(myforecast$mean, pce_split$test)
  autoplot(myforecast, PI = FALSE) + autolayer(pce_split$test)
      
        #======================================================= #
    # ========================================= #
    # Exercise ==========
    # Add the Michigan Sentiment Index to Consumer Spending above.
    # Does the model improve?
    
  getSymbols( "UMCSENT",             
    src = "FRED", 
    freq = "monthly",
    return.class = "xts",
    #from = "2010-01-01",
    to = Sys.Date()
  )
  
  sent = UMCSENT
  
      sent = pdfetch::pdfetch_FRED("UMCSENT")
  
      plot(sent)
      ts_info(sent)
      psych::headTail(sent)
  
  
  sent.ts = ts(sent, start = c(1952,11), frequency = 12)
        ts_info(sent.ts)
    
    autoplot(cbind(sent.ts, pce.ts)) + autolayer(dspi.ts)
    
    
    sent.ts = window(sent.ts, start = c(1980,1), end = c(2025, 8), frequency = 12)
    pce.ts = window(pce.ts, start = c(1980,1), end = c(2025, 8), frequency = 12)
    dspi.ts = window(dspi.ts, start = c(1980,1), end = c(2025, 8), frequency = 12)
    
    
    sent_split = ts_split(sent.ts)
        length(sent_split$train)
        length(sent_split$test)
    pce_split = ts_split(pce.ts)
    dspi_split = ts_split(dspi.ts)    
        length(pce_split$train)
        length(dspi_split$train)
        
    mod_train_x = auto.arima(pce_split$train, 
                             xreg = as.matrix(dspi_split$train, sent_split$train))
    
    myforecast = forecast(mod_train_x, 164, xreg =as.matrix(dspi_split$test, sent_split$test ))
    
    forecast::accuracy(myforecast$mean, pce_split$test)
    autoplot(myforecast) + autolayer(pce_split$test)
    
    
    # Exponential (ETS) Smoothing
    # lets go back to Holt Winters (i.e. es()), 
    # this time we use the algo in the package smooth
    
    pacman::p_load("smooth")
    
        length(pce_split$test)
        length(pce_split$train)
    
  myes = smooth::es(pce_split$train, xreg = dspi_split$train, h = 164)
  
      names(myes)
        myes$fitted
          myes$forecast
          
    autoplot(myes$fitted, size = 2) + autolayer(myes$forecast) + 
      autolayer(pce_split$train) + autolayer(pce_split$test)
    
    forecast::accuracy(myes$forecast, pce_split$test)
    
    # ===================  #

    # HW: ARIMAX
    #' Add a third predictor to the PCE forecasting exercise.  
    #' eg, the price of oil (West Texas Intermediate); or the Cass shipments index (FRGSHPUSM649NCIS);
    #' Create a graph including the forecast plus the testing portion of pce.
    #' 
    #' Put the graph in the bucket labeled ARIMAX by Friday November 7, cob. 
    
      # ========================================= #
    
   
    
    #==========  #
    
    # ============================================
    # HW: ARIMAX with Three Predictors
    # Forecast PCE using DSPI, UMCSENT, and Oil Prices (WTI)
    # Due: Friday, November 7
    # ============================================
    
    options(digits = 3, scipen = 9999)
    remove(list = ls())
    graphics.off()
    
    library(tidyverse)
    library(lubridate)
    library(forecast)
    library(pdfetch)
    library(TSstudio)
    library(tsutils)
    library(smooth)
    library(ggthemes)
    library(tsbox)
    
    # ============================================
    # Load Data from FRED
    # ============================================
    
    getSymbols(c("PCE", "DSPI", "UMCSENT", "WTISPLC"), 
               src = "FRED", return.class = "xts")
    
    pce  <- PCE
    dspi <- DSPI
    sent <- UMCSENT
    oil  <- WTISPLC   # West Texas Intermediate Crude Oil Price (USD/barrel)
    
    # Convert to time series objects
    pce.ts  <- ts(pce,  start = c(1959, 1), frequency = 12)
    dspi.ts <- ts(dspi, start = c(1959, 1), frequency = 12)
    sent.ts <- ts(sent, start = c(1978, 1), frequency = 12)
    oil.ts  <- ts(oil,  start = c(1986, 1), frequency = 12)
    
    # Align time windows (use 1986–2025 to cover all)
    pce.ts  <- window(pce.ts,  start = c(1986, 1), end = c(2025, 8))
    dspi.ts <- window(dspi.ts, start = c(1986, 1), end = c(2025, 8))
    sent.ts <- window(sent.ts, start = c(1986, 1), end = c(2025, 8))
    oil.ts  <- window(oil.ts,  start = c(1986, 1), end = c(2025, 8))
    
    # ============================================
    # Split Data into Train/Test
    # ============================================
    
    pce_split  <- ts_split(pce.ts)
    dspi_split <- ts_split(dspi.ts)
    sent_split <- ts_split(sent.ts)
    oil_split  <- ts_split(oil.ts)
    
    # ============================================
    # Fit ARIMAX Model with 3 Predictors
    # ============================================
    
    # Combine predictors into a matrix
    x_train <- cbind(dspi_split$train, sent_split$train, oil_split$train)
    x_test  <- cbind(dspi_split$test,  sent_split$test,  oil_split$test)
    
    # Fit ARIMAX
    mod_train_x <- auto.arima(pce_split$train, xreg = x_train)
    summary(mod_train_x)
    
    # Forecast using test data
    h <- length(pce_split$test)
    mypred <- forecast(mod_train_x, h = h, xreg = x_test)
    
    # ============================================
    # Accuracy
    # ============================================
    
    forecast::accuracy(mypred$mean, pce_split$test)
    
    # ============================================
    # Visualization
    # ============================================
    
    autoplot(pce_split$train, size = 1.1) +
      autolayer(pce_split$test, size = 1.1, linetype = "dashed") +
      autolayer(mypred$mean, size = 1.1) +
      labs(title = "ARIMAX Forecast of Personal Consumption Expenditures (PCE)",
           subtitle = "Predictors: Disposable Income (DSPI), Consumer Sentiment (UMCSENT), Oil Price (WTI)",
           y = "Billions of Dollars",
           x = "Year") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
    

    
    
    
    # ============================================
    # Optional: Compare ARIMA vs ARIMAX Accuracy
    # ============================================
    
    fit_arima <- auto.arima(pce_split$train)
    base_pred <- forecast(fit_arima, h = h)
    accuracy(base_pred$mean, pce_split$test)
    accuracy(mypred$mean,  pce_split$test)
    
    # ============================================
    # End of HW
    # Save Plot or Results if Needed
    # ============================================
    
    
    
    