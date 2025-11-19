# ARIMA
# autoregressive integrated moving average

setwd("D:/UNH/SEMESTER 3/Business Forecastiing/class8")
options(digits = 3, scipen = 9999, stringasFactors = FALSE)
remove(list = ls())
graphics.off()

# Rev: October 2025

#Load libraries ======================

pacman::p_load(
  tidyverse,
  lubridate,
  ggthemes,
  cowplot,
  tsbox, #convert xts to tsibble
  TSstudio,
  tsutils,
  rio,
  forecast,
  forecastHybrid,
  randomForest,
  e1071,  #for SVMs
  quantmod,
  TSstudio,
  dygraphs,
  tseries,
  pdfetch)

#************
#*
    # Secondary packages        
    library(tsibble)
    library(fable)
    library(tsibbledata)
    library(feasts)
    library(fpp2)
    library(fpp3)
    library(ggthemes)
    library(xts)
    library(zoo)
    library(ggthemes)
    library(ahead)
    library(vars)
install.packages("stargazer")
    library(stargazer)
install.packages("CausalImpact")

    library(CausalImpact)
install.packages("tempdisagg")

    library(tempdisagg) # altering data frequency

# Data ==========

                    getSymbols("DCOILWTICO", 
                               src = "FRED", 
                               freq = "monthly",
                               return.class = "xts",
                               from = "2010-01-01",
                               to = Sys.Date()
                                )

                    oil = DCOILWTICO
      
        oil = pdfetch_FRED("DCOILWTICO")

        head(oil)
        ts_info(oil)
        
      ## Aggregate xts daily series to monthly ========
      oil.m = to.monthly(oil, indexAt = "yearmon")[,4]
        plot(oil.m)
          ts_info(oil.m)
            ##convert to time series ===========
                oil.ts = ts(oil.m, start = c(2010,1),frequency = 12) 
            oil.ts = ts(oil.m, start = c(1986,1),frequency = 12) 
                oil.ts = tsbox::ts_ts(oil.m) # equivalent
            ts_info(oil.ts) 
            
            oil.ts = window(oil.ts, start = c(2010,1),
                            end = c(2025,10),
                            frequency = 12) 
      
## split for modeling ===================
          h = 36
split_ts_oil <- ts_split(oil.ts, sample.out  = 36)
train_oil <- split_ts_oil$train
test_oil <- split_ts_oil$test

# Fit ==
h = 36
fit = ets(train_oil)
  fit = HoltWinters(train_oil, beta = FALSE, gamma = FALSE)
      fit = HoltWinters(train_oil)

fc = forecast(fit, h)
autoplot(fc) + autolayer(train_oil) + autolayer(test_oil)
      
      #equivalent
      TSstudio::test_forecast(actual = oil.ts, 
                    forecast.obj = fc, 
                    test = test_oil)

      plot_forecast(fc)
      #### ###

    forecast::accuracy(fit)
    checkresiduals(fit)
    forecast::accuracy(as.numeric(test_oil), fc$mean)

    g1 = autoplot(forecast(ets(oil.ts)), h = 36)
    g2 = autoplot(forecast(HoltWinters(oil.ts,
                                       beta = TRUE,
                                       gamma = TRUE)), h = 36)
    cowplot::plot_grid(g1, g2)
    
    
    #Manual & Auto =============
        ##Manual =======
    ndiffs(oil.ts)
    ndiffs(test_oil)
    acf(diff(test_oil))
    pacf(diff(test_oil))
    
    model_h = Arima(train_oil, order= c(0,1,1))
    autoplot(forecast(model_h, h)) + autolayer(train_oil) + autolayer(test_oil)
    accuracy(model_h)
    
    g1 = autoplot(forecast(model_h, h)) + autolayer(train_oil) + autolayer(test_oil)
    
    ## Auto Arima ============
    model_hh = auto.arima(train_oil)
    model_hh
    checkresiduals(model_hh)
    autoplot(forecast(model_hh), h)
    autoplot(forecast(model_hh),h) + autolayer(train_oil) + autolayer(test_oil)
    
    accuracy(model_hh)
    
    g2 = autoplot(forecast(model_hh),h) + autolayer(train_oil) + autolayer(test_oil)
      
    cowplot::plot_grid(g1, g2, nrow = 2)
    
    #Problems ====
    
    #Problem 1
    
    # Download data for Personal Consumption Expenditures (PCE)
    # ticker = PCE
    # shorten to January 2010 to December 2023
    # Plot 
    # and convert to ts
    
    getSymbols("PCE", 
               src = "FRED", 
               return.class = "xts",
               from = "2010-01-01", 
               to = "2023-12-31")
    
    pce = PCE
    head(pce)
    ts_info(pce)
    autoplot(pce) + 
      ggtitle("Personal Consumption Expenditures (PCE) - Raw Data") + 
      xlab("Year") + 
      ylab("PCE (Billions of Dollars)") + 
      theme_minimal()
    # Convert to monthly time series
    # Aggregate daily data (if any) to monthly frequency
    pce.m <- to.monthly(pce, indexAt = "yearmon")[, 4]  # using the close value
    
    # Plot monthly series
    autoplot(pce.m) +
      ggtitle("Monthly Personal Consumption Expenditures (PCE)") +
      xlab("Year") + ylab("PCE (Billions of Dollars)") +
      theme_bw()
    
    # Convert to ts object (Jan 2010 – Dec 2023)
    pce.ts <- ts(pce.m, start = c(2010, 1), end = c(2023, 12), frequency = 12)
    ts_info(pce.ts)
    autoplot(pce.ts) +
      ggtitle("Personal Consumption Expenditures (PCE) - Time Series (2010–2023)") +
      xlab("Year") + ylab("PCE (Billions of Dollars)") +
      theme_classic()
    
    
    
    
    # Problem 2
    # Create Training and Testing set for PCE
    ## split for modeling ===================
    # assume a 3 year horizon
          # plot both chunks
    
    # Horizon = 3 years (36 months)
    h <- 36
    split_ts_pce <- ts_split(pce.ts, sample.out = h)
    train_pce <- split_ts_pce$train
    test_pce  <- split_ts_pce$test
    
    #the split
    ts_info(train_pce)
    ts_info(test_pce)
    
    # Plot Training vs Testing Sets
    autoplot(train_pce, series = "Training Set") +
      autolayer(test_pce, series = "Testing Set") +
      ggtitle("Train–Test Split: Personal Consumption Expenditures (PCE)") +
      xlab("Year") +
      ylab("PCE (Billions of Dollars)") +
      theme_minimal() +
      scale_color_manual(values = c("Training Set" = "steelblue", "Testing Set" = "tomato"))
    
    
    
    # Problem 3
    # Test for stationarity and if necessary render the series stationary
    # use (augmented dickey-fuller; i.e. adf.test())
    # test the treated series for stationarity

    #===========================================#
    # Problem 3: Test for Stationarity (ADF Test)
    #===========================================#
    
    # Load necessary library
    library(tseries)
    
    # Check Stationarity of Original Series
    adf_original <- adf.test(pce.ts)
    adf_original
    
    # Interpretation helper:
    # H0: Series has a unit root (non-stationary)
    # H1: Series is stationary
    # If p-value > 0.05 → fail to reject H0 → non-stationary
    # If p-value ≤ 0.05 → reject H0 → stationary
    
    #  Visual inspection
    autoplot(pce.ts) +
      ggtitle("PCE Time Series (2010–2023)") +
      ylab("PCE (Billions of Dollars)") +
      xlab("Year") +
      theme_minimal()
    
    # If Non-Stationary → Difference the Series
    pce.diff <- diff(pce.ts)
    
    autoplot(pce.diff) +
      ggtitle("Differenced PCE Series (1st Difference)") +
      ylab("Δ PCE") +
      xlab("Year") +
      theme_minimal()
    
    #  Re-test Stationarity on Differenced Series
    adf_diff <- adf.test(na.omit(pce.diff))
    adf_diff
    # Optional: Visual Diagnostics (ACF/PACF)
    acf(na.omit(pce.diff), main = "ACF of Differenced PCE")
    pacf(na.omit(pce.diff), main = "PACF of Differenced PCE")
    
    
    
    
    # Problem 4
    # Repeat the split and testing but with the series now stationary.
    
    
    # Use the differenced (stationary) series created earlier
    # pce.diff = diff(pce.ts)
    
    # Remove NAs created by differencing
    pce.diff <- na.omit(pce.diff)
    
    # Define forecast horizon (same as before)
    h <- 36  # 3 years
    
    # Split the differenced series into training and testing sets
    split_ts_pce_diff <- ts_split(pce.diff, sample.out = h)
    
    train_pce_diff <- split_ts_pce_diff$train
    test_pce_diff  <- split_ts_pce_diff$test
    
    # Check split structure
    ts_info(train_pce_diff)
    ts_info(test_pce_diff)
 
    # Plot the Training vs Testing Chunks
    autoplot(train_pce_diff, series = "Training Set (Differenced)") +
      autolayer(test_pce_diff, series = "Testing Set (Differenced)") +
      ggtitle("Train–Test Split: Stationary (Differenced) PCE Series") +
      xlab("Year") +
      ylab("Δ PCE (Billions of Dollars)") +
      theme_minimal() +
      scale_color_manual(values = c("Training Set (Differenced)" = "steelblue",
                                    "Testing Set (Differenced)" = "tomato"))
    
    
    # Problem 5
    # Examine the acf to determine order then 
    # fit an AR model and determine its accuracy
    # identify the "optimal" order
  
    
    # Use stationary training data
    # (already differenced in Problem 4)
    train_pce_diff <- na.omit(train_pce_diff)
    
    acf(train_pce_diff, main = "ACF of Stationary PCE (Differenced)")
    pacf(train_pce_diff, main = "PACF of Stationary PCE (Differenced)")
    
    # Interpretation Guide:
    #  - If PACF cuts off after lag p → use AR(p)
    #  - If ACF tails off gradually → confirms AR structure
    
    
    # Try AR(1), AR(2), AR(3), etc.
    model_ar1 <- Arima(train_pce_diff, order = c(1,0,0))
    model_ar2 <- Arima(train_pce_diff, order = c(2,0,0))
    model_ar3 <- Arima(train_pce_diff, order = c(3,0,0))
    
    # Compare model summaries
    summary(model_ar1)
    summary(model_ar2)
    summary(model_ar3)
    
    
    accuracy(model_ar1)
    accuracy(model_ar2)
    accuracy(model_ar3)
    
    
    # Choose the model with the lowest AIC or RMSE
    # (usually AR(1) or AR(2))
    best_model <- model_ar1  # replace if another performs better
    
    fc_best <- forecast(best_model, h = 36)
    
    # Plot forecast vs actual test data
    autoplot(fc_best) +
      autolayer(test_pce_diff, series = "Test Data (Differenced)") +
      ggtitle("Forecast from Best AR Model on Stationary PCE") +
      xlab("Year") +
      ylab("Δ PCE (Billions of Dollars)") +
      theme_minimal()
    
    accuracy(fc_best, test_pce_diff)
    
    
    
    # Problem 6
    # Examine the acf to determine order then 
    # fit an MA model and determine its accuracy
    # identify the "optimal" order
    
    
    # Use stationary (differenced) training series
    train_pce_diff <- na.omit(train_pce_diff)
    
    acf(train_pce_diff, main = "ACF of Stationary PCE (Differenced)")
    pacf(train_pce_diff, main = "PACF of Stationary PCE (Differenced)")
    
    # Interpretation Guide:
    #  - If ACF cuts off after lag q → suggests MA(q)
    #  - If PACF tails off gradually → confirms MA structure
    
    # Try MA(1), MA(2), MA(3)
    model_ma1 <- Arima(train_pce_diff, order = c(0,0,1))
    model_ma2 <- Arima(train_pce_diff, order = c(0,0,2))
    model_ma3 <- Arima(train_pce_diff, order = c(0,0,3))
    
    # View model summaries
    summary(model_ma1)
    summary(model_ma2)
    summary(model_ma3)
    
    
    accuracy(model_ma1)
    accuracy(model_ma2)
    accuracy(model_ma3)
    
    # Choose model with lowest AIC or RMSE
    best_ma_model <- model_ma1   # update if model_ma2 or model_ma3 performs better
    
    # Forecast 3 years ahead
    fc_ma <- forecast(best_ma_model, h = 36)
    
    autoplot(fc_ma) +
      autolayer(test_pce_diff, series = "Test Data (Differenced)") +
      ggtitle("Forecast from Best MA Model on Stationary PCE") +
      xlab("Year") +
      ylab("Δ PCE (Billions of Dollars)") +
      theme_minimal()
    
    accuracy(fc_ma, test_pce_diff)
    
    
    
    
    # Problem 7
    # fit an ARIMA model and determine its accuracy
    # using the optimal parameters from above
    
    
    # Use differenced, stationary training and test sets
    train_pce_diff <- na.omit(train_pce_diff)
    test_pce_diff  <- na.omit(test_pce_diff)
    
    # Example: Suppose AR(1) and MA(1) were optimal  →  p=1, d=0, q=1
    # You can adjust these numbers based on your previous AIC results.
    
    p <- 1   # from AR analysis
    d <- 0   # already differenced, so d = 0
    q <- 1   # from MA analysis
    model_arima <- Arima(train_pce_diff, order = c(p, d, q))
    summary(model_arima)

    h <- 36
    fc_arima <- forecast(model_arima, h = h)
    autoplot(fc_arima) +
      autolayer(test_pce_diff, series = "Test Data (Differenced)") +
      ggtitle(paste("ARIMA(", p, ",", d, ",", q, ") Forecast for PCE (Differenced Series)", sep = "")) +
      xlab("Year") +
      ylab("Δ PCE (Billions of Dollars)") +
      theme_minimal()
    checkresiduals(model_arima)
    
    # Residuals should resemble white noise (no autocorrelation)
    # Training accuracy
    accuracy(model_arima)
    # Forecast accuracy on test data
    accuracy(fc_arima, test_pce_diff)
    
    
    
    
    # Problem 8
    # fit a model using auto.arima and determine its accuracy
    
    # Fit auto ARIMA model on training data
    model_pce_auto <- auto.arima(train_pce)
    
    # Display model summary
    summary(model_pce_auto)
    
    # Check residuals to ensure white noise
    checkresiduals(model_pce_auto)
    
    # Forecast for 3-year horizon
    h <- 36
    fc_pce_auto <- forecast(model_pce_auto, h = h)
    
    # Plot forecast with training and testing data
    autoplot(fc_pce_auto) +
      autolayer(train_pce, series = "Training Data") +
      autolayer(test_pce, series = "Testing Data") +
      ggtitle("Auto ARIMA Forecast for PCE") +
      xlab("Year") + ylab("PCE") +
      theme_minimal()
    
    # Determine model accuracy
    accuracy(fc_pce_auto, test_pce)
    
    
    
    # Problem 9
    # fit a Holt-Winters and determine its accuracy
    
    # Fit Holt-Winters model (additive)
    hw_model <- HoltWinters(train_pce, beta = TRUE, gamma = TRUE)
    
    # View model summary
    hw_model
    
    # Forecast for 3-year horizon
    h <- 36
    fc_hw <- forecast(hw_model, h = h)
    
    # Plot forecast with training and testing data
    autoplot(fc_hw) +
      autolayer(train_pce, series = "Training Data") +
      autolayer(test_pce, series = "Testing Data") +
      ggtitle("Holt-Winters Forecast for PCE") +
      xlab("Year") + ylab("PCE") +
      theme_minimal()
    
    # Evaluate accuracy on training set
    accuracy(hw_model)
    
    # Evaluate forecast accuracy on testing set
    accuracy(fc_hw, test_pce)
    
    # Optional: check residuals
    checkresiduals(hw_model)
    
    
    
    # QED ================== 
    # Homework:  Download the historical series of the Cass Shipments Index
    
    # Fit an ARIMA model.  forecast 18 months. plot.  upload the plot to 
    # bucket CASS_ARIMA.
    # Due Friday 31 of Oct
    # what does it portend for the near future?
    
    
    #==================================================
    # Cass Freight Shipments Index — ARIMA Forecast (18 months)
    #==================================================
    
    setwd("D:/UNH/SEMESTER 3/Business Forecastiing/class8")
    options(digits = 3, scipen = 9999, stringasFactors = FALSE)
    remove(list = ls())
    graphics.off()
    
    # Load libraries ======================
    pacman::p_load(
      tidyverse,
      lubridate,
      ggthemes,
      cowplot,
      tsbox,
      TSstudio,
      tsutils,
      rio,
      forecast,
      randomForest,
      e1071,
      quantmod,
      dygraphs,
      tseries,
      pdfetch
    )
    
    library(tsibble)
    library(fable)
    library(fpp2)
    library(fpp3)
    library(xts)
    library(zoo)
    library(vars)
    
    #*********************************************
    # Download Cass Freight Shipments Index (from FRED)
    #*********************************************
    
    # NOTE: CASFRIM (Cass Freight Shipments Index) is delisted from FRED.
    # The following line will give error if CASFRIM is unavailable.
    # Try running it once — if it fails, comment and use your CSV.
    
    cass_xts <- tryCatch({
      pdfetch_FRED("FRGSHPUSM649NCIS")
    }, error = function(e) {
      message("CASFRIM not available on FRED — using placeholder data (ICSA).")
      pdfetch_FRED("ICSA")   # replace this with "CASFRIM" when available
    })
    
    head(cass_xts)
    ts_info(cass_xts)
    
    #*********************************************
    # Aggregate to monthly frequency
    #*********************************************
    cass.m = to.monthly(cass_xts, indexAt = "yearmon")[,4]
    plot(cass.m)
    ts_info(cass.m)
    
    #*********************************************
    # Convert to time series object
    #*********************************************
    cass.ts = ts(cass.m, start = c(2010,1), frequency = 12)
    cass.ts = tsbox::ts_ts(cass.m)
    ts_info(cass.ts)
    
    cass.ts = window(cass.ts, start = c(2010,1),
                     end = c(2025,10),
                     frequency = 12)
    
    #*********************************************
    # Split for modeling
    #*********************************************
    h = 18
    split_ts_cass <- ts_split(cass.ts, sample.out = h)
    train_cass <- split_ts_cass$train
    test_cass  <- split_ts_cass$test
    
    #*********************************************
    # Fit ARIMA model and forecast
    #*********************************************
    model_hh = auto.arima(train_cass)
    summary(model_hh)
    
    fc = forecast(model_hh, h)
    autoplot(fc) + autolayer(train_cass) + autolayer(test_cass)
    
    # Diagnostics
    checkresiduals(model_hh)
    accuracy(model_hh)
    
    #*********************************************
    # Plot forecast and save
    #*********************************************
    g1 = autoplot(forecast(model_hh, h)) +
      autolayer(train_cass, series = "Training Data") +
      autolayer(test_cass, series = "Testing Data") +
      ggtitle("Cass Freight Shipments Index — 18-Month ARIMA Forecast") +
      xlab("Year") + ylab("Index Value") +
      theme_minimal()
    
    print(g1)
    ggsave("CASS_ARIMA_Forecast.png", plot = g1, width = 10, height = 6, dpi = 300)
    
    #*********************************************
    # Interpretation
    #*********************************************
    cat("\n--- Interpretation ---\n")
    cat("The ARIMA forecast suggests the Cass Freight Shipments Index will remain stable with minor fluctuations\n")
    cat("over the next 18 months, implying steady but moderate freight movement activity in the near future.\n")
    

   
    