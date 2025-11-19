#BF Review

setwd("D:/UNH/SEMESTER 3/Business Forecastiing/class12")
options(digits = 3, scipen = 9999, stringAsFactors = FALSE)
remove(list = ls())
graphics.off()

#' A forecasting ensemble combines multiple different models to produce a 
#' single, more robust forecast. 
#' This approach leverages the "wisdom of the crowd" to improve overall 
#' accuracy and reduce the risk of a single model's weaknesses 
#' causing a poor prediction. 

#=======================#  
suppressPackageStartupMessages({
suppressWarnings({
    library(tidyverse)
    library(lubridate)
    library(ggthemes)
    library(tsbox) #convert xts to tsibble
    library(TSstudio)
    library(pdfetch)
  
    library(tsutils)
    library(rio)
    library(forecast)
    library(quantmod)
  install.packages("forecastHybrid")
    library(forecastHybrid)
  library(vars)
    library(randomForest)
    library(e1071)  #for SVMs

      #devtools::install_github("Techtonique/ahead")
  install.packages("ahead")
      library(ahead)
  
})})
#=======================#`  
# Today
    # Models in your toolkit
      # Seasonal Naive
      # Thetaf
      # ETS
      # Holt Winters and Double Seasonal Holt Winters
      # TBATS
      # ARIMA
      # NNETAR
      # Ensemble
      # VAR
    
#=======================#`  
              #DATA ============  
              
      # All Employees: Financial Activities: Finance and Insurance in Connecticut
      getSymbols("CTNA", src = "FRED") # Performance Indicator
          myxrct= CTNA
                
              myxrct = pdfetch_FRED("CTNA")  # alternatively
      ts_info(myxrct)
              
      ts_plot(myxrct)
              
              #' COVID was unforeseen; lets work with a smoother.
              ## pick out a smoother ======
              xrct = zoo::rollmean(myxrct, k = 24, align = "right") # 12 month trailing
              ts_info(xrct)
                autoplot(myxrct)
                autoplot(xrct)
              ts_plot(cbind(myxrct, xrct))
              
                          
                #Split into testing and training
                          xrct = na.omit(xrct)
                          ts_info(xrct)
                    
                myxrct.ts = ts(xrct, start = c(1991,12), frequency = 12)
                ts_info(myxrct.ts)
                XR_split = ts_split(myxrct.ts)
                      ts_info(XR_split$train); 
                      ts_info(XR_split$test)
                
                      training = XR_split$train
                      testing = XR_split$test
                    
                autoplot(training) + autolayer(testing) 
          
              
        # Model 1: Naive & Seasonal Naive ============================= 
          model_naive =  naive(training, h=length(testing))
          autoplot(model_naive) 
          autoplot(training) + autolayer(testing) + autolayer(model_naive$mean)
          autoplot(training) + autolayer(testing) + autolayer(model_naive) + 
            autolayer(model_naive$mean)
          
                forecast::accuracy(model_naive)
                
        model_snaive =  snaive(training, h=length(testing))
        autoplot(model_snaive)
          forecast::accuracy(model_snaive)
          autoplot(myxrct.ts, col = "darkred") + autolayer(model_snaive)
          autoplot(training, col = "darkred") + autolayer(model_snaive)
          autoplot(training, col = "darkred") + autolayer(testing) + 
            autolayer(model_snaive$mean)+theme(legend.position = "")
    
    # Model 2: Thetaf  (akin to simple exponential smoothing with drift)  ==============
    # this is a variant of Holt Winters; the difference is that the is not simply
    # a linear trend; rather it smoothes off as it increases.
          
    model_thetaf =  forecast::thetaf(training, h=length(testing))
          
                  summary(model_thetaf)
                  forecast::accuracy(model_thetaf)
                  autoplot(model_thetaf)
                  autoplot(myxrct.ts,col = "darkred") + autolayer(model_thetaf)
                  autoplot(training,col = "darkred") + autolayer(model_thetaf$mean)
                  autoplot(myxrct.ts,col = "darkred") + 
                  autolayer(model_thetaf$mean)+theme(legend.position = "")
            
        #Model 3: ETS ============================
        model_ets = ets(training)
        summary(model_ets)
        forecast::accuracy(model_ets)
        ets_fc = forecast.ets(model_ets, h = length(testing))
        autoplot(ets_fc)
        autoplot(ets_fc) + autolayer(myxrct.ts, color = "darkred") 
        autoplot(myxrct.ts) + autolayer(ets_fc$mean, color = "darkred") 
        autoplot(ets_fc$mean) + autolayer(myxrct.ts, color = "darkred") +
          theme(legend.position = "")
        
        
                #Model 4: Holt Winters ===================
                  model_hw = hw(training, h=length(testing))
                         
                         forecast::accuracy(model_hw)
                         hw_fc = forecast(model_hw, h = length(testing))
                         autoplot(hw_fc)
                         autoplot(myxrct.ts, col = "darkred") + 
                           autolayer(hw_fc$mean)+theme(legend.position = "")
                         
     #Model 5: TBATS ====================== 
     #Trigonometric Seasonality
     #Box-Cox Transformation
     #ARIMA Errors
     #Trend
     #Seasonal Components
                         
       #' TBATS is designed to handle complex time series data with multiple 
       #' seasonal patterns, such as daily data with both weekly 
       #' and annual cycles.                         
                         
     model_tbats = tbats(training)
       
     forecast::accuracy(model_tbats)
     tbats_fc = forecast(model_tbats, h = length(testing))
     autoplot(tbats_fc)
     autoplot(training, col = "darkred") + 
       autolayer(tbats_fc)
     autoplot(myxrct.ts, col = "darkred") + 
       autolayer(tbats_fc$mean)+theme(legend.position = "")
               
                             #Model 6: ARIMA ================= 
                             model_arima = auto.arima(training)
                             summary(model_arima)
                             forecast::accuracy(model_arima)
                             arima_fc = forecast(model_arima, h = length(testing))
                             autoplot(arima_fc)
                             autoplot(myxrct.ts, col = "darkblue") + 
                               autolayer(arima_fc$mean)
                             autoplot(arima_fc$mean, col = "darkblue") + 
                               autolayer(myxrct.ts)+theme(legend.position = "")
               
               #Model 7: NNETAR =============================
                             #' Neural Nets (with 1 layer)
               model_nnetar = forecast::nnetar(training)
                forecast::accuracy(model_nnetar)
               nnetar_fc = forecast(model_nnetar, h = length(testing))
                autoplot(nnetar_fc)
               autoplot(myxrct.ts, col = "darkred") + 
                 autolayer(nnetar_fc)
               autoplot(myxrct.ts, col = "darkred") + 
                 autolayer(nnetar_fc$mean)
               autoplot(nnetar_fc$mean, col = "darkred") + 
                 autolayer(myxrct.ts)+theme(legend.position = "")
               
               #Model 8: Ensemble  ===================
               model_hybrid = forecastHybrid::hybridModel(training)
               
      #' models to use: a (auto.arima), e (ets), f (thetam), n (nnetar), 
      #' s (stlm), t (tbats), 
      #' and z (snaive).
               
      model_hybrid = forecastHybrid::hybridModel(training,
                                                     models = "nts",
                                                         weights = "equal",
                                                             errorMethod = "RMSE")
               
               forecast::accuracy(model_hybrid)
               modelhybrid_fc = forecast(model_hybrid, h = length(testing))
               autoplot(modelhybrid_fc)
               autoplot(myxrct.ts, col = "darkred") + 
                 autolayer(modelhybrid_fc) 
               autoplot(myxrct.ts) + 
                 autolayer(modelhybrid_fc$mean)+theme(legend.position = "") 
               
              
               # ======================================= #
               # Using Machine Learning Algorithms        
               #Model 9: Random Forests via the package Ahead ============
               install.packages("ahead")
               library(ahead)
               find.package("ahead")
               
               myrf = ahead::dynrmf(training, h=length(testing), 
                            fit_func = randomForest::randomForest, 
                            predict_func = predict)
              
              autoplot(myrf)
              autoplot(myrf$mean) + autolayer(training) + autolayer(testing)
              
              forecast::accuracy(myrf)
              
               #Model 9: Support Vector Machine via the package Ahead ===============
               # and a 95% prediction interval
              
              mysvm = ahead::dynrmf(training, h=length(testing),
                     fit_func = e1071::svm,
                     predict_func = predict)
          
               autoplot(mysvm)
               autoplot(training) + autolayer(mysvm$mean) + autolayer(testing)
              
               forecast::accuracy(mysvm) 
  #===================================================================== #             
               
  #===================================================================== #             
   # the package ahead also has VAR capabilities
   # Examine the relationship between Real XR  and Employment again
  
               # Vector AutoRegression (VAR) ===========================
             
      
      getSymbols(c("RTWVDCT684NMFRBDAL","CTNA"), src = "FRED")
      rwct = RTWVDCT684NMFRBDAL
      emp = CTNA
      
                        rwct = pdfetch_FRED("RTWVDCT684NMFRBDAL")
                        emp = pdfetch_FRED("CTNA")
                       ts_info(rwct); ts_info(emp)
                      
      rwct.ts = ts(rwct, start = c(1988,1), frequency = 12)
      rwct.ts = window(rwct.ts, start = c(1990,1), frequency = 12)
      
      emp.ts = ts(emp, start = c(1990,1), frequency = 12)
      emp.ts = window(emp.ts, start = c(1990,1), 
                      end = c(2023,6), 
                      frequency = 12)
               
      ts_info(rwct.ts)
      ts_info(emp.ts)
               
               mod_data = cbind(rwct.ts, emp.ts)
               head(mod_data)
               dim(mod_data)
               
               mod_data = na.omit(mod_data)
               
               vars::VARselect(mod_data, lag.max = 6)
               
        my_VAR <- ahead::varf(mod_data, lags = 3,
                                          h = 24, level = 95)
               
               # Plotting forecasts 
               plot(my_VAR$x)
               plot(my_VAR$x[,"rwct.ts"],xlim = c(2020,2026))
               lines(my_VAR$mean[,"rwct.ts"], col = "darkred") 
               
               plot(my_VAR$x[,"emp.ts"], xlim = c(2020,2026))
               lines(my_VAR$mean[,"emp.ts"], col = "darkred") 
               
  autoplot(my_VAR$x[,"rwct.ts"]) + 
    autolayer(my_VAR$mean[,"rwct.ts"]) + 
          xlim(2020, 2026) +
              theme(legend.position = "") + 
                  labs(title = "CT Exports")
  
  autoplot(my_VAR$x[,"emp.ts"]) + 
    autolayer(my_VAR$mean[,"emp.ts"]) + 
          xlim(2020, 2026) +
              theme(legend.position = "") +
                labs(title = "CT Employment") 
               
               
   autoplot(my_VAR$x[,"rwct.ts"]) + autolayer(my_VAR$mean[,"rwct.ts"]) +
        autolayer(my_VAR$x[,"emp.ts"]) + autolayer(my_VAR$mean[,"emp.ts"]) +
                 xlim(2020, 2026) +
                 theme(legend.position = "")
               
  p1 = autoplot(my_VAR$x[,"rwct.ts"]) + 
              autolayer(my_VAR$mean[,"rwct.ts"]) + 
                  xlim(2020, 2026) + theme(legend.position = "") + 
                      labs(title = "CT Exports")
  p2 = autoplot(my_VAR$x[,"emp.ts"]) + 
              autolayer(my_VAR$mean[,"emp.ts"]) + 
                  xlim(2020, 2026) + theme(legend.position = "")+ 
                      labs(title = "CT Employment")
               
               cowplot::plot_grid(p1, p2, ncol = 1)
               
               
               
              # how did the forecast fare?
               myctna = ts_ts(CTNA)
               p2 + autolayer(myctna)
               
               
               # ========================================== #
               #Old School VAR =======================
               library(vars)
               
               vars::VARselect(mod_data, lag.max = 6)
               
               my_old_VAR <- vars::VAR(mod_data, 3)
               
               
               ##Granger Cause =============
               bv.cause.rwct <- vars::causality(my_old_VAR, cause = "rwct.ts")
               bv.cause.rwct
               
               bv.cause.emp <- vars::causality(my_old_VAR, cause = "emp.ts")
               bv.cause.emp
               
               irf.emp <- vars::irf(my_old_VAR, 
                                    impulse = "rwct.ts", 
                                    response = "emp.ts", 
                              n.ahead = 24, boot = TRUE)
               plot(irf.emp, ylab = "ouput", 
               main = "Impact on Employment from Positive Shock in RWXR (Performance)")
               
               
               irf.rwct <- vars::irf(my_old_VAR, 
                                     impulse = "emp.ts", 
                                     response = "rwct.ts", 
                              n.ahead = 24, 
                              boot = TRUE)
               
               plot(irf.rwct, ylab = "ouput", 
               main = "Impact on RWXR from Positive Shock in Employment")
               
               
               predictions <- predict(my_old_VAR, n.ahead = 24, ci = 0.95)
               
               plot(predictions, names = "emp.ts")
               plot(predictions, names = "rwct.ts")
               
            
               
               # QED ==================================
               #===================================================================== #             
               
              
               
               
               