#Time Series Regression Models

# PRELIMINARIES ======
setwd("D:/UNH/SEMESTER 3/Business Forecastiing/class10")
options(digits = 3, scipen = 9999, stringasFactors = FALSE)
remove(list = ls())
graphics.off()
    # REV: October 2025


    # Intro  ==================== 
    # We are mostly familiar with regression models - deployed with cross-sectional
    # data rather than Time Series data.  
    
    # A cross section of data provides a slice of variables across the same period
    # in time.  
    
    # y = bo + b1x1 + b2x2 + ... + bnXn + e
    
    #A Time series represents the realization of one variable across 
    #  a period time - (constituting multiple observations or rows in a dataframe). 

# Illustrative Example =======================================================

# you are working for a company that advertises across several channels.
# the objective is to increase sales; but the immediate objective is to increase
# page visits (POTS)

# Your task: identify the most important channel determining page visits.

# Packages ==========
suppressPackageStartupMessages({
  suppressMessages({
    library(tidyverse)
    library(forecast)
    library(rio)
    library(lubridate)
    library(pacman)
    library(tsbox)
    library(TSstudio)
    library(vars)
    library(tseries)
    library(tsutils)
    library(reghelper)
    library(dygraphs)
    library(foreign)   
    install.packages("wooldridge")
    library(wooldridge)
    library(fpp3)
  })
})
#=================================== #
# Data Input ========================

data.1 <- read.csv("mmm.csv",header = TRUE)
  data.1
    str(data.1)
      psych::headTail(data.1)
        colSums(is.na(data.1))
          dim(data.1)
          
##format data ====
z = data.1 %>% dplyr::select(-1)
        dim(z)
        head(z)
zz = ts(z, start = c(2017,1), frequency = 12)
    str(zz)
      ts_info(zz)
        head(zz,3); tail(zz,3)

##View Pots
autoplot(zz[,"Pots"], series = "Pots")
  ##View All Variables            
  head(zz, 3)

  autoplot(zz, linewidth = 1.2)+
    theme(legend.position = 'bottom')
  
  autoplot(zz[,"Radio.Budget"], series = "Radio", linewidth = 1.2)    +
  autolayer(zz[,"OOH.Budget"], series = "OOH", linewidth = 1.2)    +
  autolayer(zz[,"Television.Budget"], series = "Television", linewidth = 1.2)    +
  autolayer(zz[,"Digital.Budget"], series = "Digital", linewidth = 1.2)    +
  theme(legend.position = 'bottom')

#Multiple Linear Regression   ==============================================
## TSLM: Variable Significance & Variable Importance ====================

mod_z <- lm(Pots~Radio.Budget+OOH.Budget+Television.Budget+Digital.Budget,data = z)
    summary(mod_z)  
        coef(mod_z)
            barplot(mod_z$coefficients)
              barplot(mod_z$coefficients[2:5])
                  barplot(mod_z$coefficients[2:5], horiz = TRUE)
                confint(mod_z)

# repeat after scaling all variables
zzscale = scale(zz) #normalize
head(zzscale)

mod_zzscale <- lm(Pots~ Radio.Budget + 
                            OOH.Budget + 
                                Television.Budget + 
                                    Digital.Budget,
                                              data = zzscale)

      summary(mod_zzscale)
      coef(mod_zzscale)
      barplot(mod_zzscale$coefficients[2:5], horiz = TRUE)

      install.packages("reghelper")
      library(reghelper)
      # using beta from the package reghelper
      myreg = reghelper::beta(mod_z)  # this will give you standardized coefficients
        coefficients(myreg)
          coef(myreg)
            barplot(myreg$coefficients[2:5], 
                    names.arg = names(z)[2:5],
                        horiz = TRUE)
      
            # Machine Learning ====            
            ## Boruta: Variable Importance ==================
            install.packages("Boruta")
            library(Boruta)
            
            # RF
            myzz = as.data.frame(zz)
            head(myzz)
            
            myboruta = Boruta(Pots~., myzz )
            plot(myboruta)
            
          
      #Interestingly
      fit_z = predict(mod_z, z)
      plot(z$Pots, type = "l")
      lines(fit_z, col = "darkred", lwd = 2)

# same for scaled data
matplot(cbind(zzscale[,1], mod_zzscale$fitted.values), type ="l", lwd = 2)

# ============== = 
# Seasonality and all that 

          ts_info(zz)
                mydecomp = stats::decompose(zz)
                plot(mydecomp$seasonal)
                plot(mydecomp$trend)
                
          forecast::ggseasonplot(zz[,"Pots"], year.labels = TRUE)
          forecast::ggseasonplot(zz[,"Digital.Budget"], year.labels = TRUE)
      
          install.packages("scales")
          library(scales)
          ggsubseriesplot(zz[,"Digital.Budget"] )
          ggmonthplot(zz[,"Digital.Budget"])
          
          
      # ==================== = 
      
  mod_z <- tslm(Pots~Radio.Budget+OOH.Budget+Television.Budget+Digital.Budget +
                  trend +
                  season,
                data = zz)
  summary(mod_z)  
    coef(mod_z)    
      barplot(mod_z$coefficients)
          coef(mod_z)
      barplot(mod_z$coefficients[2:16], horiz = TRUE)
      barplot(mod_z$coefficients[2:5])
      barplot(mod_z$coefficients[2:5], horiz = TRUE)

      #PART II: Out of sample forecast ===========
# Create the 24 period-ahead data set
      head(zz)
      str(zz)
      ts_info(zz)
      tail(zz)
      
      #alternatively: old school
      mynewdata = data.frame(Radio.Budget = rep(mean(data.1$Radio.Budget),24),
                             Digital.Budget = rep(mean(data.1$Digital.Budget),24),
                             Television.Budget = rep(mean(data.1$Television.Budget),24),
                             OOH.Budget = rep(mean(data.1$OOH.Budget),24)
      )
      
      mynewdata
      
      install.packages("tsibble")
      library(tsibble)
              # alternatively: new school
              zst = as_tsibble(zz)
              
              mynewdata = tsibble::new_data(zst,6) |>
              dplyr::mutate(Radio.Budget = mean(data.1$Radio.Budget),
                        Digital.Budget = mean(data.1$Digital.Budget),
                        Television.Budget = mean(data.1$Television.Budget),
                        OOH.Budget = mean(data.1$OOH.Budget)
                  )
        
              head(mynewdata,3); tail(mynewdata,3)
              install.packages("forecast")
              library(forecast)

myts = tslm(Pots~ Radio.Budget + 
          OOH.Budget + 
            Television.Budget + 
              Digital.Budget + 
                  trend + 
                      season,
                  data = zz)

              myts = tslm(Pots~ Radio.Budget + 
                        OOH.Budget + 
                          Television.Budget + 
                            Digital.Budget,
                          data = zz)


        str(mynewdata)
        mynewdata = as.data.frame(mynewdata)

fc = forecast::forecast(myts, newdata = mynewdata)
        autoplot(fc)
        autoplot(fc, PI = FALSE)
        install.packages("ggplot2")   # only if not installed yet
        library(ggplot2)
        
        g1 = autoplot(zz[,1]) + autolayer(fc, PI = FALSE)
        g1


        g2 = autoplot(zz[,1]) + autolayer(fc, PI = FALSE) + xlim(2020,2024)
        g2

        cowplot::plot_grid(g1, g2)

        install.packages("fpp3")
        library(fpp3)
          # =============================== #                 
          # Again: Out of Sample Forecast ====
          data("us_change")
            head(us_change,3); tail(us_change)
              dim(us_change) 
                  str(us_change)
      
          head(us_change,3); tail(us_change,3)
          str(us_change)
          
          myts =tsbox::ts_ts(us_change)
          ts_info(myts)
          head(myts)
          
          climatets.lm =  tslm(Consumption ~ Income + 
                                                Production + 
                                                    Unemployment + 
                                                        Savings,
                                                    data = myts)
          
          summary(climatets.lm)
          forecast::accuracy(climatets.lm)
          climatets.lm$fitted.values
          autoplot(climatets.lm$fitted.values) + autolayer(myts[,1])
          
          
          climatets.lm =  tslm(Consumption ~ Income + 
                                 Production + 
                                    Unemployment + 
                                      Savings + 
                                          trend + 
                                            season,
                                              data = myts)
          summary(climatets.lm)
          forecast::accuracy(climatets.lm)
          climatets.lm$fitted.values
          autoplot(climatets.lm$fitted.values) + autolayer(myts[,1])
          
          # Out of Sample
          # Create the period-ahead data set
          mynewdata = tsibble::new_data(us_change,4) |>
            dplyr::mutate(Income = 1,
                          Production = 1,
                          Savings = 0.5,
                          Unemployment = 0,
            )
          
          head(mynewdata)
          str(mynewdata)
          mynewdata = as.data.frame(mynewdata)
          
          fc = forecast::forecast(climatets.lm, newdata = mynewdata)
                                  
          
          g1 = autoplot(myts[,1]) + autolayer(fc, PI = FALSE) + xlim(2010,2021)
          g1
          
          mynewdata = new_data(us_change,4) |>
            dplyr::mutate(Income = mean(us_change$Income),
                          Production = mean(us_change$Production),
                          Savings = mean(us_change$Production),
                          Unemployment = mean(us_change$Production),
            )
          
          mynewdata = as.data.frame(mynewdata)
          
          myfc = forecast::forecast(climatets.lm, mynewdata)
          
          g2 = autoplot(myts[,1]) + autolayer(myfc, PI = FALSE) + xlim(2010,2021)
          g2
          
          cowplot::plot_grid(g1, g2)
        
#======================================================= #
#================================================== #
# Examine Climate ===============================================
          install.packages("rio")   # run once
          library(rio)
climate = import("climate.csv")
  head(climate)
    plot.ts(climate[,2:3])
      tsdisplay(climate$CO2)
      tsdisplay(climate$Temp)

      myclimate = scale(climate[,2:3]) |> as.data.frame()
climate.lm = lm(Temp ~ CO2, data = myclimate)
  summary(climate.lm)
    forecast::accuracy(climate.lm)
      checkresiduals(climate.lm)
        coef(climate.lm)
      
barplot(climate.lm$coefficients)
barplot(coef(climate.lm), horiz = TRUE)

# now with trend 
climate.ts = ts(climate[,2:3], start = 1919, frequency = 1) |> scale()

climatets.lm = tslm(Temp ~ CO2 + trend, data = climate.ts)
    summary(climatets.lm)
      forecast::accuracy(climatets.lm)
        checkresiduals(climatets.lm)

  barplot(climatets.lm$coefficients)


# ==============================  #

# QED ============            

#============ HW or In-Class ==============
# Does disposable personal income explain personal consumption spending?

# test a simple model
    ##determine accuracy
# test a simple model + trend and seasonality
    ##determine accuracy

  # hint: dont forget to "scale" the data
  
# generate a barplot of the coefficients of the best model

# place in bucket labeled PCE by November 7, 2025, COB/

#============= 

  suppressPackageStartupMessages({
    library(fpp3)
    library(forecast)
    library(tsbox)
    library(ggplot2)
  })
  data("us_change")
  myts <- ts_ts(us_change)  

  # Simple model: Consumption ~ Income
  simple_model <- tslm(Consumption ~ Income, data = myts)
  summary(simple_model)
  accuracy(simple_model)
  checkresiduals(simple_model)
  # Model with trend and seasonality
  trend_season_model <- tslm(Consumption ~ Income + trend + season, data = myts)
  summary(trend_season_model)
  accuracy(trend_season_model)
  checkresiduals(trend_season_model)  
  # Compare models
  cat("Simple model RMSE:", accuracy(simple_model)["RMSE"], "\n")
  cat("Trend+Season model RMSE:", accuracy(trend_season_model)["RMSE"], "\n")
  
  # Choose the best model (lower RMSE)
  best_model <- trend_season_model
  # Barplot of coefficients of best model
  barplot(coef(best_model),
          horiz = TRUE,
          main = "Coefficients of Best Model",
          col = "skyblue",
          las = 1)
  # Fitted vs Actual plot
  autoplot(myts[, "Consumption"], series = "Actual") +
    autolayer(fitted(best_model), series = "Fitted") +
    ggtitle("Actual vs Fitted - Best Model") +
    theme_minimal()
  
  
install.packages("pdfetch")
library(pdfetch)
library(TSstudio)
library(forecast)
library(ggplot2)
  pce = pdfetch_FRED("PCE")
  ts_info(pce)
  dspi = pdfetch_FRED("dspi")
  ts_info(dspi)
  mydata = cbind(pce,dspi)
  autoplot(mydata)
  autoplot(cbind(pce,dspi))
  mydata.ts=ts(mydata,start=c(1959,1),frequency=12)
  head(mydata.ts)
  mymodel=tslm(pce ~ dspi, mydata.ts)
  summary(mymodel)
  mymodel2=tslm(pce ~ dspi+trend + season, mydata.ts)
  summary(mymodel2)
  
  # ==============================
  # Scale the data
  # ==============================
  mydata.scaled <- scale(mydata.ts)
  
  # ==============================
  # Model 1: Simple Regression
  # ==============================
  model01 <- tslm(pce ~ dspi, data = mydata.scaled)
  summary(model01)
  
  # Accuracy
  accuracy(model01)
  
  # ==============================
  # Model 2: With Trend and Seasonality
  # ==============================
  model12 <- tslm(pce ~ dspi + trend + season, data = mydata.scaled)
  summary(model12)
  
  # Accuracy
  accuracy(model12)
  
  # ==============================
  # Compare Models
  # ==============================
  cat("\nModel 1 Adjusted R²:", summary(model01)$adj.r.squared)
  cat("\nModel 2 Adjusted R²:", summary(model12)$adj.r.squared)
  
  # ==============================
  # Coefficients Bar Plot (Best Model)
  # ==============================
  # Choose the better model based on R² or AIC
  best_model <- ifelse(summary(model12)$adj.r.squared > summary(model01)$adj.r.squared, "Model 2", "Model 1")
  cat("\nBest Model:", best_model)
  
  # Extract coefficients from best model
  coeff_data <- as.data.frame(summary(if (best_model == "Model 2") model12 else model1)$coefficients)
  coeff_data$Variable <- rownames(coeff_data)
  colnames(coeff_data) <- c("Estimate", "StdError", "tValue", "pValue", "Variable")
  
  # Plot coefficients
  ggplot(coeff_data, aes(x = Variable, y = Estimate, fill = Variable)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("Coefficient Estimates of", best_model),
         x = "Variables", y = "Estimate") +
    theme(legend.position = "none")
  
  # ==============================
  # Interpretation Notes
  # ==============================
  # Model 1 tests whether DSPI alone explains PCE.
  # Model 2 adds trend and seasonality to improve accuracy.
  # Check Adjusted R² and p-values to evaluate significance.
  # If the DSPI coefficient is significant (p < 0.05), it indicates
  # that disposable income has a strong positive relationship with consumption.
  
  
  
  
  
  
  
  
  pce = pdfetch::pdfetch_FRED("PCE")   #Personal consumption expenditures
  dspi = pdfetch::pdfetch_FRED("DSPI") #Disposable personal income
  
  ts_info(pce); ts_info(dspi)
  plot(cbind(pce, dspi))
  
  pce.ts  = ts(pce , start = c(1959,1), frequency = 12)
  dspi.ts = ts(dspi, start = c(1959,1), frequency = 12)
  
  mydata.ts = cbind(pce.ts, dspi.ts)
  head(mydata.ts)
  
  
  mydata.ts.scaled = scale(mydata.ts)
  head(mydata.ts.scaled)
  
  
  mymodel0 = tslm(pce.ts ~ dspi.ts, data = mydata.ts.scaled)
  summary(mymodel0)
  accuracy(mymodel0)
  barplot(mymodel0$coefficients[2])
  
  
  mymodel = tslm(pce.ts ~ dspi.ts + trend + season, data = mydata.ts.scaled)
  summary(mymodel)
  accuracy(mymodel)
  barplot(mymodel$coefficients[2:14])

  