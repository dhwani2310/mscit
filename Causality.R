# Granger Causality 

## 1. the Chicken and the Egg
## 2. Global Warming and Carbon Emissions


            #' Granger Causality
            #' 
            #' A variable X is said to Granger-causes another variable Y 
            #' if predictions of the value of Y based on its own 
            #' past values and on the past values of X are better 
            #' than predictions of Y based only on Y's own past values. 
            
            #' More appropriately rather than testing whether X causes Y, 
            #' Granger causality tests whether X forecasts Y.
            
setwd("D:/UNH/SEMESTER 3/Business Forecastiing/Class3")
      # Libraries ===========
      library(lmtest) # for the granger causality test function
      library(forecast)
      library(vars)
      library(tidyverse)
      library(xts)
      library(lubridate)
      library(TSstudio)
      library(tsbox)
      
      remove(list= ls())
      graphics.off()
      options(digits = 3, scipen = 99999)

      # http://www.tylervigen.com/spurious-correlations
      
      #======================================================================
      ## Which came first: the chicken or the egg?
      # Load Data ChickEgg
      # Annual data from 1930 to 1983 

      chickegg = read.csv("ChickEgg.csv", header = TRUE)
      head(chickegg)
      dim(chickegg)

      chick.ts = ts(chickegg$chicken, start = c(1930,1), frequency = 1)
      egg.ts = ts(chickegg$egg, start = c(1930,1), frequency = 1)
      
      periodicity(chick.ts)
      ts_info(egg.ts)
      
      autoplot(chick.ts)+ autolayer(egg.ts)
      autoplot(chick.ts)+ autolayer(egg.ts) + scale_y_log10() 
      
      ts_plot(cbind(chick.ts, egg.ts))
      
            # in ggplot
                  #Date = seq(as.Date("1930/1/1"), by = "year", length.out = 54)
            
                  Date = index(chick.ts) # alternatively
            
            #cbind.data.frame(year = lubridate::year(Date), chick.ts, egg.ts) |> 
              cbind.data.frame(year = Date, chick.ts, egg.ts) |> 
              pivot_longer(cols = c(chick.ts, egg.ts),names_to = "series",
                                                               values_to = "value") |>
              ggplot(aes(x = year, y = value, col = series)) + 
              geom_line(linewidth = 2) + scale_y_log10()
      
      # Base R
      plot(chick.ts, col = "darkred", lwd = 2)
      par(new= T)
      plot(egg.ts, col = "darkblue", lwd = 2)
      
      
      #' Null Hypothesis (H0): chicken do not predict egg
      #' Alternative Hypothesis (H1): chicken granger-causes egg.
      
            chickegg.ts = cbind(chick.ts, egg.ts)
            chickegg.ts = ts.union(chick.ts, egg.ts) # alternatively
      lmtest::grangertest(egg.ts ~ chick.ts, order = 3, data = chickegg.ts)
      
      #' The F test statistic is denoted by the letter F equals 0.59 and 
      #' the p-value associated with the F test statistic is Pr(>F) 0.62.
      #' Accordingly, we cannot reject the null hypothesis because 
      #' the p-value is greater than 0.05, 
      #' Accordingly, we can conclude that knowing the value of chicken 
      #' is not valuable for 
      #' forecasting the future values of egg.
      
    
      # Re run the Granger-Causality test in reverse
      #' Null Hypothesis (H0): egg do not predict chicken
      #' Alternative Hypothesis (H1): egg granger-causes chicken.
      grangertest(chick.ts ~ egg.ts, order = 3, data = chickegg.ts)
      
      #' Here the test shows a p-value pf 0.003. And we can thus
      #' we can reject the null that there is no granger causation between
      #' eggs and chicken and conclude that eggs granger-cause chicken.
      
      
      #======================================================================

#########################
      
      #======================================================================
      # Does Carbon cause global warming?
      
#https://www.r-bloggers.com/cause-effect-a-different-way-to-explore-temporal-data-in-tableau-with-r/
#https://www.r-bloggers.com/an-inconvenient-statistic/
#Source of CO2 emissions data: http://cdiac.ess-dive.lbl.gov/trends/emis/tre_glob_2014.html
#Source for surface temp data: https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/download.html


      gb = read.csv("global_warming.csv")
          head(gb)
            tail(gb)
              str(gb)
      

      #put data into a time series format
      carbon.ts = ts(gb$carbon, frequency=1, start=c(1850), end=c(2014))
      surface_temp.ts = ts(gb$surface_temp, frequency=1, start=c(1850), end=c(2014))

      autoplot(carbon.ts) + autolayer(surface_temp.ts)
      autoplot(carbon.ts) + autolayer(surface_temp.ts) + scale_y_log10()
      
      
      mydata = cbind(carbon.ts, surface_temp.ts*1000)
      autoplot(mydata)
      
        grangertest(surface_temp.ts, carbon.ts, order = 3)
        grangertest(carbon.ts, surface_temp.ts, order = 3)
      
    
      #Package VARS  
      library(vars)
        
      tsVAR <- vars::VAR(cbind(surface_temp.ts, carbon.ts), p = 3)
        tsDat <- ts.union(surface_temp.ts, carbon.ts) 
        tsVAR <- vars::VAR(tsDat, p = 3)
          
      vars::causality(tsVAR, cause = "carbon.ts")$Granger
      vars::causality(tsVAR, cause = "surface_temp.ts")$Granger
      
      
      #Stationarity =================
      
      #Using the adf test above in the ndiffs command of the forecast package, 
      #we can see that a 1st difference will allow us to achieve stationarity, 
      #which is necessary for vector autoregression and granger causality.
      
      
      #determine stationarity and number of lags to achieve stationarity
      
      ndiffs(carbon.ts, alpha = 0.05, test = c("adf"))
      ndiffs(carbon.ts, alpha = 0.05, test = c("kpss"))
      
      ndiffs(surface_temp.ts, alpha = 0.05, test = c("adf"))
      ndiffs(surface_temp.ts, alpha = 0.05, test = c("kpss"))
      
      
      #difference to achieve stationarity
      d.co2 = diff(carbon.ts)
      d.temp = diff(surface_temp.ts)
      
      # try vars
      tsDat <- ts.union(d.temp, d.co2) 
      ts_plot(tsDat)
      #determine the optimal number of lags for vector autoregression
      VARselect(tsDat, lag.max=10)$selection
      
      tsVAR <- vars::VAR(tsDat, p = 3)
      vars::causality(tsVAR, cause = "d.co2")$Granger
      vars::causality(tsVAR, cause = "d.temp")$Granger
      
      # ================  #
      # Back to the chicken egg problem with differenced data ==========
      
      ndiffs(egg.ts, alpha = 0.05, test = c("adf"))
      ndiffs(chick.ts, alpha = 0.05, test = c("adf"))
      
      
      #difference once to achieve stationarity
      d.egg = diff(egg.ts)
      d.chick = diff(chick.ts)
      
      
      tsDat <- cbind(d.egg, d.chick) 
                tsDat <- ts.union(d.egg, d.chick) 
      
      install.packages("cowplot")
      library("cowplot")
      #determine the optimal number of lags for vector autoregression
      VARselect(tsDat, lag.max=10)$selection
      
      tsVAR <- vars::VAR(tsDat, p = 1)
      vars::causality(tsVAR, cause = "d.egg")$Granger
      vars::causality(tsVAR, cause = "d.chick")$Granger
      
      cowplot::plot_grid(autoplot(d.chick), autoplot(d.egg), nrow =2 )
      
# QED ===============        
      
      # To Do
      # Go back to the sentiment and retail sales exercise
      
      # Use a granger causality test to detemine whether sentiment "causes"
      # retails sales or the other way around.
      
      # run through the test to determine whether the series need to be
      # differenced and the lag structure.
      
      
      # plot via cowplot::plot_grid() plots of the first differences graph
      # of sentiment and retail sales.
      # Place in the bucket "Sentiment-Cause" by Friday September 19.
    
    
      #Does sentiment cause retail sales?
      
      # ================== #
      