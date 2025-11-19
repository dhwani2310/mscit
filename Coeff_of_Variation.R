# Coefficient of Variation


#' Businesses use the CV to assess the predictability of demand patterns, 
#' with a higher CV indicating a less predictable pattern

#' In finance, the coefficient of variation (CV) is used to compare the 
#' risk-to-reward ratio of different investments; it shows the volatility of 
#' an investment relative to its expected return. 

#' A lower CV indicates a more favorable risk-to-reward trade-off; 
#' meaning the investment is less volatile,
#' for the amount of return it provides; (i.e. less risky)


# which is more risky: ES or HD?

setwd("D:/UNH/SEMESTER 3/Business Forecastiing/class8")
options(digits = 3, scipen = 9999, stringasFactors = FALSE)
remove(list = ls())
graphics.off()

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
                  # Home Depot 
                  getSymbols("HD", 
                             src = "yahoo", 
                             freq = "monthly",
                             return.class = "xts",
                             from = "2010-01-01",
                             to = Sys.Date()
                  )   
                  
                  myhd = HD
                  
                              myhd = pdfetch_YAHOO("HD")
                              head(myhd)
                              ts_info(myhd)
                                        
            myhd = to.monthly(myhd)
            myhd = myhd$myhd.Close

            ts_plot(myhd)
            ts_info(myhd)
    
    #* Eversource
    getSymbols("ES", 
               src = "yahoo", 
               freq = "monthly",
               return.class = "xts",
               from = "2010-01-01",
               to = Sys.Date()
    )
    
    myes = ES
    
                    myes = pdfetch_YAHOO("ES")
                    head(myes)
                    ts_info(myes)
                    
    myes = to.monthly(myes)
    myes = myes$myes.Close
    ts_plot(myes)
    ts_info(myes)

    # calculate returns =========
    
    # You can also calculate weekly or monthly returns
    es_returns <- monthlyReturn(myes, type = "arithmetic")
      ts.plot(es_returns)
    hd_returns <- monthlyReturn(myhd, type = "arithmetic")
    
            es_returns_log <- diff(log(myes)) #alternatively
            arsenal::comparedf(as.data.frame(es_returns),
                               as.data.frame(es_returns_log))
            
            #ts.plot(cbind(es_returns,es_returns_log ), 
            #col = c("darkblue", "darkred"), lwd = c(2,1))
    

    
myes_cv = sd(es_returns)*100/abs(mean(es_returns))
myhd_cv = sd(hd_returns)*100/abs(mean(hd_returns))

    myes_cv; myhd_cv


        mydata = cbind(myes, myhd)
    ts.plot(mydata, col = c('red', "blue"))
    ts.plot(myes)
    ts.plot(myhd)

 
 ts_df(mydata) |> ggplot(aes(x = time, y = value, col = as.factor(id))) +
                        geom_line(linewidth = 1.2) +
                              theme(legend.position = "bottom")

 
 #========  # 
 
 
 