# Impact Analysis


setwd("D:/UNH/SEMESTER 3/Business Forecastiing/class11")
options(digits = 3, scipen = 999999)
remove(list = ls())
graphics.off()

# March 22.25
# Rev: November 5, 2025


# Packages ==========
suppressPackageStartupMessages({
  suppressMessages({
    install.packages("stargazer")
                library(stargazer)
                library(tidyverse)
    install.packages("CausalImpact")
                library(CausalImpact)
                library(TSstudio)
                library(wooldridge)
                library(randomForest)
                library(quantmod)
})})

      
    # Tortious Interference ================ 

    # You are asked to calculate two things: (i) whether the allegation of 
    #' tortious inteference had an impact on Company A's stock price. 
    #' And, if true, (ii) how much was lost as a results; i.e. what are the 
    #' damages owed A.

          # https://www.youtube.com/shorts/SjV4vxnaY3U # oprah winfrey
          # https://www.youtube.com/watch?v=1bWqlOh6f3w # Texaco Pennzoil
          # https://www.youtube.com/watch?v=zFMgpxG-chM  # A/b testing

    # myprices.csv contains stock price data for company a and a competitor
    # Company B; there are 101 periods in the data set

    # The event occurred at time = 38
   
    # Data ========

     myprices = read.csv("myprices.csv")
          psych::headTail(myprices,3)
          dim(myprices)
     
     # impacted company's (company A) performance      
     plot.ts(myprices[,1], type = "l")
     abline( v= 38, col="red", lty=2, lwd = 2 ) # Event occurred
     
     # COUNTERFACTUAL: "but for the event, company A would have 'performed' 
     # differently.
     
        #YOUR Task: to set forth a 'counterfactual' (aka a 'but for') performance
        # and then estimate the difference between what actually happened 
        #and what would have happened 'but-for' the event.
     
              # Two Methods:
                  # 1. Estimate the counterfactual from past company performance.
                  # 2. Estimate the counterfactual from past "competitor" 
                  #    or 'equivalent'
     
     # METHOD 1
     
     #Create the "time" variable    
     myprices$time = 1:nrow(myprices)
     plot(myprices$time, type = "l")
          
     plot(myprices$PriceA ~ myprices$time, type = "l")
     # Event occurred
     abline( v= 38, col="red", lty=2, lwd = 2 )
     
     # Create a Dummy Variable to separate time into a ''before' and 
     # 'after' the Event.
     
     myprices$Dummy = 0
     myprices$Dummy[39:101] = 1
     myprices
          str(myprices)
          myprices$Dummy = myprices$time >= 39  # equivalent
          as.numeric(myprices$Dummy)
       
    myprices$Dummy = as.factor(myprices$Dummy)
    head(myprices)
     
    ts <- lm( PriceA ~ time + Dummy, data=myprices )
        summary(ts) 
    plot(myprices$PriceA ~ myprices$time, type = "l")
    abline( v= 38, col="red", lty=2, lwd = 2 )
    lines( ts$fitted.values ~ myprices$time, col="steelblue", lwd=2 )
    
         #  Does the Dummy variable change over time? 
         ts2 <- lm( PriceA ~ time + Dummy + time*Dummy, data=myprices )
            summary(ts2) 
         plot(myprices$PriceA ~ myprices$time, type = "l")
         abline( v= 38, col="red", lty=2, lwd = 2 )
         lines( ts$fitted.values ~ myprices$time, col="steelblue", lwd=2 )
         
     
             ######################## #
    
    # The Counterfactual ======================
    
    pred1 <- predict(ts2, myprices) 
          ts2$fitted.values # equivalent
          
          plot.ts(pred1)
    # To estimate all predicted values of Y, we just use our dataset
    # and the model capturing the kink
    
    
    # forecast the model as if the event did not occur
          ts3 <- lm( PriceA ~ time, data=myprices[1:38,] )
          datanew = data.frame(time = 1:101)
          pred2 <- predict(ts3, datanew) 
    
    # Predict the counterfactual
    plot(myprices$PriceA ~ myprices$time, type = "l",
         bty="n",
         col = gray(0.5,0.5), pch=19,
         xlab = "Time (days)", 
         ylab = "Price")
    abline( v=38, col="red", lty=2, lwd = 2 )
    lines( pred1[1:101] ~rep(1:101), col="dodgerblue4", lwd = 3 )
    lines( pred2[39:101] ~ rep(39:101),  col="darkorange2", lwd = 3, lty = 5 ) 
    text(60, 81, labels = "Actual Performance", pos = 4, cex = 1, col = "dodgerblue3")
    text(44, 83.25, labels = "Counterfactual ('But-for') Performance", 
         pos = 4, cex = 1, col = "darkorange2")
    
          diff = pred2[39:101] - pred1[39:101]
          damages = sum(diff)
          damages
          
          #METHOD 2
          
    # Project Counterfactual from Competitor or Alternative = ==
    #' Given that the counterfactual is not known, 
    #' Examine (i.e. model) the behavior of Firm B,
    #' a competitor to A and one not impacted by the event. 
     
    #'  This sets forth a different counterfactual - paradoxically - one that
    #'  did occur.
     
    #'  Use the information provided by Firm B's prices to (i) show the "path" of 
    #'  the counterfactual and (ii) to show the amount owed A. 
    
    plot(myprices$PriceA ~ myprices$time, type = "l",
         bty="n",
         col = "darkred",
         lwd = 2,
         pch=19,
         ylim = c(76,90),
         xlab = "Time (days)", 
         ylab = "Price")
    # Event occurred
    abline( v=38, col="red", lty=2, lwd = 2 )
    lines( myprices$PriceB ~myprices$time, col="dodgerblue4", lwd = 3 )
    
    mynewprices = myprices[39:101,]
        dim(mynewprices)
    
    myB = lm(PriceB ~ time, mynewprices)
    myBPred = predict(myB, mynewprices)
    
    myA = lm(PriceA ~ time, mynewprices)
    myAPred = predict(myA, mynewprices)
    
    plot(mynewprices$PriceA ~ mynewprices$time, type = "l", 
         ylim = c(76,90), col = "darkred", lwd = 2)
    lines(PriceB  ~ time, myprices, type = "l", col = "darkorange", lwd = 2)
    lines( myAPred ~mynewprices$time, col="dodgerblue4", lwd = 3 )
    lines( myBPred ~mynewprices$time, col="dodgerblue4", lwd = 3 )
    
    (  Dif = sum(myBPred -myAPred)   )
    
    # END =======
    
    # =========================  #
    # R package CausalImpact =========
    
    mydata = read.csv("mydata.csv",  header = TRUE)
      dim(mydata)
      str(mydata)
      head(mydata,3)  
    matplot(mydata, type = "l", lwd = 2)
    
    pre.period = c(1,70)
    post.period = c(71,100)
    
    impact <- CausalImpact(mydata$y,pre.period, post.period)
    
    plot(impact)
    summary(impact, "report")### summary with the report###
    
    impact$summary
    impact$series
    plot.ts(impact$series$response)
    plot.ts(impact$series$cum.response)
    
    colSums(impact$series)
    tail(impact$series)
    
    
    # ===========  #
    # Use the firm's performance to project counterfactual
    
    # Repeat the analysis above where you compare the series to its own linear
    # forecast as the counterfactual.  Show the damages calculated.
    
    pre.period = c(1,38)
    post.period = c(39,101)
    
    impact <- CausalImpact(cbind(myprices$PriceA, 
                                 myprices$time  # this is the key
                                 ), 
                           pre.period, post.period)
    
    
    plot(impact)
    names(impact)
    impact
    summary(impact)
    summary(impact, "report")
    
    impact$summary
    impact$series
    plot.ts(impact$series$response)
    plot.ts(impact$series$cum.effect)
    
    names(impact$series)
    tail(impact$series$cum.response)
    tail(impact$series)
    View(impact$series)
    # QED ===================
    
    ############################ #
    #Part two ===============
    # Determine if Brexit was good for Connecticut
    # use the Value of Connecticut Exports ("EXPTOTCT")
    # dependent variable.
    
    # Conduct 
    # (i) conduct a simple hypothesis test (using a linear model)
    # (i) determine an answer using CausalImpact
    
    
    # Exports of Goods for Connecticut (EXPTOTCT) (Millions of Dollars, NSA)
    getSymbols("EXPTOTCT", src = "FRED")
    rw = EXPTOTCT
    
    plot(rw)
    plot(log(rw))
    TSstudio::ts_info(rw)
    
    # shorten the series
    rw = window(rw, 
                start = as.Date("1995-08-01"), 
                end = as.Date("2024-12-31"))
    
                  rw = rw["1995-08-01/2024-12-31"] # alternatively
    
    TSstudio::ts_info(rw)
    
    # coincident economic activity index for the US: ticker USPCHI
    getSymbols("USPHCI", src = "FRED")
    eai = USPHCI
    TSstudio::ts_info(eai)
        eai = window(eai,start = "1995-08-01", end = "2024-12-31")
    plot(eai)
    
    # combine data
    mydata = data.frame(rw, eai )
    
    mydata = mydata |>rownames_to_column("Date")
    
    head(mydata,3)
    colnames(mydata)[2] = "CTExports"
    colnames(mydata)[3] = "USeai"
    
    
    mydata$time = 1:nrow(mydata)
    matplot(cbind(mydata$CTExports, mydata$USeai), type="l")
    
    # Brexit
    mydata$Brexit = 0  # Create a Brexit dummy
    mydata$Brexit = ifelse(mydata$Date >=  "2021-01-01", 1, 0)
            
              mydata$Brexit = as.numeric(mydata$Date >=  "2021-01-01") #equiv
    
        mod1 = lm(CTExports ~ time + USeai + Brexit, mydata)
    mod1 = lm(CTExports ~ time + USeai + Brexit + time*Brexit, mydata)
    summary(mod1)
    stargazer(mod1, type = "text")
    
    # Now repeat the analysis using Causal Impact
    library(CausalImpact)
    which(mydata$Date =="2020-01-01") # Brexit
    
    dim(mydata)
    pre.period = c(1,294) # Brexit
    post.period = c(295,353)
    
    impact <- CausalImpact(cbind(mydata$CTExports, mydata$USeai), 
                           pre.period, post.period)
    
    plot(impact)
    summary(impact)
    summary(impact, "report")
    tail(impact$series)
    tail(impact$series$cum.effect)
    # ===============================  #
    # Was Trump good for CT's Exports?
    
    # Exports of Goods for Connecticut (EXPTOTCT) (Millions of Dollars, NSA)
    getSymbols("EXPTOTCT", src = "FRED")
    rw = EXPTOTCT
    ts_info(rw)
    
          plot(rw)
          plot(log(rw))
          TSstudio::ts_info(rw)
          
    # shorten the series
    rw = window(rw, 
                start = as.Date("1995-08-01"), 
                end = as.Date("2024-12-31"))
    
    
          TSstudio::ts_info(rw)
    
    
    # US Exports of Goods and Services, Balance of Payments Basis (BOPTEXP) 
    # (Millions of Dollars, SA)
  
    getSymbols("BOPTEXP", src = "FRED")
    us = BOPTEXP
        ts_plot(us)
        ts_plot(log(us))
        TSstudio::ts_info(us)
    
    us = window(us, 
                start = as.Date("1995-08-01"), 
                end = as.Date("2024-12-31"))
    
    TSstudio::ts_info(us)
    plot.ts(us)
    
  
        matplot(cbind(rw, us), type = "l", ylim = c(0, 250000), 
            col = c("darkred", "darkblue"))  
    
    mydata = data.frame(rw, us )
    
    mydata = mydata |>rownames_to_column("Date")
    
    head(mydata,3)
    colnames(mydata)[2] = "CTExports"
    colnames(mydata)[3] = "USExports"
    
    mydata$Trump = 0
    mydata$Trump = ifelse(mydata$Date > "2017-02-01" & 
                            mydata$Date< "2021-01-31", 1, 0)
    
    mydata$time = 1:nrow(mydata)
    psych::headTail(mydata)
    
    mod1 = lm(CTExports ~ time + USExports + Trump, mydata)
    mod1 = lm(CTExports ~ time + USExports + Trump + time*Trump, mydata)
    summary(mod1)
    
    #Now with CausalImpact
    which(mydata$Date =="2017-01-01") # Trump
    which(mydata$Date =="2021-02-01") # Trump
    
    dim(mydata)
    
    pre.period = c(1,258) # trump
    post.period = c(307,353)
    
    impact <- CausalImpact(cbind(mydata$CTExports, mydata$USExports), 
                           pre.period, post.period)
    
    plot(impact)
    summary(impact, "report")
    tail(impact$series)
    tail(impact$series$cum.effect,50)
    
    
    # ==========================  #
    