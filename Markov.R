# Modeling Markov Chains


#'A Markov chain is a statistical model that describe a system changing randomly 
#'over time, where the future state does not depend on the entire past history.
#'Rather, the future state depends only on the current state. 
#'Markov chains are characterized by a set of discrete "states" and 
#'probabilities for transitioning between them.


      setwd("D:/UNH/SEMESTER 3/Business Forecastiing/class5")
      options(digits = 3, scipen = 99999)
      remove(list = ls())
      graphics.off()


      suppressWarnings({
        suppressPackageStartupMessages({
      library(markovchain)
      library(tidyverse)
      library(quantmod)
      library(tsbox)
          library(TSstudio)
          library(xts)
      library(vars)
          library(ggthemes)
        })
      })


# ===========  #
# First: a diffusion index for the CT Economy ====
      
        #' Average Hourly Earnings of All Employees: 
              #' Total Private in Connecticut (SMU09000000500000003)
        #' All Employees: Total Nonfarm in Connecticut (CTNA)
        #' Business Applications for Connecticut (BUSAPPWNSACT)


          getSymbols(c("BUSAPPWNSACT", "CTNA", "SMU09000000500000003"), 
                     freq = "monthly", 
                     src = "FRED", return.class = 'xts',
                     index.class  = 'Date',
                     from = "2010-01-01",
                     to = Sys.Date(),
                     #to = "2025-02-01",
                     periodicity = "monthly")

      mydata = pdfetch::pdfetch_FRED(c("BUSAPPWNSACT", "CTNA", "SMU09000000500000003"))
      
      biz = pdfetch::pdfetch_FRED("BUSAPPWNSACT")
      biz = to.monthly(biz)[,4] |> ts_ts()
      head(biz)
      ts_info(biz)
          autoplot(biz)
      
      ct_ctna = pdfetch::pdfetch_FRED("CTNA")
      head(ct_ctna)
      ts_info(ct_ctna)
      ct_ctna = ts(ct_ctna$CTNA, start = c(1990,1), frequency = 12 )
      ts_plot(ct_ctna)
      
      us_emp = pdfetch::pdfetch_FRED("SMU09000000500000003")
      ts_info(us_emp)
      us_emp = ts(us_emp$SMU09000000500000003, start = c(2007,1), frequency = 12 )
      head(us_emp)  
      install.packages("dygraph")
      library(dygraphs)
      dygraph(us_emp)
  
        #pick a smaller window
        biz_ss = window(biz,start = c(2010,1), end = c(2025,8))
        ctna_ss = window(ct_ctna,start = c(2010,1), end = c(2025,8))
        us_ss = window(us_emp,start = c(2010,1), end = c(2025,8))
        #
            #assemble it
            mydata = cbind.data.frame(biz_ss, us_ss, ctna_ss)
            head(mydata,3)

                  #' Obtain first differences
                  mydf = mydata %>% 
                    mutate(bizD1 = tsibble::difference(biz_ss, differences = 1),
                           usD1 = tsibble::difference(us_ss, differences = 1),
                           ctD1 = tsibble::difference(ctna_ss, differences = 1)
                    ) %>% dplyr::select(c(bizD1, usD1, ctD1)) |> na.omit()


      #convert to up,down, or no change
      mydf_mat = apply(mydf, 2, sign) 
          table(mydf_mat)
      mydf_mat
      

            pos = apply(mydf_mat, 1,  function(row) sum(row>0) ) # counts the positive
            neg = apply(mydf_mat, 1,  function(row) sum(row<0) ) # counts the negatives
            
            tot = pos + neg
            ( index = (pos/tot - neg/tot)*100  )
            
            table(index)
            plot(index, type = "l")
            abline(a = 0, b = 0, col = "darkred")

cbind(mydf_mat, pos, neg, tot, index)

ma_index = zoo::rollmean(index, 7, align = "right")

    length(index)    

    plot(index[7:187], type = "l")
    lines(ma_index, col = "darkred", lwd = 2.5)
    
#============================================== #        
# ============================================  #
        
        # Markov Tutorial ==========
        
        # --- Create Weather Sequence ----
        sequence = c("C", "C", "S", "W", "R", "R",
                     "W", "S", "S", "C", "C", "C",
                     "R", "C", "S", "S", "W", "R",
                     "C", "R", "R", "R", "R", "C", 
                     "W", "S", "S", "W", "R", "W",
                     "W", "R", "C", "R", "R", "R", 
                     "R", "C", "W", "S", "S", "W", 
                     "R", "W", "C", "C", "S", "W", 
                     "R", "R", "W", "S", "S", "C", 
                     "C", "C", "R", "C", "S", "S", 
                     "W", "S", "S", "W", "R", "W", 
                     "W", "R", "C", "R", "R", "R", 
                     "R", "C", "W", "S", "S", "W", 
                     "R", "W", "C", "C", "S", "W", 
                     "R", "R", "W", "S", "S", "S")
        
        sequence
        
        weather = markovchainFit(data=sequence)
        weather$estimate@transitionMatrix
        
        mc1 = new("markovchain", transition=weather$estimate@transitionMatrix)
        steadyStates(mc1)
        barplot(steadyStates(mc1))
        plot(mc1)
        
        seq = c("1", "2", "0", "3", "2", "0", "0", "1", "2",
                "3", "3", "2", "1", "3", "0", "0", "3", "2",
                "2", "1", "3", "0", "1", "3", "0", "0", "3",
                "2", "1", "2")
        
        level = markovchainFit(data=seq)
        level$estimate@transitionMatrix
        
        mchain = new("markovchain", transition=level$estimate@transitionMatrix)
        steadyStates(mchain)
        barplot(steadyStates(mchain))
        
        seq = c(-20, 60, -33.3, -66.7, -33.3, -66.7)
        
        level = markovchainFit(data=seq)
        level$estimate@transitionMatrix
        
        mchain = new("markovchain", transition=level$estimate@transitionMatrix)
        steadyStates(mchain)
        plot(mchain)
        barplot(steadyStates(mchain))
        
        seq = c(-20, 60, -33.3,0, -66.7, -33.3, -66.7) |> as.data.frame()
        myseq = apply(seq, 1, sign)
        myseq
        mylevel = markovchainFit(data=myseq)
        mylevel$estimate@transitionMatrix
        mymchain = new("markovchain", transition=mylevel$estimate@transitionMatrix)
        steadyStates(mymchain)
        plot(mymchain)
        barplot(steadyStates(mymchain))
      
        
        # Modeling Diffusion index =================
        # we will return to this after we create the diffusion index (above)
        indexseq = index |> as.data.frame()
        myindexseq = apply(indexseq, 1, sign)
        myindexlevel = markovchainFit(data=myindexseq)
        myindexlevel$estimate@transitionMatrix
        myindexmchain = new("markovchain", transition=myindexlevel$estimate@transitionMatrix)
        steadyStates(myindexmchain)
        plot(myindexmchain)
        barplot(steadyStates(myindexmchain))
        
        # =================================== =
        
#Markov Modeling
# markov

    table(index)
    as.data.frame(index)
   

myindex = trunc(as.numeric(index),digit = 4)
str(myindex)
table(myindex)

          myindex = index |> as.data.frame() |> 
            trunc() |>
            dplyr::mutate(NewIndex =
            case_when(
              index == "-100" ~ "WayDown",
                index == "-33" ~ "Down",
                  index == "0" ~ "Neutral",
                      index == "33" ~ "Up",
                        index == "100" ~ "WayUp",
              .default = "other"
            )
          )
              
          myindex
          
        #fit markov chain
        myFit<-markovchainFit(myindex$NewIndex)
        myFit

myFit$estimate
myFit$estimate@transitionMatrix

dtmcA <- new("markovchain",transitionMatrix=myFit$estimate@transitionMatrix) 
plot(dtmcA)
dtmcA
steadyStates(dtmcA)

################################ #  

        # FORECAST ====================
        library(markovchain)
        library(expm)
        
        # Example economic time series (e.g., daily stock returns)
        set.seed(42)
        returns <- rnorm(100, mean = 0.001, sd = 0.01)
        plot(returns, type = "l")
        
        # Define states based on thresholds
        states <- character(length(returns))
        states[returns > 0.005] <- "Up"
        states[returns < -0.005] <- "Down"
        states[abs(returns) <= 0.005] <- "Stable"
        
                    #states = sign(returns)
        
                    # Convert to a factor for consistency
                    states <- factor(states, levels = c("Up", "Stable", "Down"))
                    levels(states)
                    
        # Fit a Markov chain model
        mcFit <- markovchainFit(data = states)
        
        # Extract the transition matrix
        transitionMatrix <- mcFit$estimate@transitionMatrix
        print(transitionMatrix)
        
        # Assume an initial state distribution (e.g., currently in "Stable" state)
        # This represents the probability of being in each state at the current time
        initial_distribution <- c("Up" = 0, "Stable" = 1, "Down" = 0)
        
        # Forecast 5 steps ahead
        n_steps <- 5
        forecasted_distribution <- initial_distribution %*% (transitionMatrix %^% n_steps)
        print(forecasted_distribution)
        
        # You can also simulate a path
        simulated_path <- rmarkovchain(n = 10, object = mcFit$estimate, t0 = "Stable")
        print(simulated_path)
        
              plot(as.numeric(as.factor(simulated_path)), type = "l")
              transitionMatrix <- mcFit$estimate@transitionMatrix
        
        #========================#
        # Forecast the CT Index for the next 5 months
              
              head(myindex)
              table(myindex$NewIndex)
              mcFit <- markovchainFit(data = myindex$NewIndex)
              transitionMatrix = mcFit$estimate@transitionMatrix
              initial_distribution <- c("Down" = 0, 
                                        "Neutral" = 1, 
                                        "Up" = 0, 
                                        "WayUp" = 0,
                                        "WayDown" = 0)
              n_steps <- 5
              forecasted_distribution <- initial_distribution %*% (transitionMatrix %^% n_steps)
              print(forecasted_distribution)
              plot(as.numeric(forecasted_distribution), type = "l")
              
              
              # You can also simulate a path
              simulated_path <- rmarkovchain(n = 60, object = mcFit$estimate, t0 = "Neutral")
              mypath = as.numeric(as.factor(simulated_path))
              print(mypath)
              plot(mypath, type = "l")
              
              mysmooth = rollmean(mypath, 7, align = "right")
              
              plot(mypath[7:60], type = "l") 
              lines(mysmooth, col = "darkred", lwd = 2.5)
              
              
              # QED ==================  
              
              # TO DO =================
              
              # Run a 5-month Markov Chain forecast of the Chicago Diffusion Index.
              # Assume 5 States.
              # Assume a normal state for the starting period.
              # What does the forecast predict for December?
              
              # Provide a line Chart of the 5  month forecast.
              # Upload to bucket labelled Markov.
              
              # ================= =  