# Diffusion Indexes


#' Diffusion indexes are a useful way to summarize economic information 
#' because they are easy to understand and correlate well with 
#' economic activity over time. 


# Rev: September 2025

setwd("D:/UNH/SEMESTER 3/Business Forecastiing/Class4")
options(digits = 3, scipen = 99999)
remove(list = ls())
graphics.off()


      suppressWarnings({
        suppressPackageStartupMessages({
      install.packages("markovchain")
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
      library(pdfetch)

# Readings

# https://www.richmondfed.org/publications/research/economic_brief/2022/eb_22-22
# https://www.bls.gov/OPUB/MLR/1990/04/art3full.pdf


# ===========  #
# How to construct a diffusion index for the CT Economy ====

#Pick 3 or 5 or 7 pertinent variables 

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

              ts_info(CTNA)
              ts_info(SMU09000000500000003)
              ts_info(BUSAPPWNSACT)
              
              head(BUSAPPWNSACT,3); tail(BUSAPPWNSACT,3)

biz = to.monthly(BUSAPPWNSACT)[,4] |> ts_ts()
ct_ctna = CTNA |> ts_ts()
us_emp = SMU09000000500000003 |> ts_ts()

              ts_info(biz)
              ts_info(ct_ctna)
              ts_info(us_emp)

        #pick a smaller window  (for aesthetic purposes)
        biz_ss = window(biz,start = c(2010,1), end = c(2025,7))
        ctna_ss = window(ct_ctna,start = c(2010,1), end = c(2025,7))
        us_ss = window(us_emp,start = c(2010,1), end = c(2025,7))
        #
        
            #assemble it
            mydata = cbind.data.frame(biz_ss, us_ss, ctna_ss)
            head(mydata,3)
    
            write.csv(mydata, "mydata.csv", row.names = FALSE)
                    
            ts_plot(biz_ss)
            ts_plot(us_ss)
            ts_plot(ctna_ss)

                  #' Obtain first differences
                  mydf = mydata %>% 
                    mutate(bizD1 = tsibble::difference(biz_ss, differences = 1),
                           usD1 = tsibble::difference(us_ss, differences = 1),
                           ctD1 = tsibble::difference(ctna_ss, differences = 1)
                    ) %>% dplyr::select(c(bizD1, usD1, ctD1)) |> na.omit()

    colSums(is.na(mydf))
    
                # Convert to either up or down
                head(mydf,3)
                mydf_df = ifelse(mydf > 0, 1, -1 )
                mydf_df
                    table(mydf_df)
                    
                    
      #convert to up,down, or no change
      mydf_mat = apply(mydf, 2, sign) 
          table(mydf_mat)
      mydf_mat
      

            pos = apply(mydf_mat, 1,  function(row) sum(row>0) ) # counts the positive
            neg = apply(mydf_mat, 1,  function(row) sum(row<0) ) # counts the negatives
            
            cbind(pos, neg) |> head(5)
            
                #pos = apply(mydf, 1,  function(row) sum(row>0) ) # counts the positive
                #neg = apply(mydf, 1,  function(row) sum(row<0) ) # counts the negatives
            
            tot = pos + neg
              table(tot)
            ( index = (pos/tot - neg/tot)*100  )
                table(index)
            plot(index, type = "l")
            abline(a = 0, b = 0, col = "darkred")

cbind(mydf_mat, pos, neg, tot, index)

ma_index = zoo::rollmean(index, 7, align = "right")
    length(index)

    
        # BASE R Plot =========
        plot(index[7:186], type = "l")
        abline(a = 0, b = 0, col = "darkblue")
        lines(ma_index, col = "darkred", lwd = 2.5)
        
        ma_index.ts=ts(ma_index,start=c(2010,6),end=c(2025,7), frequency = 12)
        ts_info(ma_index.ts)
        ts_plot(ma_index.ts)
        
        CHI=pdfetch_FRED("CFNAIDIFF") |> first_of_month()|> ts_ts()
        CHI_SS=window(ct_ctna,start=c(2010,6),end=c(2025,7), frequency = 12)
        ts_info(CHI_SS)
        ts_plot(CHI_SS)
        ts_plot(c(ma_index,CHI_SS))
        


        # GGPLOT ==============        
                
            # match lengths (accounting for omits and the smoother lags)    
            ts_info(biz)
            length(biz) - 7 - 1
            
            length(ma_index)
        
        Date = seq.Date(from = as.Date("2010-05-1"), length.out = 186, by = "month")
            length(Date)
        
        cbind.data.frame(Date, index) |>
          ggplot(aes(x = Date, y = index)) +
          geom_line()+
          geom_hline(yintercept = 0, col = "red") +
          geom_smooth(colour = "blue") +
          labs( title = "Connecticut Economy") +
          xlab("Months") +
          ylab("Change")+
          ylim(-125,125)+
          theme(axis.line.x = element_line(size= 0.75, colour = "black"), 
                axis.line.y = element_line(size= 0.75, colour = "black"), 
                legend.position = "bottom", 
                legend.direction = "horizontal", element_blank()) +
          ggthemes::theme_tufte()       
          
        
# ============================================  #
        #HW: Compare the Chicago Fed National Activity Index with your smoothed
        # index above.  i.e. place both indexes in a time series plot.
        # place the html file (or pdf) in the bucket labelled chicago by Friday,
        # September 26. 

        # Chicago ===========
        getSymbols(c("CFNAIDIFF"), 
                   freq = "monthly", 
                   src = "FRED", return.class = 'xts',
                   index.class  = 'Date',
                   from = "2010-01-01",
                   to = Sys.Date(),
                   periodicity = "monthly")
        
        
        library(pdfetch)
        library(tsbox)
        library(lubridate)     # to handle and plot time series
        
        CHI=pdfetch_FRED("CFNAIDIFF") |> first_of_month()|> ts_ts()
        CHI_SS=window(CHI,start=c(2010,6),end=c(2025,7), frequency = 12)
        ts_info(CHI_SS)
        ts_plot(CHI_SS)
        Data=cbind(ma_index.ts,CHI_SS)
        ts_plot(Data)
        
        ts_plot(c(ma_index,CHI_SS))

        
        # QED =======
        library(pdfetch)
        library(tsbox)
        library(lubridate)
        library(xts)
        
        # define first_of_month so your pipeline works
        first_of_month <- function(x) {
          index(x) <- floor_date(index(x), "month")
          return(x)
        }
        
        CHI=pdfetch_FRED("CFNAIDIFF") |> first_of_month() |> ts_ts()
        CHI_SS=window(CHI,start=c(2010,6),end=c(2025,7), frequency = 12)
        ts_info(CHI_SS)
        ts_plot(CHI_SS)
        Data=cbind(ma_index.ts,CHI_SS)
        ts_plot(Data)
        ts_plot(ts_c(ma_index.ts, CHI_SS))
    
        # <- fixed
        