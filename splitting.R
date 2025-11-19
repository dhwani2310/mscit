# Splitting Time Series

setwd("D:/UNH/SEMESTER 3/Business Forecastiing/class6")

#' Splitting time series data into training and testing sets in R requires a 
#' sequential approach, unlike random splitting used for other data types. 

#' The training set should always consist of earlier observations, 
#' and the testing set should contain later observations.

# We use the All-Transactions House Price Index for Connecticut
# ticker = CTSTHPI

library(pdfetch)
library(forecast)
library(lubridate)
library(TSstudio)

          # Conventional Splitting ================
          
          data(mtcars)
          dim(mtcars)
          names(mtcars)
          mtcars$id <- 1:nrow(mtcars)
          
          set.seed(42)
          train <- mtcars %>% dplyr::sample_frac(0.70)
          test  <- dplyr::anti_join(mtcars, train, by = 'id')
          
          test$id = NULL
          train$id = NULL
          
              dim(train)
              dim(test)

#Time Series Splitting ==========================
#Data from FRED =================
my_ts <- pdfetch_FRED("CTSTHPI")  # All-Transactions House Price Index for Connecticut
                                 # Quarterly; NSA
      ts_info(my_ts)
          head(my_ts)
ts_plot(my_ts)
sent.ts = ts(my_ts$CTSTHPI, start = c(1975,1), frequency = 4)
    ts_info(sent.ts)
sent.ts = window(sent.ts, start = c(2010,1), end = c(2025,2), frequency = 4)
    ts_info(sent.ts)
    length(sent.ts)

          split_ts_rider <- TSstudio::ts_split(sent.ts, sample.out = 12)
          train_rider <- split_ts_rider$train
          test_rider <- split_ts_rider$test
            length(train_rider)
            length(test_rider)
          
autoplot(train_rider) + autolayer(test_rider)

    autoplot(cbind(train_rider, test_rider))
    ts_plot(cbind(train_rider, test_rider))

  # To do:  split the WTI prices series and graph both components.
  # ticker: DCOILWTICO
    # place graph with split series in bucket Oil  Split, today.
    
    oil = pdfetch_FRED("DCOILWTICO") # West Texas Intermediate
        head(oil)
          ts_info(oil)
            length(oil)
    
    oil = xts::to.monthly(oil)[,4]
    
    
    #====================  #        

    
    oil = pdfetch_FRED("DCOILWTICO") # West Texas Intermediate
    head(oil)
    ts_info(oil)
    length(oil)
    
    oil = xts::to.monthly(oil)[,4]   # convert to monthly (use Close price)
    colnames(oil) <- "WTI"
    head(oil)
    
    oil.ts = ts(oil, start = c(1986,1), frequency = 12)
    ts_info(oil.ts)
    
    oil.ts = window(oil.ts, start = c(2010,1), end = c(2025,2), frequency = 12)
    ts_info(oil.ts)
    length(oil.ts)
    
    split_oil <- TSstudio::ts_split(oil.ts, sample.out = 12)
    train_oil <- split_oil$train
    test_oil  <- split_oil$test
    length(train_oil)
    length(test_oil)
    
    autoplot(train_oil) + autolayer(test_oil)
    
    autoplot(cbind(train_oil, test_oil))
    ts_plot(cbind(train_oil, test_oil))
    
    