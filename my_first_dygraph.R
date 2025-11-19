#===================================================================== #
# HW ======================
# Dygraph a time series running from 2020:1 to 2024:Q1
# Start from a simulation of 102 data points.
# Convert the dygraph to an html page. Upload.
setwd("D:/UNH/SEMESTER 3/Business Forecastiing")
library(dygraphs)
library(zoo)       # for rollmean
library(ggplot2)
library(htmlwidgets)
install.packages("forecast")
library(forecast)

# 1. Generate 102 data points using arima.sim
set.seed(123) 
mytsdata <- arima.sim(list(order = c(1,1,0), ar = 0.7), n = 102)

# 2. Convert to quarterly time series starting in 2000
mytimeseries <- ts(mytsdata, start = c(2000, 1), frequency = 4)

# 3. Select a window from 2020:1 to 2024:Q1
mytimeseries <- window(mytimeseries, start = c(2020,1), end = c(2024,1))

# 4. Dygraph of the full series with 6-period smoother
dygraph(mytimeseries, main = "My Time Series", ylab = "Widgets / Year") %>%
  dyOptions(fillGraph = FALSE, drawGrid = TRUE, colors = "darkred") %>%
  dyRangeSelector() %>%
  dyRoller(rollPeriod = 6, showRoller = TRUE) %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.5, 
              hideOnMouseOut = FALSE)

# 5. Create a 6-period rolling average aligned right
myroll <- rollmean(mytimeseries, k = 2, align = "right")

# 6. Plot both series together
autoplot(mytimeseries) + 
  autolayer(myroll, color = "blue", size = 1.2) +
  ggtitle("Original vs 6-Period Rolling Average")

# 7. Bind the series for dygraph
mynewseries <- cbind(mytimeseries, myroll)

# 8. Dygraph with both mytimeseries and myroll
dg <- dygraph(mynewseries, main = "Time Series & 6-Period Average", 
              ylab = "Widgets / Year") %>%
  dyOptions(fillGraph = FALSE, drawGrid = TRUE, colors = c("darkred", "blue")) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.5, 
              hideOnMouseOut = FALSE)

# 9. Save dygraph as HTML
saveWidget(dg, "my_first_dygraph.html")























