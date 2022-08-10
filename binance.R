# Docs
# https://binance-docs.github.io/apidocs/spot/en/#kline-candlestick-data

# Strategy idea:
# When a coin opens and is dropping
# It will 9/10 times go above the opening price
# So buy the coin once it stops dropping (after it has dropped by x%)
# OR once it goes -x% without going over the opening price
# then sell after is goes back above the opening price.

# Strategy Idea:
# Input a sell price and a buy price
# Bot will create the limit spot trades automatically
# Once the buy is triggered create a new sell trade vice versa

# Interact with Binance API
library(httr)
library(tidyverse)
library(lubridate)
library(zoo)

url_root <- "https://api.binance.com/api/v3/"
# Optional roots are "https://api2.binance.com" 1,2 and 3
avgPrice_ex <- "avgPrice?symbol=BTCUSDT"

type_of_data <- "klines"
interval <- "1h"
symbol <- "ETHBUSD"
limit <- 1000
url_stem <- paste0(type_of_data, "?symbol=",symbol, "&interval=",interval, "&limit=",limit)

headers <- HEAD(paste0(url_root,url_stem))

response <- GET(paste0(url_root,url_stem))
data <- content(response, as = "parsed")
df <- data.frame(open_time = sapply(data, "[[", 1),
                 open = as.numeric(sapply(data, "[[", 2)),
                 high = as.numeric(sapply(data, "[[", 3)),
                 low = as.numeric(sapply(data, "[[", 4)),
                 close = as.numeric(sapply(data, "[[", 5)),
                 volume = as.numeric(sapply(data, "[[", 6)),
                 close_time = sapply(data, "[[", 7),
                 qav = as.numeric(sapply(data, "[[", 8)),
                 num_trades = sapply(data, "[[", 9),
                 taker_base_vol = as.numeric(sapply(data, "[[", 10)),
                 taker_quote_vol = as.numeric(sapply(data, "[[", 11))
                 )
df$open_time <- as.POSIXct(df$open_time/1000, origin = "1970-01-01")
df$close_time <- as.POSIXct(df$close_time/1000, origin = "1970-01-01")
df$width <- (df$close_time-df$open_time)/2
df$change <- ifelse(df$close >= df$open, "up", "down")
# Moving average based on closing price
# https://www.investopedia.com/terms/s/sma.asp
# Indicates trends
# Simple strategy
# Buy when 7 day goes above 14 day, Sell when 7 day goes below 14 day
df$MA7 <- c(rep(NA,6), rollapply(df$close, 7, mean))
df$MA14 <- c(rep(NA,13), rollapply(df$close, 14, mean))
# On Balance Volume
# https://www.investopedia.com/terms/o/onbalancevolume.asp
df$OBV <- ifelse(df$close > df$open, df$volume, ifelse(df$close < df$open, -df$volume, 0))
df$OBV <- cumsum(df$OBV)
# Relative Strength Index
# https://www.investopedia.com/terms/r/rsi.asp
# Over 14 periods
# This RSI might need something extra, its not quite the same as the Binance metric
# https://www.macroption.com/rsi-calculation/
df$RSI <- (df$close/df$open)-1
df$RSI <- c(rep(NA, 13), rollapply(df$RSI, 14, function(x){
  average_gain <- sum(x[x>0])/length(x)
  average_loss <- -sum(x[x<0])/length(x)
  100-(100/(1+(average_gain/average_loss)))
}))

ggplot(df, aes(x = open_time+width)) +
  geom_linerange(aes(ymin = low, ymax = high, col = change), linetype = 2) +
  geom_rect(aes(xmin = open_time, xmax = close_time, ymin = open, ymax = close, fill = change)) +
  geom_line(aes(y = MA7), col = "cornflowerblue", size = 1) +
  geom_line(aes(y = MA14), col = "darkorchid2", size = 1) +
  
  geom_point(aes(x = trade_dates, y = trade_price), trades) +
  
  xlab("") + ggtitle(symbol) + ylab("") +
  scale_y_continuous(position = "right") +
  scale_fill_manual(aesthetics = c("colour", "fill"), values = c("down" = "#F6465d", "up" = "#0ECB81")) + 
  guides(fill = FALSE, colour = FALSE) + 
  theme_minimal()

ggplot(df, aes(x = open_time+width)) +
  geom_line(aes(y = RSI)) +
  geom_hline(yintercept = c(30, 70))+
  xlab("") + ggtitle(paste(symbol, "Relative Strength Index")) + ylab("")+
  scale_y_continuous(position = "right") + 
  theme_minimal()

# Make a simple trading strategy
# Get a df with minimum 3 columns
# Date of trade, Price of trade, BUY or SELL

# Buy when 7 day goes above 14 day, Sell when 7 day goes below 14 day
# This can be determined by gradient of each line

# Quick is MA7 slow is MA14
poi <- function(quick, slow, dates){
  # Points of intersection
  # returns dates
  dates <- as.numeric(dates)
  # Where is one line above the other?
  above <- quick > slow
  # Where does it change?
  intersections <- which(diff(above) != 0)
  
  # https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
  # Points of the quick line
  x1 <- quick[intersections]
  x2 <- quick[intersections+1]
  y1 <- dates[intersections]
  y2 <- dates[intersections+1]
  # Points of the slow line
  x3 <- slow[intersections]
  x4 <- slow[intersections+1]
  y3 <- dates[intersections]
  y4 <- dates[intersections+1]

  # Calculate the points of intersection
  x.points <- ((x1*y2 - y1*x2)*(y3 - y4) - (y1 - y2)*(x3*y4 - y3*x4))/
    ((x1 - x2)*(y3 - y4)-(y1 - y2)*(x3 - x4))
  y.points <- ((x1*y2 - y1*x2)*(x3 - x4) - (x1 - x2)*(x3*y4 - y3*x4))/
    ((x1 - x2)*(y3 - y4)-(y1 - y2)*(x3 - x4))
  
  # Return a dataframe of xpoints and buy or sell
  data.frame(trade_dates = as.POSIXct(x.points, origin = "1970-01-01"),
             type = ifelse(x2-x1<x4-x3,"Sell","Buy"))
  
}
# Calculate dates when the quick and slow lines intersect
trades <- poi(df$MA7, df$MA14, df$open_time+df$width)
# Nearest date in the df
nearest_x <- function(x, yvec, xvec){
  x <- as.numeric(x)
  xvec <- as.numeric(xvec)
  yvec <- as.numeric(yvec)
  output <- c()
  for (xs in x) {
    output <- c(output, yvec[which.min(abs(xvec - xs))])
  }
  output
}

trades$trade_price <- nearest_x(trades$trade_dates, (df$high+df$low)/2, df$open_time)

# Calculate Profits from this strategy
start_balance <- 1 #100BUSD worth of ETH or 100BUSD
for(i in 2:dim(trades)[1]) {
  # Calculate %change
  if (trades$type[i] == "Buy") {
    # Previous is a sell, so if the sell price is greater a profit is made
    change <- trades$trade_price[i - 1] / trades$trade_price[i]
  } else if (trades$type[i] == "Sell") {
    change <- trades$trade_price[i] / trades$trade_price[i - 1]
  }
  print(i)
  print(change)
  start_balance <- start_balance * change
  print(start_balance)
}
sprintf("%.2f%%",(start_balance-1)*100)
