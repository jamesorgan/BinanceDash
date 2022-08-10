# TODO add an extra settings tab for graph settings
# TODO make graphs auto update

library(shiny)
library(httr)
library(tidyverse)
library(lubridate)
library(zoo)

url_root <- "https://api.binance.com/api/v3/"

format_price <- function(x, signif = 4, round = 2){
  ifelse(x<1, signif(x, signif), round(x, round))
}
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

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Get Cryptocurrency Data"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(title = "Connection",
                 textInput("type", "Type:", value = "klines"),
                 textInput("interval", "Time interval:", value = "15m"),
                 textInput("symbol", "Symbol:", value = "CVXBUSD"),
                 sliderInput("limit", "Number of intervals:", 100,1000,200,100)
        ),
        tabPanel(title = "Graph",
                 numericInput("MA1", "Moving Average 1: ", 7, 2,200,1),
                 numericInput("MA2", "Moving Average 2: ", 14, 2,200,1),
                 radioButtons("indicator", "Indicator to plot:", 
                              list(RSI = "RSI", OBV = "OBV"), selected = "RSI")
                 )
      ),
      verbatimTextOutput("trade_result_text")
      
    ),
    # Show a time series plot with candlesticks 
    mainPanel(
      verbatimTextOutput("plot_hoverinfo"),
      plotOutput("daily_plot", 
                 hover = hoverOpts("plot_hover", delayType = "debounce")),
      plotOutput("indicator_plot")
      
    )
  )
)

server <- function(input, output) {
  autoInvalidate <- reactiveTimer(10000)
  createurl <- reactive({
    paste0(url_root, input$type, 
           "?symbol=",input$symbol, 
           "&interval=",input$interval, 
           "&limit=",input$limit)
  })
  createdf <- reactive({
    # Refresh every 5 seconds
    autoInvalidate()
    # Make a call to the API
    response <- GET(createurl())
    data <- content(response, "parsed")
    # Create the dataframe
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
    # Simple strategy:
    # Buy when 7 day goes above 14 day, Sell when 7 day goes below 14 day
    df$MA1 <- c(rep(NA,input$MA1-1), rollapply(df$close, input$MA1, mean))
    df$MA2 <- c(rep(NA,input$MA2-1), rollapply(df$close, input$MA2, mean))
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
    df
  }) 
  createtrades <- reactive({
    df <- createdf()
    trades <- poi(df$MA1, df$MA2, df$open_time+df$width)
    trades$trade_price <- nearest_x(trades$trade_dates, (df$high+df$low)/2, df$open_time)
    trades
  })
  output$daily_plot <- renderPlot({
    df <- createdf()
    trades <- createtrades()
    ggplot(df, aes(x = open_time+width)) +
      geom_linerange(aes(ymin = low, ymax = high, col = change), linetype = 2) +
      geom_rect(aes(xmin = open_time, xmax = close_time, ymin = open, ymax = close, fill = change)) +
      geom_line(aes(y = MA1), col = "cornflowerblue", size = 1) +
      geom_line(aes(y = MA2), col = "darkorchid2", size = 1) +
      geom_label(aes(x = trade_dates, y = trade_price, label = paste(type, trade_price)), trades) +
      xlab("") + ggtitle(input$symbol) + ylab("") +
      scale_y_continuous(position = "right") +
      scale_fill_manual(aesthetics = c("colour", "fill"), values = c("down" = "#F6465d", "up" = "#0ECB81")) + 
      guides(fill = FALSE, colour = FALSE) + 
      theme_minimal()
  })
  output$plot_hoverinfo <- renderPrint({
    df <- createdf()
    cat(paste0("Open time: ", as.POSIXct(nearest_x(input$plot_hover$x,df$open_time,df$open_time+df$width), 
                                        origin = "1970-01-01"), 
              " Close time: ", as.POSIXct(nearest_x(input$plot_hover$x,df$close_time,df$open_time+df$width), 
                                         origin = "1970-01-01"), 
              "\nO", nearest_x(input$plot_hover$x,df$open,df$open_time+df$width),
              " H", nearest_x(input$plot_hover$x,df$high,df$open_time+df$width),
              " L", nearest_x(input$plot_hover$x,df$low,df$open_time+df$width),
              " C", nearest_x(input$plot_hover$x,df$close,df$open_time+df$width), " ",
              format_price(nearest_x(input$plot_hover$x,df$close-df$open,df$open_time+df$width)), " (",
              round(nearest_x(input$plot_hover$x,100L*((df$close/df$open)-1L),df$open_time+df$width),2),"%)"
        ))
  })
  output$indicator_plot <- renderPlot({
    df <- createdf()
    if (input$indicator == "RSI") {
      ggplot(df, aes(x = open_time+width)) +
        geom_line(aes(y = RSI)) +
        geom_hline(yintercept = c(30, 70))+
        xlab("") + ggtitle(paste(input$symbol, "Relative Strength Index")) + ylab("")+
        scale_y_continuous(position = "right") + 
        theme_minimal()
    } else if(input$indicator == "OBV"){
      ggplot(df, aes(x = open_time+width)) +
        geom_line(aes(y = OBV)) +
        xlab("") + ggtitle(paste(input$symbol, "On Balance Volume")) + ylab("")+
        scale_y_continuous(position = "right") + 
        theme_minimal()
    }
    
  })
  output$trade_result_text <- renderText({
    trades <- createtrades()
    # Calculate Profits from this strategy
    start_balance <- 1 # 1BUSD worth of ETH or 1BUSD
    for(i in 2:dim(trades)[1]) {
      # Calculate %change
      if (trades$type[i] == "Buy") {
        # Previous is a sell, so if the sell price is greater a profit is made
        change <- trades$trade_price[i - 1] / trades$trade_price[i]
      } else if (trades$type[i] == "Sell") {
        change <- trades$trade_price[i] / trades$trade_price[i - 1]
      }
      start_balance <- start_balance * (change-0.0001) # binance fee
    }
    sprintf("%.2f%%",(start_balance-1)*100)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
