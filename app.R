# TODO add speed and acceleration plots
# TODO get some indicators in here

library(shiny)
library(httr)
library(tidyverse)
library(zoo)

stem <- "https://api.coingecko.com/api/v3/coins/"

format_price <- function(x, signif = 4, round = 2){
  ifelse(x<1, signif(x, signif), round(x, round))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Get Cryptocurrency Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          textInput("coin", "Coin:", value = "monero"),
          textInput("type", "Type:", value = "market_chart"),
          textInput("vs_currency", "Currency:", value = "gbp"),
          sliderInput("days", "Days:", 1,90,1,1)
        ),
        # Show a plot of the generated distribution
        mainPanel(
          #plotOutput("dailyplot"),
          plotOutput("dailyggplot", 
                     hover = hoverOpts("plot_hover", delayType = "throttle")),
          verbatimTextOutput("plot_hoverinfo")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  createurl <- reactive({
    paste0(stem,input$coin,"/",input$type,"?",
           "vs_currency=",input$vs_currency,"&days=",input$days)
  })
  createdf <- reactivePoll(60000, NULL, 
                           function(){HEAD(createurl())$headers$etag},
                           function(){
    response <- GET(createurl())
    data <- content(response, "parsed")
    df <- data.frame(price = sapply(data$prices, "[[", 2),
                     date = sapply(data$prices, "[[", 1))
    df$date <- as.POSIXct(df$date/1000, origin = "1970-01-01")
    # Moving average calculations
    df$MA7 <- c(rep(NA,6), rollapply(df$price, 7, mean))
    df$MA25 <- c(rep(NA,24), rollapply(df$price, 25, mean))
    # Stochastic Oscillator calculation
    df$SO14 <- rollapply(df$price, 14, min)
    df
  }) 
  # output$dailyplot <- renderPlot({
  #   df <- createdf()
  #   plot(x = df$date, y= df$price, type = "l", xlab = "Datetime", ylab = "XMR/GBP")
  # })
  output$dailyggplot <- renderPlot({
    df <- createdf()
    ggplot(df, aes(x = date, y = price)) +
      geom_line() +
      geom_line(aes(x=date, y=MA7, col = "MA7")) +
      geom_line(aes(x=date, y=MA25, col = "MA25")) +
      geom_text(data = df[df$price == max(df$price),],
                 mapping = aes(x = date, y = price, 
                               label = format_price(price)), 
                col = "green") +
      geom_text(data = df[df$price == min(df$price),],
                 mapping = aes(x = date, y = price, 
                               label = format_price(price)), 
                col = "red") +
      geom_text(data = df[dim(df)[1],],
                 mapping = aes(x = date, y = price, 
                               label = format_price(price))) +
      theme_minimal()
  })
  output$plot_hoverinfo <- renderPrint({
    df <- createdf()
    cat("Date: ",as.character(as.POSIXct(input$plot_hover$x, origin = "1970-01-01")),
        " Price: ",format_price(df$price[which.min(abs(as.numeric(df$date - input$plot_hover$x)))]),
        " \n%% change from left: ",
        100L*((df$price[which.min(abs(as.numeric(df$date - input$plot_hover$x)))]/df[1,]$price)-1L),
        " %% change to right: ", 
        100L*((df[dim(df)[1],]$price/df$price[which.min(abs(as.numeric(df$date - input$plot_hover$x)))])-1L) 
        )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
