stem <- "https://api.coingecko.com/api/v3/coins/"
coin <- "monero"
type <- "market_chart"
vs_currency <- "gbp"
days <- 90

headers <- HEAD(
  paste0(stem,coin,"/",type,"?","vs_currency=",vs_currency,"&days=",days)
)
headers$headers$etag


response <- GET(
  paste0(stem,coin,"/",type,"?","vs_currency=",vs_currency,"&days=",days)
  )
data <- content(response, "parsed")
df <- data.frame(price = sapply(data$prices, "[[", 2),
                 date = sapply(data$prices, "[[", 1))
df$date <- as.POSIXct(df$date/1000, origin = "1970-01-01")

# df$speed <- df$price-c(df$price[-1],NA)
# df$accel <- (df$speed-c(df$speed[-1],NA))^2
df$MA7 <- c(rep(NA,6), zoo::rollapply(df$price, 7, mean))
df$MA30 <- c(rep(NA,29), zoo::rollapply(df$price, 30, mean))
df$SO14 <- c(rep(NA,13), 100*(rollapply(df$price, 14, function(x){x[length(x)]}) - zoo::rollapply(df$price, 14, min))/
  (zoo::rollapply(df$price, 14, max) - zoo::rollapply(df$price, 14, min)))

ggplot(df, aes(x=date, y=price)) +
  geom_line() +
  geom_line(aes(x=date, y=MA7), col = "red") +
  geom_line(aes(x=date, y=MA30), col = "blue") +
  theme_minimal()

ggplot(data.frame(date = rep(df$date,2), 
                  metric = rep(c("price","SO14"),each=dim(df)[1]),
                  value = c(df$price,df$SO14)), aes(x=date, y=value)) +
  geom_line() +
  facet_grid(metric ~ ., scales = "free_y")+
  xlim("2022-07-01",NA)+
  theme_minimal()
