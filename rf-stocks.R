library(tidyquant)
library(tidyverse)
library(caret)

data <- tq_exchange("NASDAQ")

SP  <- c("SPY", "AAPL", "MSFT", "AMZN", "WFC", "JPM", "BAC", "JNJ", "GOOG", "XOM", "XLF", "XLK", "XLE", "XLY")

extract_prices <- function(stocks) {
  output <- tq_get(stocks[1], 
                   get = "stock.prices",
                   from = "2007-01-01") %>% 
            select(date, adjusted)
  
  for (stock in stocks[-1]) {
    data <- stock %>% tq_get(., get = "stock.prices", from = "2007-01-01") %>% 
                      select(date, adjusted)
    output <- right_join(output, data, by = "date")
  }

  names(output) <- c("Date", stocks)
  
  return(output)
}
  
stock_data <- extract_prices(SP)


