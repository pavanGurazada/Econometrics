#' ----
#' title: Analyzing stock market returns 
#' author: Pavan Gurazada
#' ---

#' last update: 2018-03-23 08:33:09

library(tidyverse)

stocks <- read_csv("data/MBA-Matt/stocks.csv", progress = TRUE)
glimpse(stocks)

bstocks <- read_csv("data/MBA-Matt/bigstocks.csv", 
                    progress = TRUE, 
                    col_names = c("Stock", "Price"))

