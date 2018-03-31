#' ---
#' title: "Optimizing media spend"
#' author: Pavan Gurazada
#' output: github_document
#' ---
#' last update: Sat Mar 31 07:56:08 2018
#' 
#' https://www.themarketingtechnologist.co/optimize-media-spends-using-s-response-curves/

library(tidyverse)

theme_set(theme_minimal())

#' The interesting portions of this example is the data creation bit.
#'
#' The best sales happen on Wednesday (i.e., day 3) each week. We store the mean
#' for the rest of the days in a vector. We assume that the actual sale on each
#' day is drawn from a normal distribution with the corresponding mean and a
#' standard deviation of 0.5

daily_means <- c(5, 7, 8, 4, 4, 3, 2) # Mean sales for each day of the week

daily_sales <- map_dbl(daily_means, rnorm, n = 1, sd = 0.5)

#' Radio ads are placed randomly on few days of the month and have an impact on 
#' sales for the day the ad was placed. Clearly, while radio ads might have longer
#' lasting effects, we assume only short-run behavior.
#' 
#' Let us now build an entire month of dates and the corresponding weekdays into 
#' vectors

date <- seq.Date(as.Date("2018-03-01", format = "%Y-%m-%d"), by = 1, length.out = 31)
day <- wday(date, week_start = 1)

#' Let us say we place 14 radio ads during this month. The radio spots earn the
#' following set of GRP's

# radio_grps <- c(4, 0, 1, 2, 3, 0, 0, 0, 7.2, 0, 6, 0, 8, 0, 0, 0, 2.25, 4.6, 0, 
#                 1.5, 0, 0, 9, 0, 0, 6.5, 0, 0, 0, 2, 1)

radio_grps <- c(4, 1, 2, 3, 7.2, 6, 8, 2.25, 4.6, 1.5, 9, 6.5, 2, 1)

#' We now have the ingredients to build a fake data set

sales_df <- data.frame(date, day)
sales_df$sales <- sales_df %>% select(day) %>% 
                               apply(1, function(x) { 
                                          daily_means <- c(5, 7, 8, 4, 4, 3, 2)
                                          return(rnorm(1, daily_means[x], 0.5)) 
                                        })

sales_df$radio_grp <- sample(c(radio_grps, rep(0, 31 - length(radio_grps))),
                             size = nrow(sales_df),
                             replace = FALSE)

# sales_df$radio_grp <- radio_grps

logistic <- function(x, L = 10, k = 1.25, x0 = 5){
  return(ifelse(x != 0, L/(1 + exp(-k * (x - x0))), 0))
}

sales_df$total_sales <- sales_df$sales + logistic(sales_df$radio_grp)

ggplot(data = sales_df, aes(x = date, y = total_sales)) +
  geom_point() +
  geom_line() +
  geom_bar(aes(y = radio_grp, fill = radio_grp), stat = "identity") +
  scale_fill_gradient(low = "grey", high = "black") +
  labs(x = "Date",
       y = "Total Sales",
       title = "Sales and Radio GRPs for March 2018",
       fill = "Radio GRPs")

summary(lm(total_sales ~ factor(day) + radio_grp, data = sales_df))

sales_df <- sales_df %>% mutate(radio_dummy1 = ifelse(radio_grp >= 0.4 & radio_grp <= 2.4, 1, 0),
                                radio_dummy2 = ifelse(radio_grp >= 2.5 & radio_grp <= 4.9, 1, 0),
                                radio_dummy3 = ifelse(radio_grp >= 5.0 & radio_grp <= 7.4, 1, 0),
                                radio_dummy4 = ifelse(radio_grp >= 7.5 & radio_grp <= 10, 1, 0))

summary(lm(total_sales ~ factor(day) + radio_dummy1 + radio_dummy2 + radio_dummy3 + radio_dummy4,
           data = sales_df))
