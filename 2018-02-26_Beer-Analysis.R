#' ---
#' title: "Analyzing a large data set"
#' author: Pavan Gurazada
#' output: github_document
#' ---
#' last update: Mon Mar 26 13:32:30 2018

#' Answer the following questions:
#'
#' *1* Which brewery produces the strongest beers by ABV%?
#'
#' *2* If you had to pick 3 beers to recommend using only this data, which would
#' you pick?
#'
#' *3* Which of the factors (aroma, taste, appearance, palette) are most
#' important in determining the overall quality of a beer?
#'
#' *4* Lastly, if I typically enjoy a beer due to its aroma and appearance,
#' which beer style should I try?
#' 

library(tidyverse)
library(ggthemes)
library(caret)
library(rsample)

theme_set(theme_few())

beer_tbl <- read_csv("data/general/beer_reviews.tar.gz")
glimpse(beer_tbl)

#' This is a non-standard data set since it has more rows than Excel can handle.
#' Lets move to answer the questions one-by-one

#' *1* Which brewery produces the strongest beers by ABV%?
#'
#' This is the alcohol percentage by volume of the beer. A `glimpse` of the data
#' set shows that the first column `beer_reviews/` seems to be a count of the
#' number of reviews per brewery so we can omit this from the table. We then
#' group the data by brewery name, compute the average ABV for each brewery and
#' arrange the summary in descending order

beer_tbl %>% select(-`beer_reviews/`) %>% 
             group_by(brewery_name) %>% 
             summarize_at(vars(beer_abv), funs(mean), na.rm = TRUE) %>% 
             arrange(desc(beer_abv))

#' Careful examination of the brewery names suggests that some nasty brewery
#' names seem to screw up the formatting, but does not make a difference to the
#' results

#' *2* If you had to pick 3 beers to recommend using only this data, which would
#' you pick?
#' 
#' Lets begin by seeing which of the beers have the highest overall review.
#' 
#' Deselect descriptive columns: beer_reviews, review_time, beer_beerid
#' Rearrange the data so that the review variables are ordered first (outcomes)
#' Remove rows with missing values

beer_tbl_clean <- beer_tbl %>% select(-`beer_reviews/`, -review_time, 
                                      -beer_beerid) %>% 
                               drop_na() %>% 
                               select(review_overall, review_aroma, 
                                      review_appearance, review_palate,
                                      review_taste, everything()) 
                               
                               

#' A simple method would be to recommend the beer with highest overall rating

beer_tbl_clean %>% group_by(beer_name) %>% 
                   summarize_at(vars(review_overall, review_aroma, review_appearance, 
                                     review_palate, review_taste),
                                funs(mean)) %>% 
                   arrange(desc(review_overall))

#' *3* Which of the factors (aroma, taste, appearance, palette) are most
#' important in determining the overall quality of a beer?
#' 
#' It would be interesting to see the variance in the reviews overall. There is
#' too many beer brands to facet, but an interesting question to ask is how
#' variable is the rating by people.

beer_reviews <- beer_tbl_clean %>% select(review_overall, review_aroma, 
                                          review_appearance, review_palate, 
                                          review_taste) %>% 
                                   gather(rating_type, rating)

dev.new()

ggplot(beer_reviews) + 
  geom_boxplot(aes(x = rating_type, y = rating))

#' The above plot shows that there is little difference in the mean ratings across
#' the different rating categories.
#' 
#' This indicates that the overall rating should turn out to be the average of
#' all the ratings. We can run a linear regression to confirm this suspicion

set.seed(20130810)

train_test_split <- initial_split(beer_tbl_clean, prop = 0.8)
beer_train_tbl <- training(train_test_split)
beer_test_tbl <- testing(train_test_split)

glimpse(beer_train_tbl)
glimpse(beer_test_tbl)

lm_fit <- train(review_overall ~ .,
                data = beer_train_tbl %>% select(review_overall, review_aroma, 
                                                 review_appearance, review_palate, 
                                                 review_taste),
                method = "lm",
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 5))

summary(lm_fit)

#' Looks like palate and taste contribute maximum to the overall rating.
#' We can check the predictive performance of the linear fit

lm_fit$results

#' Lets fit a random forest model and check the RMSE

rf_fit <- train(review_overall ~ .,
                data = beer_train_tbl %>% select(review_overall, review_aroma, 
                                                 review_appearance, review_palate, 
                                                 review_taste),
                method = "ranger",
                trControl = trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 5),
                num.trees = 20,
                seed = 20130810)

rf_fit$results

#' Linear model was better
#' 