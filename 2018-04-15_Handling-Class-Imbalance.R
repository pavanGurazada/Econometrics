#' ---
#' title: "Handling Class Imbalance"
#' author: Pavan Gurazada
#' output: github_document
#' ---
#' 
#' last update: Sun Apr 15 14:04:44 2018

library(tidyverse)
library(caret)

set.seed(20130810)
theme_set(theme_minimal())

ccard_df <- read_csv("data/general/creditcard.csv.zip", progress = TRUE)

#' Columns
#' -------
#' Time – Time (in seconds) elapsed between each transaction and the first transaction in the dataset.
#' V1-V28 – Principal component variables obtained with PCA.
#' Amount – Transaction amount.
#' Class – Dependent (or) response variable with value as 1 in case of fraud and 0 in case of good.

glimpse(ccard_df)

#' The data is collected on a small time period.

max(ccard_df$Time, na.rm = TRUE)/(60 * 60 * 24)

#' So, the data was collected over a two day period
#' Let us rearrange the data into a more traditional form

ccard_df <- ccard_df %>% 
              drop_na() %>% 
              mutate(day = Time %/% (60 * 60 * 24) + 1,
                     hour = Time %/% (60 * 60) + 1) %>% 
              mutate(hour = ifelse(hour > 24, hour - 24, hour)) %>% 
              select(Class, Amount, Time, day, hour, everything())

glimpse(ccard_df)

dev.new()
ggplot(ccard_df) +
  geom_histogram(aes(x = factor(day), fill = factor(Class)), stat = "count")

#' We can see if there are specific times when frauds roam around.

day_names <- c(`1` = "Day 1", `2` = "Day 2")

ggplot(ccard_df %>% filter(Class == 1)) +
  geom_histogram(aes(x = factor(hour)), stat = "count", fill = "black", color = "white") +
  facet_wrap(~day, ncol = 1, labeller = as_labeller(day_names)) +
  labs(x = "Hour",
       y = "Count",
       title = "Distribution of fraudulent activity by day")

#' Since this is all about class imbalance, lets look at the imbalance

ccard_df %>% 
  group_by(Class) %>% 
  summarize(class_count = n())

#' Very severely imbalanced data as is the distinguishing feature of any
#' fraud-detection data set
#'
#' Predicting with such severe class imbalance is difficult. Begin, with a
#' traditional approach

tr_rows <- createDataPartition(ccard_df$Class, p = 0.8, list = FALSE)

ccard_df <- select(ccard_df, -hour, -day)

ccard_train <- ccard_df[tr_rows, ]
ccard_test <- ccard_df[-tr_rows, ]

model_logit1 <- train(factor(Class) ~ .,
                      data = ccard_train,
                      method = "glm",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "repeatedcv",
                                               number = 5,
                                               repeats = 2),
                      family = binomial(link = "logit"))

model_logit1$results

model_logit2 <- train(factor(Class) ~ .,
                      data = ccard_train,
                      method = "glm",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "repeatedcv",
                                               number = 5,
                                               repeats = 2,
                                               sampling = "smote"),
                      family = binomial(link = "logit"))

model_logit2$results

model_logit3 <- train(factor(Class) ~ .,
                      data = ccard_train,
                      method = "glm",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "repeatedcv",
                                               number = 5,
                                               repeats = 2, 
                                               sampling = "down"),
                      family = binomial(link = "logit"))

model_logit3$results

model_logit4 <- train(factor(Class) ~ .,
                      data = ccard_train,
                      method = "glm",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "repeatedcv",
                                               number = 5,
                                               repeats = 2, 
                                               sampling = "up"),
                      family = binomial(link = "logit"))

model_logit4$results

model_penlogit4 <- train(factor(Class) ~ .,
                         data = ccard_train,
                         method = "glmnet",
                         preProcess = c("center", "scale"),
                         trControl = trainControl(method = "repeatedcv",
                                                  number = 5,
                                                  repeats = 2,
                                                  sampling = "smote"),
                      family = "binomial")

model_logit4$results
