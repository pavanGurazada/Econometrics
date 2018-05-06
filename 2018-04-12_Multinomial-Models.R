#' ---
#' title: "Multinomial choice models "
#' author: Cameron and Trivedi
#' output: github_document
#' ---

#' last update: Thu Apr 12 15:42:05 2018
#' 

library(tidyverse)
library(caret)
library(Ecdat) # For the fishing data

fishing_df <- Fishing

glimpse(fishing_df)

#' Data summary

fishing_df %>% 
  group_by(mode) %>% 
  summarise_all(mean)

tr_rows <- createDataPartition(fishing_df$mode, p = 0.8, list = FALSE)
fishing_train <- fishing_df[tr_rows, ]
fishing_test <- fishing_df[-tr_rows, ]

model_multinom <- train(mode ~ pbeach + ppier + pboat + pcharter + cbeach + 
                               cpier + cboat + ccharter,
                        data = fishing_train,
                        method = "multinom",
                        trControl = trainControl(method = "boot", 
                                                number = 500,
                                                allowParallel = TRUE))

model_multinom$results

summary(model_multinom)

