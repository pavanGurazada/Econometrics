#' ---
#' title: "Analysis of binary choice models "
#' author: Cameron and Trivedi, Microeconometrics
#' output: github_document
#' ---

#' last update: Thu Apr 12 11:36:09 2018

library(tidyverse)
library(caret)
library(Ecdat) # required for the data set

fishing_df <- Fishing

glimpse(fishing_df)

#' The binary dependent variable is y = 1 of fishing from a charter boat and y =
#' 0, if fishing from a pier
#'
#' They consider a single predictor - the relative price of charter fish to
#' those collected form the pier. Every individual has a choice to fish from a
#' charter boat and the pier. As the relative price of the charter increases, it
#' is expected to be a less attractive option
#' 

fishing_bin_df <- fishing_df %>% 
                    filter(mode == "charter" | mode == "pier") %>% 
                    mutate(charter = if_else(mode == "charter", 1, 0),
                           lnrelp = log(pcharter/ppier)) %>% 
                    select(charter, everything())

glimpse(fishing_bin_df)

fishing_bin_df %>% 
  group_by(mode) %>% 
  summarize(n = n(),
            price_charter = mean(pcharter),
            price_pier = mean(ppier),
            avg_lnrelp = mean(lnrelp))

#' Running the formal logit and probit models

tr_rows <- createDataPartition(fishing_bin_df$charter, p = 0.8, list = FALSE)
fishing_train <- fishing_bin_df[tr_rows, ]
fishing_test <- fishing_bin_df[-tr_rows, ]

model_logit <- train(factor(charter) ~ lnrelp,
                     data = fishing_train,
                     method = "glm",
                     trControl = trainControl(method = "repeatedcv",
                                              number = 10,
                                              repeats = 5),
                     family = binomial(link = "logit"))

model_logit$results

summary(model_logit)

model_probit <- train(factor(charter) ~ lnrelp,
                      data = fishing_train,
                      method = "glm",
                      trControl = trainControl(method = "repeatedcv",
                                              number = 10,
                                              repeats = 5),
                      family = binomial(link = "probit"))

model_probit$results

summary(model_probit)

confusionMatrix(predict(model_logit, fishing_test), fishing_test$charter)
confusionMatrix(predict(model_probit, fishing_test), fishing_test$charter)
