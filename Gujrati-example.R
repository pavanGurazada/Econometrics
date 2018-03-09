#' ---
#' title: "Econometrics by example"
#' author: Pavan Gurazada
#' output: github_document
#' ---
#' last update: Fri Mar 09 10:50:26 2018

library(foreign)
library(tidyverse)
library(caret)

#' Example 1

cps <- read.dta("data/gujrati-example/Stata/Table1_1.dta")
glimpse(cps)

tr_rows <- createDataPartition(cps$wage, p = 0.8, list = FALSE)
train_cps <- cps[tr_rows, ]
test_cps <- cps[-tr_rows, ]

lm_cps <- train(wage ~ female + nonwhite + union + education + exper,
                data = train_cps,
                method = "lm",
                trControl = trainControl(method = "repeatedcv",
                                         number = 10, 
                                         repeats = 5))
summary(lm_cps)










#' This shows a pretty rubbish R2