#' ---
#' title: "Econometrics by example"
#' author: Pavan Gurazada
#' output: github_document
#' ---
#' last update: Fri Mar 09 10:50:26 2018

library(foreign)
library(tidyverse)
library(caret)
library(boot)

#' Though heavily biased towards Linear Regression, Econometrics deals with
#' specifying the regression problems rooted on economic theory. The regression
#' coefficient estimates and the sign of these estimates are derived from
#' theoretical considerations. This is a unique playground and several
#' interesting examples of economic problems are presented in this book, which
#' we intend to replicate in R

#' *Example 1*

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
#' Rubbish R^2 - features are missing

#' *Example 2*

cd_usa <- read.dta("data/gujrati-example/Stata/Table2_1.dta")
glimpse(cd_usa)

summary(lm(lnoutput ~ lnlabor + lncapital, data = cd_usa))

#' This produces a highly explanatory model for the output of the state,
#' dependent on the labor and capital as inputs
#' 
#' Since the sample size is small, a bootstrap might be used to circomvent the 
#' requirement of normality for standard errors

lm_stat <- function(data, i) {
  lm_model <- lm(lnoutput ~ lnlabor + lncapital, data[i, ])
  return(coef(lm_model))
}

boot_cb <- boot(cd_usa, statistic = lm_stat, R = 1000)

#' Running the restricted version of the regression

summary(lm(lnoutlab ~ lncaplab, data = cd_usa))

#' *Example 3*

gdp_us <- read.dta("data/gujrati-example/Stata/Table2_5.dta")
glimpse(gdp_us)

summary(lm(lnrgdp ~ time, data = gdp_us))
summary(lm(rgdp ~ time + time2, data = gdp_us))
summary(lm(lnrgdp ~ time + time2, data = gdp_us))

#' *Example 4*

food_expend <- read.dta("data/gujrati-example/Stata/Table2_8.dta")
glimpse(food_expend)

summary(lm(sfdho ~ lnexpend, data = food_expend))
summary(lm(sfdho ~ I(1/expend), data = food_expend))

#' When there is little to choose between these two models in terms of R^2,
#' cross-validation might provide an answer for which model to choose. The model
#' with better predictive power wins

tr_rows <- createDataPartition(food_expend$sfdho, p = 0.8, list = FALSE)
food_expend_train <- food_expend[tr_rows, ]
food_expend_test <- food_expend[-tr_rows, ]

food_model1 <- train(sfdho ~ lnexpend, 
                     data = food_expend_train,
                     method = "lm",
                     preProcess = c("center", "scale"),
                     trControl = trainControl(method = "repeatedcv", 
                                              number = 10, 
                                              repeats = 5))
food_model2 <- train(sfdho ~ I(1/expend), 
                     data = food_expend_train,
                     method = "lm",
                     preProcess = c("center", "scale"),
                     trControl = trainControl(method = "repeatedcv", 
                                              number = 10, 
                                              repeats = 5))

food_model1
food_model2

#' model 1 is better. When multiple models fit the data closely, I tend to lean
#' towards the model that offers a richer interpetation, even if it has lesser
#' predictive power



