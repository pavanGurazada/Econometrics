#' ---
#' title: "Exploring tree-based methods for regression"
#' author: Pavan Gurazada
#' date: "February 218"
#' output: github_document
#' ---

library(ISLR) 
library(MASS)

library(tidyverse)
library(caret)

library(doParallel)
library(parallel)
clusters <- makeCluster(detectCores() - 1)
registerDoParallel(clusters)


#' At the core of regression trees is the concept of splitting the parameter
#' space such that the mean square error is minimized. As a starting point, one
#' might predict the outcome for all points in a region by the mean observed
#' outcome in the region and use this to calculate the mean squared error.
#'
#' Tree-based methods iteratively split the parameter space using one predictor
#' at a time to minimize the overall mean squared error.
#'
#' The tree-building is stopped according to the usual criterion of
#' cross-validation accuracy. However, executing this on large splits of data is
#' computationally intensive, so *pruning* is conducted instead, where a set of
#' subtrees is selected and validation is conducted. The complexity of the tree
#' is selected by penalizing large number of branches explicitly. This is
#' similar to the penalties imposed on linear models to improve predictive
#' performance. The penalty $\alpha$ controls the cost of complexity attached to
#' the model (similar to $\lambda$ and $\alpha$ for the elastic net model).
#'
#' Several methods correct the sensitivity of tree-based methods, for e.g.,
#' bagging, random forests and bagging. We look at each in turn.
#'
#' *Bagging* In this method several bootstrap samples are taken from the
#' original sample and average the model estimates from these bootstrap samples.
#'
#' bagging = bootstrapped aggregation
#'
#' Generate B bootstrap samples, fit regression trees to each of these samples
#' and finally average the predictions from these trees. Bootstrapping from the
#' data also automatically generates the out-of-bag samples which can be used
#' for cross-valdiation.
#'
#' The interpretability of the model suffers due to the averaging process, but
#' variable importance plots can be used to judge the relative importance of the
#' features.
#'
#' *Random Forests* A small tweak with a lot of impact. On each bootstrap sample
#' a random sample of features is chosen as the candidate set for splits from
#' the full set of features. This random sample is drawn at each split. This
#' crazy tactic corrects for the highly correlated predictions from bagging by
#' forcing the model to choose from among possibly less predictive features as
#' well.
#' 
#' *Boosting* In this approach, trees are grown sequentially building on the 
#' positives of the previously grown ones. There is no bootstrap sampling, each
#' tree is fit on a modification of the original set. There are three parameters
#' to consider here - number of trees ($B$), shrinkage parameter ($\lambda$), 
#' number of splits in each tree ($d$). The original data is modified after
#' each fit by adding in the residuals from the previous fit.

head(Carseats)
Carseats$High <- ifelse(Carseats$Sales <= 8, "No", "Yes")
head(Carseats)

#' data set is too tiny to do a test-train split

logitFit <- train(High ~ . - Sales,
               data = Carseats,
               method = "glm",
               trControl = trainControl(method = "repeatedcv",
                                        number = 10,
                                        repeats = 5))

summary(logitFit)
confusionMatrix(logitFit)

rfFit <- train(High ~ . - Sales,
               data = Carseats,
               method = "rf")

varImp(rfFit)
confusionMatrix(rfFit)

#' Simple logistic regression did better than random forests on this toy data.
#' Not to a more reasonable data set.

data("Boston")
glimpse(Boston)

trainingRows <- createDataPartition(Boston$medv, p = 0.8, list = FALSE)
bostonTrain <- Boston[trainingRows, ]
bostonTest <- Boston[-trainingRows, ]

lmFit <- train(medv ~ .,
               data = bostonTrain,
               method = "lm",
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "repeatedcv",
                                        number = 10,
                                        repeats = 5))

print(lmFit)

rfFit <- train(medv ~ .,
               data = bostonTrain,
               method = "rf",
               tuneLength = 10, 
               ntree = 1000)
print(rfFit)

xgboostFit <- train(medv ~ .,
                    data = bostonTrain,
                    method = "xgbTree")
print(xgboostFit)

#' gradient boosted tree does better.

xgboostFit2 <- train(medv ~ .,
                     data = bostonTrain,
                     method = "xgbTree",
                     tuneGrid = expand.grid(nrounds = 350,
                                            max_depth = 1:6,
                                            eta = c(0.3, 0.4, 0.5),
                                            gamma = 0.01,
                                            colsample_bytree = 0.75,
                                            subsample = 0.5,
                                            min_child_weight = 0))
print(xgboostFit2$results)

#' Lets take a closer look at random forests and gradient boosted methods using 
#' bigger examples

cnames <- c("age", "workclass", "fnlwgt", "education", "education_num",
            "marital_status", "occupation", "relationship", "race",
            "sex", "capital_gain", "capital_loss", "hours_per_week",
            "native_country", "target")

adultTrain <- read.table("data/general/adult.data", header = FALSE, sep = ",",
                         col.names = cnames, na.strings = c(" ?"),
                         stringsAsFactors = FALSE)

adultTest <- read.table("data/general/adult.test", header = FALSE, sep = ",",
                        col.names = cnames, skip = 1, na.strings = c(" ?"),
                        stringsAsFactors = FALSE)
glimpse(adultTrain)
glimpse(adultTest)

table(is.na(adultTrain))
table(is.na(adultTest))

#' Both have missing data. We can incorporate this into modeling
#'  Check for class imbalance

adultTrain %>% group_by(target) %>% summarize(Fraction = n()/nrow(.))
adultTest %>% group_by(target) %>% summarize(Fraction = n()/nrow(.))

adultTrain <- adultTrain %>% mutate_if(is.character, as.factor)
adultTest <- adultTest %>% mutate_if(is.character, as.factor)

adultRF <- train(target ~ .,
                 data = adultTrain,
                 method = "xgbTree",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 1,
                                          repeats = 1, 
                                          verboseIter = TRUE),
                 na.action = na.omit,
                 allowParallel = TRUE) # For now handle missing values by removing them

