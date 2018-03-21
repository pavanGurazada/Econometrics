#' ---
#' title: "Tuning Random Forests"
#' author: Pavan Gurazada
#' output: github_document
#' ---

library(caret)
library(recipes)
library(rsample)
library(tidyverse)
library(mlbench)

data("Sonar")

glimpse(Sonar)

sonar_data <- Sonar %>% drop_na() %>% 
                        select(Class, everything())

train_test_split <- initial_split(sonar_data, prop = 0.8)
train_tbl <- training(train_test_split)
test_tbl <- testing(train_test_split)

#' There are no categorical variables, one-hot encoding is not necessary
#' we need to only scale and center our data (well, not really since random 
#' forests does not care) as good practise

rec_obj <- recipe(Class ~ ., data = train_tbl) %>% 
           step_center(all_predictors(), -all_outcomes()) %>% 
           step_scale(all_predictors(), -all_outcomes()) %>% 
           prep(data = train_tbl)

X_train_tbl <- bake(rec_obj, newdata = train_tbl) %>% select(-Class)
X_test_tbl <- bake(rec_obj, newdata = test_tbl) %>% select(-Class)

glimpse(X_train_tbl)
y_train_vec <- pull(train_tbl, Class)
y_test_vec <- pull(test_tbl, Class)

#' In tuning random forests, two parameters impact the analysis most - number 
#' of variables randomly sampled at each split and number of trees to grow.
#' 
#' The baseline recommended is to consider sqrt of number of predictors and 
#' 500 trees. We use this as a starting point

set.seed(20130810)

rf_base <- train(X_train_tbl, y_train_vec,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = expand.grid(.mtry = floor(sqrt(ncol(X_train_tbl)))),
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10,
                                          repeats = 5))

print(rf_base)
rf_base$results # Pretty bad variance on the accuracy


#' The effect of increasing the number of trees in the forest improves the
#' situation only up to a point. In that sense, the only parameter in the random
#' forest algorithm that is tunable is the number of trees.

#' One way to search for the best parameter setting is to do it randomly

rf_random <- train(X_train_tbl, y_train_vec,
                   method = "rf",
                   metric = "Accuracy",
                   tuneLength = 15,
                   trControl = trainControl(method = "repeatedcv",
                                            number = 10,
                                            repeats = 3,
                                            search = "random"))

dev.new()
plot(rf_random)

rf_random$results
print(rf_random)

#' Another way is to employ grid search

rf_gridsearch <- train(X_train_tbl, y_train_vec,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = expand.grid(.mtry = 1:15),
                       trControl = trainControl(method = "repeatedcv",
                                                number = 10,
                                                repeats = 3,
                                                search = "grid"))

dev.new()
plot(rf_gridsearch)

#' Of course we can manually set the tuneGrid parameter

model_list <- list()

for (n_tree in c(500, 1000, 1500)) {
  set.seed(20130810)
  rf_fit <- train(X_train_tbl, y_train_vec,
                  method = "rf",
                  metric = "Accuracy",
                  tuneGrid = expand.grid(.mtry = c(2, 3)),
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,
                                           repeats = 3,
                                           search = "grid"),
                  ntree = n_tree)
  
  key <- toString(n_tree)
  
  model_list[[key]] <- rf_fit
  
}
 
model_list[["500"]]$results
model_list[["1000"]]$results
model_list[["1500"]]$results

#' Always take into account the variance in the results before making any calls
#' on the optimum parameter settings

