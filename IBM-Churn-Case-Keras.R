#' ---
#' title: "Predicting customer churn with Keras"
#' author: Pavan Gurazada
#' output: github_document
#' ---
#' last update: Sat Mar 10 14:17:24 2018
 
#' A telecommunications company is concerned about the number of customers
#' leaving their landline business for cable competitors. They need to
#' understand who is leaving. Imagine that you’re an analyst at this company and
#' you have to find out who is leaving and why.

library(keras)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)
library(tidyverse)

set.seed(20130810)
theme_set(theme_bw())

churn_data_raw <- read_csv("data/ibm-watson-data/WA_Fn-UseC_-Telco-Customer-Churn.csv")
glimpse(churn_data_raw)

#' Remove unnecessary data, move the target to the first column

churn_data_tbl <- churn_data_raw %>% select(-customerID) %>% 
                                     drop_na() %>% 
                                     select(Churn, everything())
glimpse(churn_data_tbl)

train_test_split <- initial_split(churn_data_tbl, prop = 0.8)
train_tbl <- training(train_test_split)
test_tbl <- testing(train_test_split)

dev.new()
ggplot(train_tbl, aes(x = tenure)) +
  geom_histogram(fill = "black", color = "white", bins = 70) + 
  labs(x = "Tenure (years)",
       title = "Distribution of tenure")

#' The above plot is okay

ggplot(train_tbl, aes(x = TotalCharges)) +
  geom_histogram(fill = "black", color = "white", bins = 100) + 
  labs(x = "Total Charges",
       title = "Distribution of total charges")

#' The above plot is indicative of a heavy-tailed distribution, do a
#' log-transform

ggplot(train_tbl, aes(x = log(TotalCharges))) +
  geom_histogram(fill = "black", color = "white", bins = 100) + 
  labs(x = "log(Total Charges)",
       title = "Distribution of log(total charges)")

#' Determine if the log transformation improves the correlation between total 
#' charges and churn

train_tbl %>% select(Churn, TotalCharges) %>% 
              mutate(Churn = Churn %>% as.factor() %>% as.numeric(),
                     LogTotalCharges = log(TotalCharges)) %>% 
              correlate() %>% 
              focus(Churn) 

#' One-hot encoding for multiple categories, build up the recipe for this

rec_obj <- recipe(Churn ~., data = train_tbl) %>% 
           step_discretize(tenure, options = list(cuts = 6)) %>% 
           step_log(TotalCharges) %>% 
           step_dummy(all_nominal(), -all_outcomes()) %>% 
           step_center(all_predictors(), -all_outcomes()) %>% 
           step_scale(all_predictors(), -all_outcomes()) %>% 
           prep(data = train_tbl)

X_train_tbl <- bake(rec_obj, newdata = train_tbl) %>% select(-Churn)
X_test_tbl <- bake(rec_obj, newdata = test_tbl) %>% select(-Churn)

glimpse(X_train_tbl)

y_train_vec <- ifelse(pull(train_tbl, Churn) == "Yes", 1, 0)
y_test_vec <- ifelse(pull(test_tbl, Churn) == "Yes", 1, 0)

#' Building the multi-layer perceptron

model_keras <- keras_model_sequential()

model_keras %>% layer_dense(units = 16,
                            kernel_initializer = "uniform",
                            activation = "relu",
                            input_shape = ncol(X_train_tbl)) %>% 
                layer_dropout(rate = 0.1) %>% 
                layer_dense(units = 16,
                            kernel_initializer = "uniform",
                            activation = "relu") %>% 
                layer_dropout(rate = 0.1) %>% 
                layer_dense(units = 1,
                            kernel_initializer = "uniform",
                            activation = "sigmoid") -> model_keras

compile(model_keras, 
        optimizer = "adam",
        loss = "binary_crossentropy",
        metrics = c("accuracy"))

fit_keras <- fit(model_keras,
                 x = as.matrix(X_train_tbl),
                 y = y_train_vec, 
                 batch_size = 50,
                 epochs = 35,
                 validation_split = 0.3)
dev.new()
plot(fit_keras) + labs(title = "Deep Learning training results")


model_keras %>% layer_dense(units = 24,
                            kernel_initializer = "uniform",
                            activation = "relu",
                            input_shape = ncol(X_train_tbl)) %>% 
                layer_dropout(rate = 0.1) %>% 
                layer_dense(units = 24,
                            kernel_initializer = "uniform",
                            activation = "relu") %>% 
                layer_dropout(rate = 0.1) %>% 
                layer_dense(units = 1,
                            kernel_initializer = "uniform",
                            activation = "sigmoid") -> model_keras

compile(model_keras, 
        optimizer = "adam",
        loss = "binary_crossentropy",
        metrics = c("accuracy"))

fit_keras <- fit(model_keras,
                 x = as.matrix(X_train_tbl),
                 y = y_train_vec, 
                 batch_size = 50,
                 epochs = 35,
                 validation_split = 0.3)
dev.new()
plot(fit_keras) + labs(title = "Deep Learning training results")




#' Making predictions

yhat_keras_class <- predict_classes(object = model_keras, 
                                    x = as.matrix(X_test_tbl)) %>% as.vector()

estimates_keras <- data.frame(truth = as.factor(y_test_vec) %>% fct_recode(yes = "1", no = "0"),
                              estimate = as.factor(yhat_keras_class) %>% fct_recode(yes = "1", no = "0"))

options(yardstick.event_first = FALSE)

estimates_keras %>% conf_mat(truth, estimate)
estimates_keras %>% metrics(truth, estimate)
estimates_keras %>% precision(truth, estimate)
estimates_keras %>% recall(truth, estimate)

class(model_keras)

corr_analysis <- X_train_tbl %>% mutate(Churn = y_train_vec) %>% 
                                 correlate() %>% 
                                 focus(Churn) %>% 
                                 rename(feature = rowname) %>% 
                                 arrange(abs(Churn)) %>% 
                                 mutate(feature = as.factor(feature))    

corr_analysis %>% ggplot(aes(x = Churn, y = fct_reorder(feature, desc(Churn)))) +
                    geom_point() +
                    # positive correlations contribute to churn
                    geom_segment(aes(xend = 0, yend = feature), 
                                 color = "red",
                                 data = corr_analysis %>% filter(Churn>0)) +
                    geom_point(color = "red",
                               data = corr_analysis %>% filter(Churn>0)) +
                    # negative correlations prevent churn
                    geom_segment(aes(xend = 0, yend = feature), 
                                 color = "black",
                                 data = corr_analysis %>% filter(Churn<0)) +
                    geom_point(color = "black",
                               data = corr_analysis %>% filter(Churn<0)) + 
                    
                    geom_vline(xintercept = 0, size = 1, linetype = 2) +
                    geom_vline(xintercept = 0.25, size = 1, linetype = 2) +
                    geom_vline(xintercept = -0.25, size = 1, linetype = 2) +
  
                    labs(title = "Churn correlation analysis",
                         y = "Feature Importance")
