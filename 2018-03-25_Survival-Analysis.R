#' ---
#' title: "Exploring survival analysis..."
#' author: Pavan Gurazada
#' output: github_document
#' ---
#' last update: Sun Mar 25 07:01:54 2018

library(survival)
library(ranger)
library(tidyverse)
library(ggfortify)
library(ggthemes)

theme_set(theme_few())

data("veteran")
glimpse(veteran)

km_obj <- Surv(veteran$time, veteran$status)
head(km_obj)

# model the probability of survival over time, with no predictors
km_fit <- survfit(Surv(time, status) ~ 1, data = veteran) 
summary(km_fit, times = c(1, 30, 60, 90 * 1:10))

dev.new()
autoplot(km_fit)

# model the probability of survival over time, across treatment groups
km_trt_fit <- survfit(Surv(time, status) ~ trt, data = veteran)
autoplot(km_trt_fit)

vet <- veteran %>% mutate(AG = ifelse(age < 60, "LT60", "GT60"),
                          AG = factor(AG),
                          trt = factor(trt, labels = c("Standard", "Test")),
                          prior = factor(prior, labels = c("No", "Yes")))
km_ag_fit <- survfit(Surv(time, status) ~ AG, data = vet)
autoplot(km_ag_fit) + labs(x = "Time", 
                           y = "Probability of survival")

cox_fit <- coxph(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior,
                 data = vet)
summary(cox_fit)

autoplot(survfit(cox_fit))

aa_fit <- aareg(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior,
                data = vet)
aa_fit

autoplot(aa_fit)
