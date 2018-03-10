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

set.seed(20130810)
theme_set(theme_bw())

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

lm_cps2 <- train(wage ~ female + nonwhite + union + education + exper + female:nonwhite,
                 data = train_cps,
                 method = "lm",
                 trControl = trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 5))
summary(lm_cps2)

lm_cps3 <- train(wage ~ female + nonwhite + union + education + exper + 
                        female:education + female:exper + nonwhite:education,
                 data = train_cps,
                 method = "lm",
                 trControl = trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 5))
summary(lm_cps3)

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

summary(lm(outputstar ~ laborstar + capitalstar, data = cd_usa))
cd_model1 <- train(scale(output) ~ labor + capital, 
                   data = cd_usa,
                   method = "lm",
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "boot", number = 100))
summary(cd_model1)

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
summary(lm(sfdho ~ expend + expend2, data = food_expend))

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

#' *Example 5*

corruption <- read.dta("data/gujrati-example/Stata/Table2_18.dta")
glimpse(corruption)

corruption_model1 <- train(index ~ country + gdp_cap,
                           data = corruption,
                           method = "rf")
corruption_model1

corruption_model2 <- train(index ~ country * gdp_cap,
                           data = corruption,
                           method = "rf")
corruption_model2

#' *Example 6* Relationship between Gross Private Investments (GPI) and Gross
#' Personal Savings (GPS). How much of the savings do people tend to invest?

si_usa <- read.dta("data/gujrati-example/Stata/Table3_6.dta")
glimpse(si_usa)

summary(lm(gpi ~ gps, data = si_usa))

#' 1981-82 was the worst recession for peace-time US. We can include this as a
#' structural dummy using the variable recession81 = 1 for observations beyond
#' 81 and 0 otherwise

summary(lm(gpi ~ gps + recession81, data = si_usa))
summary(lm(gpi ~ gps + recession81 + gpsrec81, data = si_usa))

#' Results dont tally with the book but an interesting application of dummy
#' variables to detect structural changes. This is especially true for
#' time-series

#' *Example 7*
#' Deseasonalizing trends with dummy variables

sales_data <- read.dta("data/gujrati-example/Stata/Table3_10.dta")
glimpse(sales_data)

summary(sales_model <- lm(sales ~ d2 + d3 + d4, data = sales_data))

(sales_data$sales_adj <- mean(sales_data$sales) + residuals(sales_model))

dev.new()
ggplot(data = sales_data, aes(x = yearq, y = sales)) +
  geom_line(aes(linetype = "Original data")) +
  geom_line(aes(y = sales_adj, linetype = "Deseasonalized")) +
  labs(x = "Year",
       y = "Sales",
       linetype = "Sales")

#' As can be seen in the plot above, some of the variation in sales is
#' attributed to the variation due to the demand associated with change in
#' quarters. The underlying pattern beyond this seasonal variation can be
#' explored using this kind of a regression analysis.

summary(sales_model2 <- lm(sales ~ rpdi + conf + d2 + d3 + d4, 
                           data = sales_data))

sales_data$sales_adj <- mean(sales_data$sales) + residuals(sales_model2)

ggplot(data = sales_data, aes(x = yearq)) +
  geom_line(aes(y = sales, linetype = "Original Data")) +
  geom_line(aes(y = sales_adj, linetype = "Deseasonalized")) +
  labs(x = "Year",
       y = "Sales",
       linetype = "Sales")

summary(sales_model3 <- lm(sales ~ rpdi + conf + d2 + d3 + d4 + 
                                   d2:rpdi + d3:rpdi + d4:rpdi +
                                   d2:conf + d3:conf + d4:conf,
                           data = sales_data))

#' From the above table, the lack of significance of the coefficients show that
#' there is no seasonal variation in rpdi and conf

summary(sales_model4 <- lm(log(sales) ~ rpdi + conf + d2 + d3 + d4, 
                           data = sales_data))

#' Verifying Frisch-Waugh theorem

summary(s1 <- lm(sales ~ d2 + d3 + d4, data = sales_data))
summary(s2 <- lm(rpdi ~ d2 + d3 + d4, data = sales_data))
summary(s3 <- lm(conf ~ d2 + d3 + d4, data = sales_data))

rs1 <- residuals(s1)
rs2 <- residuals(s2)
rs3 <- residuals(s3)

summary(lm(rs1 ~ rs2 + rs3 - 1))
summary(lm(sales ~ rpdi + conf + d2 + d3 + d4, data = sales_data))

#' As can be seen from these regression outputs, the coefficients of rs2 and rs3
#' in the first regression and rpdi and conf in the second regression are the
#' same. This is nice since you do not need to account for seasonal variations
#' in the outcome and the predictors individually. Usage of dummies achieves two
#' tasks at once

summary(lm(sales ~ rpdi + conf + d2 + d3 + d4 + trend, data = sales_data))

#' *Example 8*
#' 

diabetes_data <- read.dta("data/gujrati-example/Stata/Table3_19.dta")
glimpse(diabetes_data)

summary(lm(diabetes ~ ban * sugar_sweet_cap, diabetes_data))

#' *Example 9*
#' Dependence of working women's hours of work; data is collected for 753 women
#' in 1975

wwmn_data <- read.dta("data/gujrati-example/Stata/Table4_4.dta")
glimpse(wwmn_data)

summary(w_model1 <- lm(hours ~ age + educ + exper + faminc + fathereduc + hage + 
                               heduc + hhours + hwage + kids618 + kidsl6 + wage 
                               + mothereduc + mtr + unemployment, 
                       data = wwmn_data))

w_corrs <- wwmn_data %>% select(-hours) %>% cor()

bad_preds <- findCorrelation(w_corrs, cutoff = 0.75, verbose = TRUE)

wwmn_cleaned <- wwmn_data %>% select(-hours) %>% 
                              select(-bad_preds) %>%
                              mutate(hours = wwmn_data$hours)
glimpse(wwmn_cleaned)

summary(w_model2 <- lm(hours ~ educ + exper + faminc + fathereduc + hage + 
                               heduc + hhours + hwage + kids618 + kidsl6 + wage 
                               + mothereduc + unemployment, 
                       data = wwmn_cleaned))

#' Often number of variables in econometric studies are handpicked and hence
#' multicollinearity should not be a reason for exclusion. There should be valid
#' arguments that drive that decision. This is a very subjective call dependent 
#' on the information available to the analyst

summary(w_model3 <- lm(hours ~ age + educ + exper + faminc + hhours + hwage + 
                               kidsl6 + wage + mtr + unemployment, 
                       data = wwmn_data))

#' *Example 10*

manpower <- read.dta("data/gujrati-example/Stata/Table4_11.dta")
glimpse(manpower)

x_corr <- manpower %>% select(-y) %>% cor()
findCorrelation(x_corr, cutoff = 0.75, verbose = TRUE, names = TRUE)

#' *Example 11*
#' What factors determine abortion rate across the 50 states in USA?

abortion_data <- read.dta("data/gujrati-example/Stata/Table5_1.dta")
glimpse(abortion_data)



