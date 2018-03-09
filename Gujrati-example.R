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
