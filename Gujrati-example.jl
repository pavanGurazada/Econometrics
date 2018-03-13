using DataFrames, GLM, RCall, Distributions

srand(20130810)

R"""
library(foreign)
library(tidyverse)
library(caret)
"""

R"""
cps <- read.dta("data/gujrati-example/Stata/Table1_1.dta")

"""

@rget cps

wage_model = lm(@formula(wage ~ female + nonwhite + union + education + exper), cps)

print(coef(wage_model))
print(r2(wage_model))
print(wage_model)


R"""
cb_usa <- read.dta("data/gujrati-example/Stata/Table2_1.dta")

"""

@rget cb_usa

model_cb = lm(@formula(lnoutput ~ lnlabor + lncapital), cb_usa)
print(model_cb)
print(r2(model_cb))
