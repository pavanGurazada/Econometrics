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
