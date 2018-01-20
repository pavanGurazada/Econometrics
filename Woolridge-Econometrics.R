library(tidyverse)
library(caret)

# Chapter 1
# Exercise C1

wage1 <- get(load("Data/wage1.RData"))
glimpse(wage1)

mean(wage1$educ); min(wage1$educ); max(wage1$educ)

# Exercise C2

bwght <- get(load("Data/bwght.RData"))
glimpse(bwght)

female <- ifelse(bwght$male == 0, 1, 0); sum(female)

mean(bwght$packs); mean(bwght$cigs)

bwght %>% group_by(male) %>% summarize(mean(cigs))

length(bwght$fatheduc) - sum(is.na(bwght$fatheduc))

mean(bwght$faminc); sd(bwght$faminc)

# Exercise C3

meap01 <- get(load("Data/meap01.RData"))
glimpse(meap01)

range(meap01$math4)

meap01 %>% filter(math4 == 50) %>% count()
sum(ifelse(meap01$math4 == 50, 1, 0))

mean(meap01$math4); mean(meap01$read4)
mean(meap01$exppp); sd(meap01$exppp)

# Exercise C4

jtrain2 <- get(load("Data/jtrain2.RData"))
glimpse(jtrain2)

jtrain2 %>% group_by(train) %>% summarize(Number = length(train))

jtrain2 %>% group_by(train) %>% summarize(Avg78Earnings = mean(re78))

jtrain2 %>% group_by(train) %>% summarize(FracUnemployed = sum(unem78)/length(train))

# Exercise C5

fertil2 <- get(load("Data/fertil2.RData"))
glimpse(fertil2)

range(fertil2$children); mean(fertil2$children)

fertil2 %>% group_by(electric) %>% count()

fertil2 %>% group_by(electric) %>% summarize(AvgChildren = mean(children))

# Chapter 2

# Exercise C1

wage <- get(load("Data/401k.RData"))
glimpse(wage)

mean(wage$prate); mean(wage$mrate)

summary(wageModel <- lm(prate ~ mrate, data = wage))
plot(wageModel)

trainingRows <- createDataPartition(wage$prate, p = 0.8, list = FALSE)
trainData <- wage[trainingRows, ]
testData <- wage[-trainingRows, ]

wageModel <- train(prate ~ mrate,
                   data = trainData,
                   method = "lm",
                   trControl = trainControl(method = "cv", number = 10))

# Exercise C2

ceoSal <- get(load("Data/ceosal2.RData"))
glimpse(ceoSal)

summary(ceoModel <- lm(salary ~ ceoten, data = ceoSal))
plot(ceoModel)

# Exercise C3

sleep75 <- get(load("Data/sleep75.RData"))
glimpse(sleep75)

summary(sleepModel <- lm(sleep ~ totwrk, data = sleep75))
