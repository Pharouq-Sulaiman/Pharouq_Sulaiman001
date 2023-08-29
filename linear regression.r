#linear/logistic regression\
library(tidyverse)
library(UsingR)
library(manipulate)
library(broom)
library(BSDA)
library(ggfortify)
library(caret)
mode <- lm(mpg~ wt, mtcars)
mode
summary(mode)
plot(mode$residuals)
plot(mode$fitted.values)
ggplot(mode,aes(x=mode$fitted.values, y=mode$residuals))+
  geom_point()+ geom_hline(yintercept = 0)
plot(mtcars$wt,mtcars$mpg)
ggplot(mtcars,aes(wt,mpg))+geom_point()+geom_smooth(method = lm)


#;logistic regression
churning <- read.csv("C:/Users/PC-040/Documents/data_science/Churn_dataset.csv")
churning1 <- churning%>%
  dplyr::select(Churn, MonthlyCharges, tenure)
churning1$Churn <- ifelse(churning1$Churn=="No",1,0)
mode2 <- glm(Churn~tenure+MonthlyCharges, data=churning1, family=binomial)
mode2
summary(mode2)

lmchurn <- lm(churning1$Churn~churning1$tenure)
summary(lmchurn)
lmchurn$fitted.values
logregchurn <- glm(churning1$Churn~churning1$tenure, family = binomial)
summary(logregchurn)
plot(churning1$tenure, logregchurn$fitted.values)
logregchurn$fit <- ifelse(logregchurn$fitted.values >= 0.55,1,0)
churning2 <- data.frame(churning1$Churn, logregchurn$fitted.values)
churning3 <- churning2[grepl(1, churning1$Churn),]
mean(churning3$logregchurn.fitted.values)
confusionMatrix(logregchurn$fit, churning1$Churn)
logregchurn$fit <- as.factor(logregchurn$fit)
churning1$Churn <- as.factor(churning1$Churn)
intrain24<- createDataPartition(y= churning1$Churn, p=0.75, list = FALSE )
intrain24
churning1
unique(churning1)
glimpse(churning1)
training <-churning1[intrain,]
testing <- churning1[-intrain,]
testing$Churn <- as.factor(testing$churn)
training$Churn<- factor(training$churn)
mode24 <- glm(Churn~ tenure, data = training, family = binomial)
prediction <- predict(mode24,newdata=testing, type = "response")
confusionMatrix(predictions1,testing$Churn)
predictions1 <- as.factor(predictions1)
predictions1 <- ifelse(prediction >= 0.55,1,0)
confusionMatrix(logregchurn$fit, churning1$Churn)
