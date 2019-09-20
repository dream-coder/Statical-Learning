library(ISLR)
library(MASS)

library(class)
names(Boston)
attach(Boston)
pairs(Boston)
cor(Boston)
Boston$crime01 = ifelse(Boston$crim>median(Boston$crim),1,0)
 #Logistic
dim(Boston)
names(Boston)
summary(Boston)
train = 1:300
dim(Boston[train,])
glm.fit = glm(crime01~.-crim-crime01, data=Boston, family= binomial, subset=train)
glm.probs = predict(glm.fit, newdata=Boston[-train,], type="response")
glm.pred = ifelse(glm.probs>0.5,1,0)
mean(glm.pred!=Boston$crime01[-train])

glm.fit1 = glm(crime01~indus+nox+rm+age+tax+ptratio+lstat,data=Boston ,family=binomial, subset=train)
glm.probs1 = predict(glm.fit1, newdata = Boston[-train,], type="response")
glm.preds1 = ifelse(glm.probs1>0.5,1,0)
mean(glm.preds1!=Boston$crime01[-train])
