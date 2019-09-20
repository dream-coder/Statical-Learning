library(ISLR)
library(MASS)
library(class)
dim(Weekly)
attach(Weekly)
summary(Weekly)
pairs(Weekly)
cor(Weekly[,-9])
#Year and volume appears to have pattern
set.seed(1)
glm.fit = glm(Direction~.-Year-Today, data= Weekly, family = binomial)
summary(glm.fit)
#Lag2 appears to have some statistical significanc
glm.probs = predict(glm.fit, type="response")
glm.pred = rep("Down",1089)
glm.pred[glm.probs>0.5] = "Up"
summary(glm.pred)
glm.pred
table(glm.pred, Direction)
mean(glm.pred==Direction)
#weeks the market goes up, prediction is right 92% of time{557/(557+48)}
#weeks the market goes down, prediction is right 11% of time{54/(54+430)}
train = Year<2009
glm.fit1 = glm(Direction~Lag2, data= Weekly, family= binomial ,subset= train)
glm.probs1 = predict(glm.fit1, newdata= Weekly[!train,], type= "response")
glm.pred = ifelse(glm.probs1>0.5,"Up","Down")
table(glm.pred,Direction[!train])
mean(glm.pred==Direction[!train])
#LDA
lda.fit = lda(Direction~Lag2, data= Weekly, subset= train)
lda.pred = predict(lda.fit, newdata= Weekly[!train,])
table(lda.pred$class,Direction[!train])
mean(lda.pred$class==Direction[!train])

#QDA

qda.fit = qda(Direction~Lag2, data =  Weekly, subset=train)
qda.pred = predict(qda.fit, newdata=Weekly[!train,])
table(qda.pred$class, Direction[!train])
mean(qda.pred$class==Direction[!train])

#KNN
#knn(train, test, cl, k = 1)
train.x = as.matrix(Lag2[train]) 
test.x = as.matrix(Lag2[!train])
set.seed(1)
knn.pred = knn(train.x,test.x, Direction[train], k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])

#10E
glm.fit1 = glm(Direction~Lag1*Lag2, data= Weekly,family= binomial, subset=train)
summary(glm.fit1)
glm.probs2 = predict(glm.fit1, newdata= Weekly[!train,], type="response")
glm.pred = ifelse(gl.probs2>0.5,"Up","Down")
table(glm.pred,Direction[!train])
mean(glm.pred==Direction[!train])

#LDA
lda.fit1 = lda(Direction~Lag1*Lag2, data= Weekly, subset= train)
lda.pred1 = predict(lda.fit1, newdata = Weekly[!train,])
table(lda.pred1$class, Direction[!train])
mean(lda.pred1$class==Direction[!train])

#QDA
qda.fit1 = qda(Direction~Lag2 + sqrt(abs(Lag2)), data= Weekly, subset=train)
qda.pred1 = predict(qda.fit1, newdata= Weekly[!train,])
table(qda.pred1$class, Direction[!train])
mean(qda.pred1$class==Direction[!train])

