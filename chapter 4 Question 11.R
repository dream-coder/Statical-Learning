library(MASS)
library(ISLR)
library(CLASS)
Auto$mpg01 = ifelse(Auto$mpg>median(Auto$mpg),1,0) 
summary(Auto$mpg01)
pairs(Auto)
attach(Auto)
#making train and test set
train = (year%%2 == 0)
train.auto = Auto[train,]
test.auto = Auto[!train,]
mpg01.test = mpg01[!train]

#LDA

lda.fit = lda(mpg01~horsepower+weight+cylinders+displacement, data= Auto, subset=train)
lda.pred = predict(lda.fit, newdata=test.auto)
table(lda.pred$class, mpg01.test)
mean(lda.pred$class!=mpg01.test)


#QDA

qda.fit = qda(mpg01~horsepower+cylinders+displacement+weight, data=Auto, subset=train)
qda.pred = predict(qda.fit, newdata = test.auto)
table(qda.pred$class,mpg01.test)
mean(qda.pred$class!=mpg01.test)

#Logistic regression
glm.fit2 = glm(mpg01~horsepower+cylinders+displacement+weight, data= Auto, family= binomial, subset= train)
glm.prob = predict(glm.fit2, newdata=test.auto, type="response")
glm.pred = ifelse(glm.prob>0.5,1,0)
table(glm.pred, mpg01.test)
mean(glm.pred!=mpg01.test)

#KNN
train.x = cbind(cylinders,weight,displacement,horsepower)[train,]
test.x = cbind(cylinders,weight,displacement,horsepower)[!train,]
train.mpg01 = mpg01[train]
#k=1
knn.pred = knn(train.x, test.x, train.mpg01, k=1)
mean(knn.pred!=mpg01[!train])
#k=5
knn.pred = knn(train.x, test.x, train.mpg01, k=5)
mean(knn.pred!=mpg01[!train])
#k=10
knn.pred = knn(train.x, test.x, train.mpg01, k=10)
mean(knn.pred!=mpg01[!train])
#k=100
knn.pred = knn(train.x, test.x, train.mpg01, k=100)
mean(knn.pred!=mpg01[!train])
