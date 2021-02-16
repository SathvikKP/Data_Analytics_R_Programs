library(ISLR)
library(MASS)

dt <- Default
#print(head(dt))

index <- sample(x=1:nrow(dt),size=0.5*nrow(dt))
train <- dt[index,]
test <- dt[-index,]
print(" LDA ")
print(" ")
modellda <- lda(default~student+balance+income,data=train)
#print(summary(modellda))

predlda <- predict(modellda,test)
test$predlda <- predlda$class
#print(head(test))

print(table(test$predlda,test$default))
print(" ")
print("QDA")
print(" ")

dt$student = as.numeric(dt$student)
dt$student = as.factor(dt$student)

index <- sample(x=1:nrow(dt),size=0.5*nrow(dt))
train <- dt[index,]
test <- dt[-index,]

modelqda = qda(default~student+balance+income,data=train)
#print(summary(modelqda))

predqda <- predict(modelqda,test)
test$predqda <- predqda$class

print(table(test$predqda,test$default))