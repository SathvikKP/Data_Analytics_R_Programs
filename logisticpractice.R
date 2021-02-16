#logistic

#******load dataset*******#
library(ISLR)
data <- Default
#print(head(data))

#*******normalize*******#
max_bal <- max(data$balance)
min_bal <- min(data$balance)
max_inc <- max(data$income)
min_inc <- min(data$income)

yesnoprocess1 <- function(x) {if (x=="Yes"){return (1)} else {return (0)}}
balprocess <- function(x) { return ((x-min_bal)/(max_bal-min_bal))}
incprocess <- function(x) { return ((x-min_inc)/(max_inc-min_inc))}

data$default <- sapply(data$default,yesnoprocess1)
data$student <- sapply(data$student,yesnoprocess1)
data$balance <- sapply(data$balance,balprocess)
data$income <- sapply(data$income,incprocess)

#print(head(data))

#******** split into test and train ********#

train_ind = sample.int(nrow(data),0.75*nrow(data))
train = data[train_ind,]
test = data[-train_ind,]
#print(paste0(str(train),str(test)))
x_train = train[,-1]
y_train = train[,1]
x_test = test[,-1]
y_test = test[,1]

#****** define logistic function and custompredict and customclass functions ******#

logistic_regression <- function(X,Y,lr=0.1,threshold=0.0001)
{
  params <- rep(0,ncol(x_train)+1)
  prev_loss <- 0.0
  diff <- Inf
  epochs <- 0
  
  while(diff > threshold)
  {
    epochs <- epochs+1
    diff <- 0.0
    cur_loss <- 0.0
    
    for (row in 1:nrow(X))
    {
      x <- as.numeric(c(1,X[row,]))
      
      pred <- as.double(1/as.double(1+exp(-(x%*%params))))
      loss <- Y[row] - pred
      cur_loss <- cur_loss+(loss^2)
      
      for (p in 1:length(params))
      {
        params[p] <- params[p] + lr*x[p]*loss
      }
    }
    cur_loss <- sqrt(cur_loss)
    diff <- abs(prev_loss - cur_loss)
    prev_loss <- cur_loss
    print(paste0("Epochs : ",epochs," Loss : ",loss))
  }
  return (params)
}

params <- logistic_regression(x_train,y_train)

customgetpred <- function(x)
{
  x <- as.numeric(c(1,x))
  return (as.double(1/as.double(1+exp(-(x%*%params)))))
}

customgetclass <- function(x) {if (x >= 0.5){return (1)} else { return (0)}}

#********* custom Model ********#

logmodel <- glm(default~student+balance+income,data=train,family=binomial)
modelsum <- summary(logmodel)
coefs <- modelsum$coefficients

#******print coeffs********#
print("Params")
print(params)
print("Coeffs")
print(coefs)

#**********prediction on test data*********#

pred <- sapply(apply(x_test,1,customgetpred),customgetclass)
predlibr <- sapply(predict(logmodel,newdata=x_test,type="response"),customgetclass)

print("Confusion matrix for custom function")
print(table(pred,y_test))
print("Confusion matrix for library function")
print(table(predlibr,y_test))