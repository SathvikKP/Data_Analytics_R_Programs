library(ISLR)
data <- Default
#print(head(data))

min_balance <- min(data$balance)
max_balance <- max(data$balance)
min_income <- min(data$income)
max_income <- max(data$income)

balanceprocess <- function(bal)
{
  return ((bal-min_balance)/(max_balance-min_balance))
}
incomeprocess <- function(inc)
{
  return ((inc-min_income)/(max_income-min_income))
}
yesnoprocess <- function(x)
{
  if (x=="Yes")
  {
    return (1)
  }
  else
  {
    return (0)
  }
}

data$default <- sapply(data$default,yesnoprocess)
data$student <- sapply(data$student,yesnoprocess)
data$balance <- sapply(data$balance,balanceprocess)
data$income <- sapply(data$income,incomeprocess)

print(head(data))

train_ind = sample.int(nrow(data),size=0.75*nrow(data))
set.seed(123)
train <- data[train_ind,]
test <- data[-train_ind,]
#print(str(train))
#print(str(test))

x_train <- train[,-1]
y_train <- train[,1]
x_test <- test[,-1]
y_test <- test[,1]

##WRONG
logisticRegression <- function(X,Y,lr=1,threshold=0.01)
{
  params <- rep(0,ncol(x_train)+1)
  prev_loss <- 0.0
  diff <- Inf
  
  while(diff > threshold)
  {
    cur_loss <- 0.0
    diff <- 0
    
    for (row in 1:nrow(X))
    {
      x <- as.numeric(c(1,X[row,]))
      pred <- as.double(1/as.double(1+exp(-(x%*%params))))
      loss <- Y[row] - pred
      
      curr_loss <- cur_loss+(loss^2)
      for (p in 1:length(params))
      {
        params[p] <- params[p] + lr*x[p]*loss
      }
      
      cur_loss <- sqrt(cur_loss)
      diff <- abs(cur_loss - prev_loss)
      prev_loss <- curr_loss
      print(paste0("Loss : ",curr_loss))
    }
    return (params)
  }
}

params <- logisticRegression(x_train,y_train)

getPred <- function(x)
{
  x <- as.numeric(c(1,x))
  return (1/(1+exp(-(x%*%params))))
}

getclass <- function(x)
{
  if (x>=0.5)
  {
    return (1)
  }
  else
  {
    return (0)
  }
}

model <- glm(default~student+balance+income,data=train,family = binomial)
modelsummary <- summary(model)
modelcoef <- modelsummary$coefficients

print(params)
print(modelcoef)

rsemodel <- sqrt(sum((y_train-predict(model,x_train,type="response"))^2))
rse <- sqrt(sum((y_train-apply(x_train,1,getPred))^2))

print(paste0("RSE of model and custom ",rsemodel," ",rse))

pred <- sapply(apply(x_test,1,getPred),getclass)
predlib <- sapply(predict(model,x_test,type="response"),getclass)

print(table(pred,y_test))
print(table(predlib,y_test))

#print(summary(model))
