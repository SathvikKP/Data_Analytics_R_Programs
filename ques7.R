setwd("/home/user/DA_LAB/final_exam_practice")

dataset <- read.csv("ques7.csv",header=TRUE,stringsAsFactors = FALSE)

print(dataset)

multt <- function(t1,t2,t1avg,t2avg)
{
  return ((t1-t1avg)*(t2-t2avg))
}

n <- nrow(dataset)
xavg <- sum(dataset$exp)/n
yavg <- sum(dataset$pub)/n

xy <- sum(mapply(multt,dataset$exp,dataset$pub,xavg,yavg))
xx <- sum(mapply(multt,dataset$exp,dataset$exp,xavg,xavg))

beta1 <- xy/xx
beta0 <- yavg - beta1*xavg

print("Enter exp : ")
x <- readline()
x <- as.numeric(x)
p <- as.data.frame(list(x))
colnames(p) <- c("exp")

linearmod <- lm(pub~exp,data=dataset)
modelsummary <- summary(linearmod)
modelcoef <- modelsummary$coefficients


print(paste0("beta 0 : ",beta0," beta 1 : ",beta1))
print(modelcoef)

y <- beta0 + beta1*x
pred <- predict(linearmod,newdata=p)

print(paste0("Prediction without predefined : ",y))
print(paste0("Prediction with predefined : ",round(pred)))