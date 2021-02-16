setwd("/home/user/DA_LAB/final_exam_practice")

dataset <- read.csv("ques5.csv",header=TRUE,stringsAsFactors = FALSE)
print(dataset)

preprocess <- function(x)
{
  if (x=="newspaper")
  {
    return (1)
  }
  else if (x=="radio")
  {
    return (2)
  }
  else if (x=="tv")
  {
    return (3)
  }
  else
  {
    return (0)
  }
}

dataset$media <- sapply(dataset$media,preprocess)
print(dataset)

n <- nrow(dataset)
x2avg <- sum(dataset$budget)/n
x1avg <- sum(dataset$media)/n
yavg <- sum(dataset$sales)/n

mult <- function(t1,t2,t1avg,t2avg)
{
  return ((t1-t1avg)*(t2-t2avg))
}

#calculate terms
#x2^2
x2x2 <- sum(mapply(mult,dataset$budget,dataset$budget,x2avg,x2avg))
#x1y
x1y <- sum(mapply(mult,dataset$media,dataset$sales,x1avg,yavg))
#x1x2 
x1x2 <- sum(mapply(mult,dataset$media,dataset$budget,x1avg,x2avg))
#x2y
x2y <- sum(mapply(mult,dataset$budget,dataset$sales,x2avg,yavg))

#x1^2
x1x1 <- sum(mapply(mult,dataset$media,dataset$media,x1avg,x1avg))

#beta 1 and beta 2

numerator1 <- x2x2*x1y - x1x2*x2y
denominator <- x1x1*x2x2 - x1x2*x1x2
numerator2 <- x1x1*x2y - x1x2*x1y

beta1 <- numerator1/denominator
beta2 <- numerator2/denominator
beta0 <- yavg - beta1*x1avg - beta2*x2avg

print(paste0("beta 0 = ",beta0," beta 1 = ",beta1," beta2 = ",beta2))

print("Enter budget and media type : (newspaper-1/radio-2/tv-3)")
x2 <- readline()
x1 <- readline()
x1 <- as.numeric(x1)
x2 <- as.numeric(x2)
y <- beta0 + beta1*x1 + beta2*x2


linearmod <- lm(sales~media+budget,data=dataset)
modelsummary <- summary(linearmod)
modelcoeffs <- modelsummary$coefficients
print(modelcoeffs)
p <- data.frame(list(x1,x2))
colnames(p) <- c("media","budget")
pred <- predict(linearmod,newdata=p)
print(paste0("Prediction without predefined = ",y))
print(paste0("prediction with predefined = ",pred))