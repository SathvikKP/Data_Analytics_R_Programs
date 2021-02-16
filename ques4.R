setwd("/home/user/DA_LAB/final_exam_practice")

adv <- read.csv("ques4.csv",header=TRUE,stringsAsFactors = FALSE)
print(adv)

beta0 <- 0
beta1 <- 0

xavg <- (sum(adv$budget))/nrow(adv)
yavg <- (sum(adv$sales))/nrow(adv)

func1 <- function(x)
{
  return (x-xavg)
}
func2 <- function(y)
{
  return (y-yavg)
}

func3 <- function(x)
{
  return((x-xavg)*(x-xavg))
}

col1 <- sapply(adv$budget,func1)
col2 <- sapply(adv$sales,func2)
col3 <- sapply(adv$budget,func3)

numtemp <- col1*col2
num <- sum(numtemp)
den <- sum(col3)

beta1 <- num/den
beta0 <- yavg-(beta1*xavg)

print(paste0("Beta 0 = ",beta0," Beta 1 = ",beta1," x avg = ",xavg," y avg = ",yavg))
print("Enter budget")
x1 = readline()
#x1 = 5000
x1 = as.numeric(x1)
y1 = beta0 + beta1*x1
print(paste0("prediction for budget = ",x1," is y = ",y1 ))


linearmod <- lm(sales~budget,data=adv)
#print(linearmod)
modelsummary <- summary(linearmod)
#print(modelsummary)
modelcoeffs <- modelsummary$coefficients
print(modelcoeffs)

print("Enter budget")
#x2 = readline()
#x2 <- as.numeric(x2)
p1 <- data.frame(x1)
colnames(p1) <- "budget"
y2 <- predict(linearmod,p1)
print(paste0("prediction for budget using predefined = ",x1," is y = ",y2 ))
#help(predict)