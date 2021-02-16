setwd("/home/user/DA_LAB/final_exam_practice")

dataset <- read.csv("ques6.csv",header=TRUE,stringsAsFactors = FALSE)
dataset$age <- as.numeric(dataset$age)
dataset$loan  <- as.numeric(dataset$loan)
print(dataset)

knnfromscratch <- function(x,k)
{
  distance = vector()
  age <- x[1]
  loan <- x[2]
  print(paste0("Age : ",age,"  Loan ",loan))
  length(distance) = nrow(dataset)
  
  for (i in 1:nrow(dataset))
  {
    distance[i] = sqrt((age-dataset[i,1])^2 + (loan-dataset[i,2])^2)
  }
  temp_table <- dataset
  temp_table$distance <- distance
  #print(temp_table)
  temp_table <- temp_table[order(temp_table$distance),]
  #print(temp_table)
  nclasses = temp_table[1:k,3]
  print(nclasses)
  pos<-0
  neg<-0
  for (i in 1:k)
  {
    if (nclasses[i]=="Y")
    {
      pos <- pos+1
    }
    else
    {
      neg <- neg+1
    }
  }
  if (pos>neg)
  {
    return ("Y")
  }
  else
  {
    return ("N")
  }
  
}

print("Enter age and loan and k")
x1 <- as.numeric(readline())
x2 <- as.numeric(readline())
karg <- as.numeric(readline())
x <- c(x1,x2)

ans <- knnfromscratch(x,karg)
print("")
print("")
print(paste0("Prediction = ",ans))


p <- as.data.frame(list(x1,x2))
colnames(p) <- c("age","loan")
x <- dataset[,1:2]
y <- dataset[,3]

library(class)
pred <- knn(train=x,test=p,cl=y,k=karg)
#help(knn)
print(paste0("Prediction using predefined = ",pred))