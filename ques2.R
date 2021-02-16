setwd("/home/user/DA_LAB/final_exam_practice")

fac <- read.csv("ques2.csv",header =TRUE,stringsAsFactors = FALSE)
origfac <- data.frame(fac)
print(origfac)

fac$name <- gsub("[^a-zA-Z ]+","",fac$name)
fac$name <- gsub("\\s+"," ",trimws(fac$name))
library(stringr)
fac$name <- str_to_title(fac$name)

fac$salary <- gsub("[^0-9]+","",fac$salary)
fac$salary <- replace(fac$salary,is.na(fac$salary),0)
fac$salary <- as.numeric(fac$salary)

Q1 <- quantile(fac$salary,0.25)
Q3 <- quantile(fac$salary,0.75)
IQR <- IQR(fac$salary)

lowerbound <- Q1-1.5*IQR
upperbound <- Q3+1.5*IQR

print(paste0("lowerbound = ",lowerbound," upperbound = ",upperbound))
condition <- fac$salary < lowerbound | fac$salary <= 0 | fac$salary > upperbound

fac$salary <- replace(fac$salary,condition,NA)
fac$salary[is.na(fac$salary)] <- mean(fac$salary,na.rm=TRUE)

print(fac)