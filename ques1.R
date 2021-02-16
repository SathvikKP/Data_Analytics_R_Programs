dir <- getwd()
print(getwd())
setwd("/home/user/DA_LAB/final_exam_practice")

stud <- read.csv("ques1.csv",stringsAsFactors = FALSE,header=TRUE)
#print(stud)
print(origstud)
origstud <- data.frame(stud)

stud$name <- gsub("[^a-zA-Z ]+","",stud$name)
stud$name <- gsub("\\s+"," ",trimws(stud$name))
library(stringr)
stud$name <- str_to_title(stud$name)

#stud$cgpa[is.na(stud$cgpa)] <- 11.0
stud$cgpa <- replace(stud$cgpa,is.na(stud$cgpa),0)
stud$cgpa <- gsub("[^0-9]+","",stud$cgpa)
stud$cgpa <- as.numeric(stud$cgpa)

Q1 <- quantile(stud$cgpa,0.25)
Q3 <- quantile(stud$cgpa,0.75)
IQR <- IQR(stud$cgpa)

lowerbound <- Q1 - 1.5*IQR
upperbound <- Q3 + 1.5*IQR

print(paste0("lowerbound = ",lowerbound," upperbound = ",upperbound))
condition <- (stud$cgpa < lowerbound | stud$cgpa > upperbound | stud$cgpa < 0)
stud$cgpa <- replace(stud$cgpa,condition,NA)
stud$cgpa[is.na(stud$cgpa)] <- mean(stud$cgpa,na.rm=TRUE)

#data$cgpa[is.na()]

print(stud)