setwd("/home/user/DA_LAB/final_exam_practice")

dept <- read.csv("ques3.csv",header = TRUE,stringsAsFactors = FALSE)
origdata <- data.frame(dept)
print(origdata)

dept$dept.name <- gsub("[^a-zA-Z ]+","",dept$dept.name)
dept$dept.name <- gsub("\\s+"," ",trimws(dept$dept.name))
library(stringr)
dept$dept.name <- str_to_title(dept$dept.name)

dept$staff.count <- replace(dept$staff.count,is.na(dept$staff.count),0)
dept$staff.count <- gsub("[^0-9]+","",dept$staff.count)
dept$staff.count <- as.numeric(dept$staff.count)
condition1 <- dept$staff.count <= 0 | dept$staff.count > 100
dept$staff.count <- replace(dept$staff.count,condition1,NA)
dept$staff.count[is.na(dept$staff.count)] <- ceiling(mean(dept$staff.count,na.rm=TRUE))

Q1 <- quantile(dept$pub.count,0.25)
Q3 <- quantile(dept$pub.count,0.75)
IQR <- IQR(dept$pub.count)

lowerbound <- Q1-1.5*IQR
upperbound <- Q3+1.5*IQR

print(paste0("lowerbound = ",lowerbound," upperbound = ",upperbound))
condition2 <- dept$pub.count < lowerbound | dept$pub.count > upperbound

dept$pub.count <- replace(dept$pub.count,condition2,NA)
dept$pub.count[is.na(dept$pub.count)] <- ceiling(mean(dept$pub.count,na.rm=T))

print(dept)