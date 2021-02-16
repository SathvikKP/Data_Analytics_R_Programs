setwd("/home/user/DA_LAB/final_exam_practice")
marks <- read.csv("marks.csv",header=TRUE,stringsAsFactors = FALSE)
print(marks)
par(mfrow=c(1,1))
help(boxplot)
print(marks[2:6])
boxplot(marks[2:6],xlab="Sub",ylab="Marks",main="Subject marks")

votes <- read.csv("party.csv",header=TRUE)
print(votes)
boxplot(votes$Votes,xlab="election",ylab="votes",main="Election votes")


stud <- read.csv("studentperform.csv",header=TRUE,stringsAsFactors = FALSE)
print(stud)
par(mfrow=c(1,2))
boxplot(stud$sub1~stud$gender,xlab="Gender",ylab="Marks",main="Sub1 marks")
boxplot(stud$sub2~stud$gender,xlab="Gender",ylab="Marks",main="Sub2 marks")
