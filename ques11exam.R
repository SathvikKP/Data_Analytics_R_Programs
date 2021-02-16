setwd("/home/user/DA_LAB/final_exam_practice")

marks <- read.csv("marks.csv",header=TRUE,stringsAsFactors = FALSE)
print(marks)

par(mfrow=c(1,2))
barplot(marks$phys,names.arg = marks$name,main="physics marks",xlab="Name",ylab="Marks",col=rainbow(5))
barplot(marks$chem,names.arg = marks$name,main="chemistry marks",xlab = "Name",ylab="Marks",col=rainbow(5))

sub <- c("phys","chem","maths","foc","cad")
alice < marks[1,2:6]
barplot(alice,names.arg=sub,main="Alice Scores",xlab="Subject",ylab="Marks",col=rainbow(5))

votes <- read.csv("party.csv",header=TRUE,stringsAsFactors = FALSE)
sum1 <- sum(votes$Votes)
votes$Votes <- replace(votes$Votes,TRUE,votes$Votes/sum1*100)
print(votes)

barplot(votes$Votes,names.arg = votes$Party,main="Party votes in perc",xlab="Party",ylab="Perc of votes")
help(barplot)

stud <- read.csv("studentperform.csv",header=TRUE,stringsAsFactors = FALSE)
print(stud)

m <- 0
f <- 0
ms1 <- 0
ms2 <- 0
fs1 <- 0
fs2 <- 0
ms1a <- 0
ms2a <- 0
fs1a <- 0
fs2a <- 0

for (i in 1:nrow(stud))
{
  if (stud[i,4]=="M")
  {
    m <- m+1
    ms1 <- ms1+stud[i,2]
    ms2 <- ms2+stud[i,3]
  }
  else
  {
    f <- f+1
    fs1 <- fs1+stud[i,2]
    fs2 <- fs2+stud[i,3]
  }
}

fs1a <- fs1/f
fs2a <- fs2/f
ms1a <- ms1/m
ms2a <- ms2/m

label1 = c("Male","Female")
barplot(c(ms1a,fs1a),names.arg=label1,main="Sub1 comparision",xlab="Gender",ylab="Avg Marks")
barplot(c(ms2a,fs2a),names.arg=label1,main="Sub2 comparision",xlab="Gender",ylab="Avg Marks")
