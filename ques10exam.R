setwd("/home/user/DA_LAB/final_exam_practice")

marks <- read.csv("marks.csv",header = TRUE)
print(marks)

par(mfrow=c(1,5))
pie(marks$phys,labels=marks$name,main="Physics Marks")
pie(marks$chem,labels=marks$name,main="Chem Marks")
pie(marks$maths,labels=marks$name,main="Math Marks")
pie(marks$foc,labels=marks$name,main="FOC Marks")
pie(marks$cad,labels=marks$name,main="CAD Marks")
help(pie)

par(mfrow=c(1,1))
sub <- c("phys","chem","maths","foc","cad")
alice <- as.numeric(marks[1,2:6])
print(alice)
pie(alice,sub,main="ALice Scores")

votes <- read.csv("party.csv",header=TRUE,stringsAsFactors = FALSE)
print(votes)
sum1 = sum(votes$Votes)
percvotes <- data.frame(votes)
percvotes$Votes <- percvotes$Votes/sum1*100
print(percvotes)
pie(percvotes$Votes,labels=percvotes$Votes,main="PARTY VOTES IN PERC",col=rainbow(5))
legend("topleft",percvotes$Party,fill=rainbow(5))
help(legend)

stud<-read.csv("studentperform.csv",header=TRUE,stringsAsFactors = FALSE)
print(stud)

label1 <- c("Male","Female")

m <- 0
f <- 0
sm1 <- 0
sm2 <- 0
sf1 <- 0
sf2 <- 0
sm1a <- 0
sm2a <- 0
sf1a <- 0
sf2a <- 0

for (i in 1:nrow(stud))
{
  if (stud[i,4]=="M")
  {
    m <- m+1
    sm1 <- sm1+stud[i,2]
    sm2 <- sm2+stud[i,3]
  }
  else
  {
    f <- f+1
    sf1 <- sf1+stud[i,2]
    sf2 <- sf2+stud[i,3]
  }
}
sm1a <- sm1/m
sm2a <- sm2/m
sf1a <- sf1/f
sf2a <- sf2/f

par(mfrow=c(1,2))
pie(x=c(sm1a,sf1a),labels=label1,main="Sub 1 comparision")
pie(x=c(sm2a,sf2a),labels=label1,main="Sub 2 comparision")