
Grading Policy:
Quizzes: 20% (average of 6 highest quiz grades)
Lab: 15% (average of 6 highest lab grades)
Projects: 15% (peer assessed)
Midterm: 20%
Final: 30%


my score:
  Quizzes 1: 12.00 / 12.00
  Quizzes 2: 11.00 / 12.00
  Quizzes 3: 13.00 / 15.00
  Quizzes 4: 14.00 / 14.00
  Quizzes 5: 12.00 / 12.00
  Quizzes 6: 12.00 / 13.00
  Quizzes 7: 10.00 / 10.00
Lab:
  Lab0: 8.00 / 8.00
  Lab1: 10.00 / 10.00
  Lab2: 9.00 / 9.00
  Lab3A:6.00 / 6.00
  Lab3B:6.00 / 6.00
  Lab4: 11.00 / 11.00
  Lab5: 13.00 / 13.00
  Lab6: 10.00 / 10.00
  Lab7: 10.00 / 10.00
Projects
  Data Analysis Project: 64.5

Midterm
  Part1: 11.00 / 15.00
  part2: 15.00 / 15.00
finalTerm:
  part1: 9.00 / 15.00
  part2: 13.00 / 15.00

# 计算分数 With Distinction (with labs and data analysis project)
Lab<-.15
Project<-.15
Midterm <-.2
Final<-.3
Quiz<-.2 

## average of 6 highest quiz grades
Q1<-12/12
Q2<-11/12
Q3<-13/15
Q4<-14/14
Q5<-12/12
Q6<-12/13
Q7<-10/10
quizzes <- c(Q1,Q2,Q3,Q4,Q5,Q6,Q7)
MQ <- mean(quizzes[order(-quizzes)][1:6])
MQ

# (average of 6 highest lab grades), lab0 not graded
L1<- 10.00 / 10.00
L2<- 9.00 / 9.00
L3A<- 6.00 / 6.00
L3B<- 6.00 / 6.00
L4<- 11.00 / 11.00
L5<- 13.00 / 13.00
L6<- 10.00 / 10.00
L7<- 10.00 / 10.00

labs<-c(L1,L2,L3A,L3B,L4,L5,L6,L7)
ML<-mean(labs[order(-labs)][1:6])

# mid term
My_Mid<-(11+15)/30
My_Mid

# project
My_project<-64.5/65
My_project

# final trem 
My_Final<-(9+13)/30
My_Final

Final<-(Quiz*MQ) + (Lab*ML) + (Project*My_project) + (Midterm*My_Mid) + (My_Final*Final)
round(Final,4)  ## Needs to by 80%

# Without Distinction (no labs and no data analysis project)

Quizzes: 30% (average of 6 highest quiz grades)
Midterm: 30%
Final: 40%


QND<-.3
MND<-.3
FND<-.4
NO_D<-(MND*My_Mid)+(QND*MQ)+(FND*Final)
NO_D

