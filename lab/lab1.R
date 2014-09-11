source("http://www.openintro.org/stat/data/cdc.R")
names(cdc)

#1
dim(cdc)

#5
table(cdc$gender)

# 6
table(cdc$genhlth)
4657/20000

# 7
mosaicplot(table(cdc$gender, cdc$smoke100))

# 8
table(cdc$age<23 & cdc$smoke100==1)

# 9
cdc$BMI<-cdc$weight/(cdc$height^2) * 703
boxplot(BMI~genhlth,data=cdc)

# 10
plot(cdc$weight, cdc$wtdesire)
