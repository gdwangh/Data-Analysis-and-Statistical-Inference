setwd("D:/doc/study/Data Analysis and Statistical Inference/Data-Analysis-and-Statistical-Inference/courseproject")

load("gss.Rdata")

# student 1:  the sex education in school (sexeduc - categorical) and opinion about the sexsual relationship between two adult people (homosex - categorical)
# student 2: income06 and abpoor 
# student 3: - WRKSLF: R SELF-EMP OR WORKS FOR SOMEBODY - categorical variable
#            - SATFIN: SATISFACTION WITH FINANCIAL SITUATION - categorical variable

# race and satjob
summary(data.frame(gss$race, gss$satjob))
gss_clean<-na.omit(data.frame(year=gss$year, race=gss$race, satjob=gss$satjob))
table(gss_clean$year)
m<-table(gss_clean$race, gss_clean$satjob) 
plot(gss_clean$race, gss_clean$satjob)
calchi_pvalue(m)

for (y in unique(gss_clean$year)) {
  DF<-subset(gss_clean, year==y, select=c(race, satjob))
  m<-table(DF$race, DF$satjob)
  print(paste("year",toString(y)))
  calchi_pvalue(m)
}


# sexeduc and homosex
summary(data.frame(gss$sexeduc, gss$homosex))
gss_clean<-na.omit(data.frame(year=gss$year, sexeduc=gss$sexeduc, homosex=gss$homosex))
table(gss_clean$year)
m<-table(gss_clean$sexeduc, gss_clean$homosex) # 不满足每个cell的expected count>5 的条件
plot(gss_clean$sexeduc, gss_clean$homosex)
calchi_pvalue(m)


# WRKSLF and SATFIN 
summary(data.frame(gss$wrkslf, gss$satfin))
gss_clean<-na.omit(data.frame(year=gss$year, wrkslf=gss$wrkslf, satfin=gss$satfin))
m<-table(gss_clean$wrkslf, gss_clean$satfin)
plot(gss_clean$wrkslf, gss_clean$satfin)
calchi_pvalue(m)

for (y in unique(gss_clean$year)) {
  DF<-subset(gss_clean, year==y, select=c(wrkslf, satfin))
  m<-table(DF$wrkslf, DF$satfin)
  calchi_pvalue(m)
}

# coninc and abpoor 
summary(data.frame(gss$income06, gss$abpoor))
summary(data.frame(gss$coninc, gss$abpoor))
plot(gss$abpoor,gss$coninc)
table(gss$coninc,gss$abpoor)

DF_yes<-subset(gss, abpoor=="Yes" & !is.na(coninc), select=c(year,coninc,abpoor))
DF_No<-subset(gss, abpoor=="No" & !is.na(coninc), select=c(year,coninc,abpoor))



check condition:
  1. independence
  2. nearly normal
hist(DF_yes$coninc)
hist(DF_No$coninc)

size=nrow(DF_yes)
k<-rep(NA,size)
for (i in 1:10000) {
   sample_yes<-sample(DF_yes$coninc, size, replace=TRUE)
   k[i]<-mean(sample_yes)
}
hist(k)
qqnorm(k)
qqline(k,col="red")

k<-rep(NA,size)
for (i in 1:10000) {
  sample_No<-sample(DF_No$coninc, size, replace=TRUE)
  k[i]<-mean(sample_No)
}
hist(k)
qqnorm(k)
qqline(k,col="red")

H0: miu_yes - miu_no =0
Ha: miu_yes - miu_no <> 0

x_yes<-mean(DF_yes$coninc)
s_yes<-sd(DF_yes$coninc)
n_yes<-nrow(DF_yes)

x_no<-mean(DF_No$coninc)
s_no<-sd(DF_No$coninc)
n_no<-nrow(DF_No)
se<-sqrt(s_yes^2/n_yes + s_no^2/n_no)

confidence interval:
  (x_yes - x_no) + c(-1,1) * 1.96 * se

p-value
   Z = (x_yes-x_no-0)/se
   pnorm(Z, lower.tail=FALSE) * 2
