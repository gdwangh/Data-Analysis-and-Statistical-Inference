# student 1:  the sex education in school (sexeduc - categorical) and opinion about the sexsual relationship between two adult people (homosex - categorical)
# student 2: income06 and abpoor 
# student 3: - WRKSLF: R SELF-EMP OR WORKS FOR SOMEBODY - categorical variable
#            - SATFIN: SATISFACTION WITH FINANCIAL SITUATION - categorical variable

setwd("D:/doc/study/Data Analysis and Statistical Inference/Data-Analysis-and-Statistical-Inference/courseproject")

load("gss.Rdata")

summary(data.frame(gss$race, gss$jobfind))
table(gss$race, gss$jobfind)

plot(table(gss$year, gss$jobfind),col=c(2,3,4))

good_gss<-gss[!is.na(gss$jobfind)]

calchi_pvalue<-function(m) {
  rn<-nrow(m)
  cn<-ncol(m)
  df<-(rn-1)*(cn-1)
  
  sm<-addmargins(m)
  p_rtotal<-prop.table(sm[1:rn, cn+1])
  ctotal<-sm[rn+1,1:cn]
  Em<-sapply(ctotal, FUN=function(x){ x*p_rtotal } )
  if (min(Em)<5) {
    print(paste("NOt all cell is at lease 5: min(m)=",toString(min(m))))
  } else {
    addmargins(prop.table(Em,2))
    X_squr<-sum((m-Em)^2/Em)
    pvalue<-pchisq(X_squr, df,lower.tail=FALSE)
    print(paste("p-value=",toString(pvalue)))
  }
}

m<-table(good_gss$race, good_gss$jobfind)
calchi_pvalue(m)


for (y in unique(good_gss$year)) {
  print(y)
  DF<-subset(good_gss, year==y, select=c(race, jobfind))
  m<-table(DF$race, DF$jobfind)  
  calchi_pvalue(m)
}
