(9.51-10)/(4.65/sqrt(40))
pnorm(-0.6665)

# Q10 用抽样法解题
x<-c(-5,-4,-3,-2,1,7,10,11,17,18)
y<-c(-11,-5,-3,-3,-1,-1,-1,2,3,5,12)
mean(x)-mean(y)
median(x)-median(y)

library(data.table)
s<-data.table(idx=c(1:21), diff=c(x,y))
k1<-rep(NA,100)
k2<-rep(NA,100)

for (i in 1:100) {
  # 一般性抽样，感觉有问题，不知是否对题目说明理解错误
  s_idx<-sample(1:21,10)
  t<-s[idx %in% s_idx,diff]
  p<-s[!(idx %in% s_idx),diff]
  k1[i]=median(t) -median(p)
  
  # bootrap方式抽样，感觉这才是正确的
  t2<-sample(x,10, replace=TRUE)
  p2<-sample(y,11, replace=TRUE)
  k2[i]=median(t2) -median(p2)
}
par(mfrow=c(1,2))
hist(k1,breaks=20,col="red")
hist(k2,breaks=20,col="blue")

# 从图2观察，以5为轴对称

# Q11，题目大概是求x=5的p-value?，一个点算1%

