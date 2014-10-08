(9.51-10)/(4.65/sqrt(40))
pnorm(-0.6665)

# Q10 用抽样法解题
x<-c(-5,-4,-3,-2,1,7,10,11,17,18)
y<-c(-11,-5,-3,-3,-1,-1,-1,2,3,5,12)
mean(x)-mean(y)
median(x)-median(y)

library(data.table)
s<-data.table(idx=c(1:21), diff=c(x,y))
n=100
k1<-rep(NA,n)
k2<-rep(NA,n)
z1<-0

for (i in 1:n) {
  # 一般性抽样，感觉有问题，不知是否对题目说明理解错误。 从图形上看，以0为对称
  # 从答案上看，就是采用一般混合抽样，解释说因为 H0：=0，所以抽样是以0为对称
  s_idx<-sample(1:21,10)
  t<-s[idx %in% s_idx,diff]
  p<-s[!(idx %in% s_idx),diff]
  k1[i]=median(t) -median(p)
  if (k1[i]<=-5 | k1[i]>=5) {
    z1<-z1+1
  }
  
  # bootrap方式抽样，感觉这才是正确的。-- 答案不是这个
  t2<-sample(x,10, replace=TRUE)
  p2<-sample(y,11, replace=TRUE)
  k2[i]=median(t2) -median(p2)
}
par(mfrow=c(1,2))
hist(k1,breaks=20,col="red")
hist(k2,breaks=20,col="blue")

z1/n  # fail to reject H0, 所以两组的差异是偶然的
sort(k2) # 95% ci 跨越0，即ci包含0，说明median的差有很大可能是0，两组的差异是偶然的


# Q11，一个点算1%，p-value是计算<=-5 和 >=5的点的个数。


