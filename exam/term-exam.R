# part 1

# 1，error

# 2， error
因为这是一个random observation, 不能得出因果关系，所以，不能选3，而应该选1

# 3，eror

# 4，right
size=200
g1<-rnorm(n=size)
g2<-rnorm(n=size, mean=32)

library(data.table)
s<-data.table(idx=c(1:(size*2)), sleep_time=c(g1,g2))

diff<-rep(NA,size)
for (i in 1:size) {
  s_idx<-sample(1:(size*2),size)
  t_g1<-s[idx %in% s_idx,sleep_time]
  t_g2<-s[!(idx %in% s_idx), sleep_time]
  diff[i]=mean(t_g1) - mean(t_g2)
}

hist(diff,breaks=30,col="red")


# 5， error
3. 错误，不应该用表中的t-value作为confidence interval中的t*，而应该用df=157-1 下的t值
4. 0.915 ^ 2 = 0.84

# 6， right
p=0.11
n = 100
miu <- n*p
sigma <- sqrt(n*p*(1-p))

miu + c(-1,1) * 1.96 * sigma

# 7， right
ME <- 1.96 * sigma / sqrt(n)

# 8， right

# 9，right
1-(1-0.12)^4
sum(dbinom(1:4,4,0.11))

# 10， right

# 11， right
i: 
ii: miu<-66; S<- 16/ sqrt(16); S  # S=4 --- C
iii: miu<-66; S<- 16/ sqrt(64); S   # S=2  ---> B

# 12 ， right
n=10
miu_diff <-  248.3 - 244.8  # 3.5


1. (6.13 - 0.87) /2 + 0.87 # 中心在3.5,
2. (6.48 - 2.65) /2 + 2.65  # 中心在4.565，<> 均值3.5，所以不是这个
3. (9.76 - (-2.76)) / 2 + (-2.76) # 中心在3.5；p-value=0.0066, reject H0, 即差额均值不是0，即区间不应跨越0
4. (251 - 243)/2+243   # 中心 247，<> 均值3.5，所以不是这个

# 13， right

# 14， right
happiness_level df=k-1 = 3-1 = 2
Residuals df = n-k = 831 - 3 = 828

Residuals Sum sq = 195528 - 1627 = 193901

happiness_level Mean Sq = Sum sq / df = 1627 / 2 = 813.5
Residuals Mean Sq = Sum sq / df = 193901 / 828 = 234.18

F statistic = happiness_level Mean Sq / Residuals Mean Sq = 813.5 / 234.18 = 3.47


# 15, right
P(>F)=0.03 < 0.05, H0 is rejected

R-squared is the proportion of variability in y explained by the model

---> the variability in number of friends is explained by happiness level. 
R-squared = happiness_level Sum sq / Total Sum sq = 1627 / 195528 = 0.008321059 = 0.83%

1. Approximately 0.83% of the variability in number of friends is explained by happiness level. 
  --- x、y反了。


# part 2

# 1 right

# 2 error

# 3
HT: sqrt( 0.2 * (1-0.2) / 3226)   # 0.007042515
CI: sqrt( 0.24 * (1-0.24) / 3226) # 0.007519349

# 5
46 * 112 / 625  # 8.24

# 10
P(drug) = 5%
p(no drug) = 1- 5%=95%
P(+|nodrug)=3%
P(-|drug)=7%
P(+|drug) = 1- P(-|drug) = 1- 7% = 93%


P(drug|+) = P(drug & + ) / P(+) 
          = P(+|drug) * P(drug) /( P(+|drug)*P(drug) + p(+|no drug)*P(nodrug))
          = (0.93 * 0.05)/(0.93 * 0.05 + 0.03* 0.95)
          = 0.62

# 14
R-squared = 1- var(ei)/var(yi) = 0.7467
 so, var(ei)/var(yi) = 1-0.7467= 0.2533

adjust R-squared = 1-  var(ei)/var(yi) * (n-1)/(n-k-1)
                 = 1 - 0.2533 * 251 / (251 - 8)
                 = 0.7383609

