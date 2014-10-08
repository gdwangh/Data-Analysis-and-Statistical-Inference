load(url("http://d396qusza40orc.cloudfront.net/statistics/lab_resources/nc.RData"))

# Q1

# Q2
summary(nc)

gained_clean = na.omit(nc$gained)
n = length(gained_clean)

# Q3
boot_means = rep(NA, 100)
for(i in 1:100) {
  boot_sample = sample(gained_clean, n, replace = TRUE)
  boot_means[i] = mean(boot_sample)
}

# Q4
quantile(boot_means,probs=c(0.05,0.95))

# 5%       95% 
# 29.60601 31.08952 

m<-mean(boot_means)
s<-sd(boot_means)
m+c(-1,1)*pnorm(0.95)*s
# [1] 29.96511 30.70328

source("http://d396qusza40orc.cloudfront.net/statistics/lab_resources/inference.R")

inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.9, est = "mean", boot_method = "perc")
# 90 % Bootstrap interval = ( 29.5498 , 31.0637 )

inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.95, est = "mean",boot_method = "perc")
# 95 % Bootstrap interval = ( 29.444 , 31.2323 )

inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.95, est = "mean",boot_method = "se")
# 95 % Bootstrap interval = ( 29.4199 , 31.2201 )

inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.95, est = "median",boot_method = "se")
# 95 % Bootstrap interval = ( 29.4696 , 30.4168 )

inference(nc$fage, type = "ci", method = "simulation", conflevel = 0.95, est = "mean",boot_method = "se")
# 95 % Bootstrap interval = ( 29.7855 , 30.7115 )


# Q6
plot(nc$habit, nc$weight)

by(nc$weight, nc$habit, mean)
by(nc$weight, nc$habit, sd)
by(nc$weight, nc$habit, length)

# Q7
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ht", null = 0,alternative = "twosided", method = "theoretical")
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ht", null = 0,alternative = "twosided", method = "theoretical", order = c("smoker","nonsmoker"))
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ci", null = 0,alternative = "twosided", method = "theoretical", order = c("smoker","nonsmoker"))


# Q8
by(nc$mage,nc$mature, max)
by(nc$mage,nc$mature, min)

# Q9
load(url("http://d396qusza40orc.cloudfront.net/statistics/lab_resources/gss.RData"))
plot(gss$class,gss$wordsum)

inference(y = gss$wordsum, x = gss$class, est = "mean", type = "ht",alternative = "greater", method = "theoretical")
