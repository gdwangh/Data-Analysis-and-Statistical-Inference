source("http://bit.ly/dasi_inference")
load(url("http://www.openintro.org/stat/data/atheism.RData"))

# Q7
us12 = subset(atheism, atheism$nationality == "United States" & atheism$year == "2012")
prop.table(table(us12$response))

inference(us12$response, est = "proportion", type = "ci", method = "theoretical",success = "atheist")

# Q8
table(atheism$nationality)
s112 = subset(atheism, atheism$nationality == "Switzerland" & atheism$year == "2012")
table(s112$response)
prop.table(table(s112$response))

inference(s112$response, est = "proportion", type = "ci", method = "theoretical",success = "atheist")

# Q9
n <- 1000
p <- seq(0, 1, 0.01)
me <- 2 * sqrt(p * (1 - p)/n)
plot(me ~ p)

# Q10
sp = subset(atheism, atheism$nationality == "Spain")
table(sp$year)
table(sp$response,sp$year)

H0: p_sp05 == p_sp12
Ha: p_sp05 <> p_sp12

inference(sp$response, as.factor(sp$year), null=0,est = "proportion", type = "ht", method = "theoretical",success = "atheist", alternative="twosided")

# Q11
us= subset(atheism, atheism$nationality == "United States")
table(us$year)
table(us$response,us$year)

H0: p_us05 == p_us12
Ha: p_us05 <> p_us12

inference(us$response, as.factor(us$year), null=0,est = "proportion", type = "ht", method = "theoretical",success = "atheist", alternative="twosided")

# Q12
p<-0.5
0.01 = 1.96 * sqrt(p*(1-p)/n)
n <- (1.96*sqrt(p*(1-p))/0.01)^2

n
