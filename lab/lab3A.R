load(url("http://www.openintro.org/stat/data/ames.RData"))
area <- ames$Gr.Liv.Area
price <- ames$SalePrice

# Q1
summary(area)
par(mfrow = c(1, 1))
hist(area,breaks=20)

# Q2
sample_means50 <- rep(NA, 5000)
for (i in 1:5000) {
samp <- sample(area, 50)
sample_means50[i] <- mean(samp)
}

sample_means100 <- rep(NA, 5000)
for (i in 1:5000) {
  samp <- sample(area, 100)
  sample_means100[i] <- mean(samp)
}

sample_means1000 <- rep(NA, 5000)
for (i in 1:5000) {
  samp <- sample(area, 1000)
  sample_means1000[i] <- mean(samp)
}

par(mfrow = c(2, 2))
hist(sample_means50)
hist(sample_means100)
hist(sample_means1000)

# Q3
sample_means_small<-rep(NA, 100)
for (i in 1:100) {
  samp <- sample(area, 50)
  sample_means_small[i] <- mean(samp) 
}


# Q5
par(mfrow = c(2, 2))
xlimits = range(sample_means50)

hist(sample_means50, breaks = 20,xlim = xlimits)
hist(sample_means100,breaks = 20,xlim = xlimits)
hist(sample_means1000,breaks = 20,xlim = xlimits)


