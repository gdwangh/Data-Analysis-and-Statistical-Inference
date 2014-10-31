load(url("http://www.openintro.org/stat/data/mlb11.RData"))

# 2
plot(mlb11$at_bats, mlb11$run)

cor(mlb11$runs, mlb11$at_bats)

# 4
plot_ss(x = mlb11$at_bats, y = mlb11$runs)
plot_ss(x = mlb11$at_bats, y = mlb11$runs, showSquares = TRUE)

m1 <- lm(runs ~ at_bats, data = mlb11)
summary(m1)

m2 <- lm(runs ~ homeruns , data = mlb11)
summary(m2)

# 5
plot(mlb11$runs ~ mlb11$at_bats)
abline(m1)

summary(m1)
y_obs<-mlb11$runs[which(mlb11$at_bats==5579)]
y_hat<--2789.2429+0.6305*5579
resid<-y_obs - y_hat

# 6
plot(m1$residuals ~ mlb11$at_bats)
abline(h = 0, lty = 3) # adds a horizontal dashed line at y = 0

hist(m1$residuals)
qqnorm(m1$residuals)
qqline(m1$residuals) # adds diagonal line to the normal prob plot

# 9
m1 <- lm(runs ~ at_bats , data = mlb11)
summary(m1) # R: 0.3729

m2 <- lm(runs ~ hits , data = mlb11)
summary(m2) # R: 0.6419

m3 <- lm(runs ~ wins , data = mlb11)
summary(m3) # R: 0.361

m4 <- lm(runs ~ bat_avg , data = mlb11)
summary(m4) # R: 0.6561

# 10
m5 <- lm(runs ~ new_obs , data = mlb11)
summary(m5) # R:  0.9349

m6 <- lm(runs ~ new_slug , data = mlb11)
summary(m6) # R: 0.8969

m7 <- lm(runs ~ new_onbase , data = mlb11)
summary(m7) # R:  0.8491
